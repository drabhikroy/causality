// test_dom.js
// The browser code, tested where it actually lives. The tour is defined inline
// inside app.R rather than in a separate file, because a separate file that
// fails to evaluate leaves the button dead with no trace. This test pulls that
// inline block straight out of app.R and drives it, so what is verified is the
// same code the Tour button calls. The notation builder is tested alongside it.

const { JSDOM } = require('jsdom');
const fs = require('fs');

let fails = 0;
function check(label, cond) {
  if (cond) { console.log('ok    ' + label); }
  else { console.log('FAIL  ' + label); fails++; }
}

// Pull the inline bootstrap out of app.R, between the assignment and the lone
// closing quote, which is how the R source delimits it.
const appSrc = fs.readFileSync('app.R', 'utf8').split('\n');
const start = appSrc.findIndex(l => l.startsWith('bootstrap_js <- "'));
let end = -1;
for (let i = start + 1; i < appSrc.length; i++) {
  if (appSrc[i] === '"') { end = i; break; }
}
check('inline bootstrap block found in app.R', start >= 0 && end > start);
const bootstrapJs = appSrc.slice(start + 1, end).join('\n');

const dom = new JSDOM('<!DOCTYPE html><body></body>', { runScripts: 'outside-only' });
const window = dom.window;
window.matchMedia = () => ({ matches: false });
window.eval(bootstrapJs);

// ---- The tour: the object must exist and the overlay must actually mount. ----
check('Causality global is defined', typeof window.Causality === 'object');
check('openTour is callable', typeof window.Causality.openTour === 'function');

window.Causality.openTour();
check('tour overlay mounted', !!window.document.querySelector('.wt-overlay'));
check('tour card present', !!window.document.querySelector('.wt-card'));
check('slide title has text',
  window.document.querySelector('.wt-title').textContent.length > 5);
check('slide body has text',
  window.document.querySelector('.wt-body').textContent.length > 30);
const dots = window.document.querySelectorAll('.wt-dot').length;
check('one dot per slide', dots === 7);
check('step counter reads first slide',
  /Step 1 of 7/.test(window.document.querySelector('.wt-step').textContent));

// Walk to the end; the final action closes the tour.
for (let i = 0; i < dots - 1; i++) {
  window.document.querySelector('.wt-btn.primary').click();
}
check('final button reads Start',
  window.document.querySelector('.wt-btn.primary').textContent === 'Start');
window.document.querySelector('.wt-btn.primary').click();
check('tour closes on Start', !window.document.querySelector('.wt-overlay'));
window.Causality.openTour();
check('tour reopens after closing', !!window.document.querySelector('.wt-overlay'));

// ---- Theme and palette act on the body, with no server round trip. ----
window.Causality.openTour();
const themeBtn = window.document.createElement('button');
themeBtn.innerHTML = '<span class="hbtn-label">Light</span>';
window.Causality.toggleTheme(themeBtn);
check('theme toggle adds light mode',
  window.document.body.classList.contains('light-mode'));
check('theme button relabels to Dark',
  themeBtn.querySelector('.hbtn-label').textContent === 'Dark');
window.Causality.toggleTheme(themeBtn);
check('theme toggle returns to dark',
  !window.document.body.classList.contains('light-mode'));

const group = window.document.createElement('div');
group.innerHTML = '<button data-palette="none"></button><button data-palette="cb-mono"></button>';
window.document.body.appendChild(group);
window.Causality.setPalette('cb-mono', group.children[1]);
check('palette applies its class', window.document.body.classList.contains('cb-mono'));
check('palette marks the pressed button',
  group.children[1].getAttribute('aria-pressed') === 'true');
window.Causality.setPalette('none', group.children[0]);
check('palette clears back to standard',
  !window.document.body.classList.contains('cb-mono'));

// ---- The notation builder, one diagram per design in the chooser. ----
const notationJs = fs.readFileSync('www/notation.js', 'utf8');
window.eval(notationJs);
check('notation builder exposed', typeof window.CausalityNotation === 'object');

const kinds = ['oneshot', 'prepost', 'posttest_only', 'matching', 'did', 'its', 'rdd'];
kinds.forEach(k => {
  const svg = window.CausalityNotation.diagram(k, {});
  check('notation renders for ' + k, !!svg && svg.querySelectorAll('text').length >= 2);
});
const ivSvg = window.CausalityNotation.diagram('iv', {});
check('iv notation renders a path diagram',
  ivSvg && ivSvg.querySelectorAll('circle').length === 4);

// One group designs carry no comparison row, so they must have no dashed rule.
const oneGroup = window.CausalityNotation.diagram('prepost', {});
check('one group design has no nonequivalence rule',
  oneGroup.querySelectorAll('line[stroke-dasharray]').length === 0);
const twoGroup = window.CausalityNotation.diagram('matching', {});
check('two group design has the nonequivalence rule',
  twoGroup.querySelectorAll('line[stroke-dasharray]').length === 1);

// Skip must be present on every slide but the last, where the primary action
// already closes the tour, and it must leave immediately from wherever it sits.
window.Causality.openTour();
const skipBtn = window.document.querySelector('.wt-btn.quiet');
check('skip button offered on the first slide',
  !!skipBtn && skipBtn.textContent === 'Skip');
skipBtn.click();
check('skip closes the tour at once', !window.document.querySelector('.wt-overlay'));

window.Causality.openTour();
window.document.querySelector('.wt-btn.primary').click();
const midSkip = window.document.querySelector('.wt-btn.quiet');
check('skip still offered partway through', !!midSkip);
midSkip.click();
check('skip closes from a middle slide', !window.document.querySelector('.wt-overlay'));

window.Causality.openTour();
for (let i = 0; i < dots - 1; i++) {
  window.document.querySelector('.wt-btn.primary').click();
}
check('skip is withdrawn on the final slide',
  !window.document.querySelector('.wt-btn.quiet'));
window.document.querySelector('.wt-btn.primary').click();

console.log('\nDOM: ' + fails + ' failures');
process.exit(fails > 0 ? 1 : 0);
