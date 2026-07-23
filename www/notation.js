/* Required Notice: Copyright Abhik Roy
   PolyForm Noncommercial License 1.0.0. See LICENSE.md.

   notation.js
   The design notation from Shadish, Cook, and Campbell, rendered as SVG. Each row
   is a group, time runs left to right, O marks an observation, X marks the
   treatment, and NR marks that the groups were not formed by random assignment.
   A dashed rule between the rows carries the same meaning it does in the book:
   the two groups are nonequivalent. This is the honest picture of what each
   design in the app actually is, so it sits at the top of the results and
   inside the design picker rather than a decorative chart. */

(function () {
  var NS = 'http://www.w3.org/2000/svg';
  function e(tag, a, txt) {
    var n = document.createElementNS(NS, tag);
    for (var k in a) if (a.hasOwnProperty(k)) n.setAttribute(k, a[k]);
    if (txt !== undefined) n.textContent = txt;
    return n;
  }
  function tok(name) {
    return getComputedStyle(document.body).getPropertyValue(name).trim() || '#888';
  }

  // The instrumental variables path diagram: Z to D to Y across the middle, with
  // the unmeasured confounder U reaching down to both D and Y along dashed
  // arrows. Nodes are lettered circles so the structure reads at a glance.
  function ivDiagram(compact) {
    var W = 300, H = compact ? 96 : 120;
    var svg = e('svg', { viewBox: '0 0 ' + W + ' ' + H, width: '100%',
      height: H, role: 'img' });
    svg.appendChild(e('title', {}, 'Instrument Z moves program D, which moves outcome Y; confounder U pushes on D and Y'));
    var midY = H - (compact ? 28 : 36);
    var xZ = 34, xD = 150, xY = 266, yU = compact ? 20 : 26;

    function node(cx, cy, label, hue) {
      svg.appendChild(e('circle', { cx: cx, cy: cy, r: 15,
        fill: tok('--sf2'), stroke: hue, 'stroke-width': 2 }));
      svg.appendChild(e('text', { x: cx, y: cy + 5, 'text-anchor': 'middle',
        fill: hue, 'font-size': 15, 'font-weight': 700,
        'font-family': tok('--ff-mono') }, label));
    }
    function arrow(x1, y1, x2, y2, hue, dashed) {
      var a = { x1: x1, y1: y1, x2: x2, y2: y2, stroke: hue, 'stroke-width': 1.8 };
      if (dashed) a['stroke-dasharray'] = '4 4';
      svg.appendChild(e('line', a));
      // A small arrowhead at the destination.
      var ang = Math.atan2(y2 - y1, x2 - x1);
      var hx = x2 - 15 * Math.cos(ang), hy = y2 - 15 * Math.sin(ang);
      svg.appendChild(e('polygon', {
        points: [hx + 5 * Math.cos(ang) + ',' + (hy + 5 * Math.sin(ang)),
          (hx - 4 * Math.sin(ang)) + ',' + (hy + 4 * Math.cos(ang)),
          (hx + 4 * Math.sin(ang)) + ',' + (hy - 4 * Math.cos(ang))].join(' '),
        fill: hue }));
    }
    // Z to D to Y along the spine.
    arrow(xZ + 15, midY, xD - 15, midY, tok('--treat'));
    arrow(xD + 15, midY, xY - 15, midY, tok('--treat'));
    // U down to D and Y, dashed because it is unmeasured.
    arrow(xD, yU + 15, xD, midY - 15, tok('--m'), true);
    arrow(xD + 8, yU + 12, xY - 6, midY - 14, tok('--m'), true);
    node(xZ, midY, 'Z', tok('--accent'));
    node(xD, midY, 'D', tok('--treat'));
    node(xY, midY, 'Y', tok('--treat'));
    node(xD + ((xY - xD) / 2), yU, 'U', tok('--m'));

    if (!compact) {
      svg.appendChild(e('text', { x: xZ, y: H - 6, fill: tok('--m'),
        'font-size': 10, 'font-family': tok('--ff-body') }, 'Z instrument'));
      svg.appendChild(e('text', { x: xD - 10, y: H - 6, fill: tok('--m'),
        'font-size': 10, 'font-family': tok('--ff-body') }, 'D program'));
      svg.appendChild(e('text', { x: xY - 12, y: H - 6, fill: tok('--m'),
        'font-size': 10, 'font-family': tok('--ff-body') }, 'Y outcome'));
    }
    return svg;
  }

  // Build one design diagram. spec.rows is an array of glyph arrays, where each
  // glyph is a string like O, X, or a blank. treatColRow marks which row and
  // column carry the X so it can be tinted as the treated group.
  function diagram(kind, opts) {
    opts = opts || {};
    var compact = opts.compact;

    // Instrumental variables is not an O and X row design; its picture is a
    // path diagram. The instrument Z moves the program D, D moves the outcome
    // Y, and an unmeasured confounder U pushes on both D and Y along dashed
    // paths. The instrument earns its place by having no arrow of its own into Y.
    if (kind === 'iv') return ivDiagram(compact);

    var rows, caption;
    if (kind === 'matching') {
      // Untreated control group design with pretest and posttest.
      rows = [
        { tag: 'NR', cells: ['O', 'X', 'O'], treated: true },
        { tag: 'NR', cells: ['O', '', 'O'], treated: false }
      ];
      caption = 'Untreated comparison group, pretest and posttest';
    } else if (kind === 'did') {
      // Comparison group interrupted time series, the many period form.
      rows = [
        { tag: 'NR', cells: ['O', 'O', 'O', 'X', 'O', 'O', 'O'], treated: true },
        { tag: 'NR', cells: ['O', 'O', 'O', '', 'O', 'O', 'O'], treated: false }
      ];
      caption = 'Comparison group across repeated observations';
    } else if (kind === 'its') {
      // Interrupted time series: one series, the interruption partway through,
      // and no comparison row, so no dashed rule.
      rows = [
        { tag: '', cells: ['O', 'O', 'O', 'O', 'X', 'O', 'O', 'O', 'O'], treated: true }
      ];
      caption = 'One series, interrupted at a known time';
    } else if (kind === 'rdd') {
      // Regression discontinuity: assignment by a cutoff. C marks the cutoff
      // based assignment; the treated row sits above the threshold.
      rows = [
        { tag: 'C', cells: ['O', 'X', 'O'], treated: true },
        { tag: 'C', cells: ['O', '', 'O'], treated: false }
      ];
      caption = 'Assignment by a cutoff on a running variable';
    } else if (kind === 'oneshot') {
      rows = [{ tag: '', cells: ['X', 'O'], treated: true }];
      caption = 'One group, treatment then one observation';
    } else if (kind === 'prepost') {
      rows = [{ tag: '', cells: ['O', 'X', 'O'], treated: true }];
      caption = 'One group, observed before and after';
    } else {
      // Two nonequivalent groups, each observed once after, no pretest.
      rows = [
        { tag: '', cells: ['X', 'O'], treated: true },
        { tag: '', cells: ['', 'O'], treated: false }
      ];
      caption = 'Two groups, posttest only, no pretest';
    }

    var cols = rows[0].cells.length;
    var cw = compact ? 26 : 34;         // column width
    var rh = compact ? 34 : 42;         // row height
    var padL = compact ? 30 : 40;       // room for the NR tag
    var padT = compact ? 10 : 16;
    var W = padL + cols * cw + 14;
    var H = padT + rows.length * rh + (compact ? 8 : 26);

    var svg = e('svg', { viewBox: '0 0 ' + W + ' ' + H, width: '100%',
      height: H, role: 'img' });
    svg.appendChild(e('title', {}, 'Design notation: ' + caption));

    rows.forEach(function (row, ri) {
      var cy = padT + ri * rh + rh / 2;
      // The NR tag.
      svg.appendChild(e('text', {
        x: 4, y: cy + 4, fill: tok('--m'),
        'font-size': compact ? 10 : 12, 'font-family': tok('--ff-mono')
      }, row.tag));
      row.cells.forEach(function (c, ci) {
        if (!c) return;
        var cx = padL + ci * cw + cw / 2;
        var color = row.treated ? tok('--treat') : tok('--compare');
        if (c === 'X') {
          // X is the treatment, set heavier and in the treated hue.
          svg.appendChild(e('text', {
            x: cx, y: cy + (compact ? 5 : 6), 'text-anchor': 'middle',
            fill: tok('--treat'), 'font-weight': 700,
            'font-size': compact ? 16 : 20, 'font-family': tok('--ff-mono')
          }, 'X'));
        } else {
          svg.appendChild(e('text', {
            x: cx, y: cy + (compact ? 5 : 6), 'text-anchor': 'middle',
            fill: color, 'font-size': compact ? 15 : 19,
            'font-family': tok('--ff-mono')
          }, 'O'));
        }
      });
    });

    // The dashed rule between the two rows: nonequivalent groups. A single
    // series has no second row and so no rule, which is itself the point: an
    // interrupted time series has no comparison group to separate from.
    if (rows.length > 1) {
      var midY = padT + rh;
      svg.appendChild(e('line', {
        x1: padL - 6, y1: midY, x2: padL + cols * cw, y2: midY,
        stroke: tok('--br'), 'stroke-width': 1.5, 'stroke-dasharray': '4 4'
      }));
    }

    if (!compact) {
      svg.appendChild(e('text', {
        x: padL, y: H - 8, fill: tok('--m'),
        'font-size': 11, 'font-family': tok('--ff-body')
      }, caption));
    }
    return svg;
  }

  // Public surface. paint fills only hosts that are still empty, so it can be
  // called as often as we like, including from a mutation observer, without
  // ever looping: once a host holds an svg it is skipped. repaint clears first,
  // for when the palette changed and the glyph colors must be redrawn.
  function paint() {
    document.querySelectorAll('[data-notation]').forEach(function (host) {
      if (host.querySelector('svg')) return;
      host.appendChild(diagram(host.getAttribute('data-notation'),
        { compact: host.hasAttribute('data-compact') }));
    });
  }
  function repaint() {
    document.querySelectorAll('[data-notation]').forEach(function (host) {
      host.innerHTML = '';
      host.appendChild(diagram(host.getAttribute('data-notation'),
        { compact: host.hasAttribute('data-compact') }));
    });
  }

  window.CausalityNotation = { diagram: diagram, paint: paint, repaint: repaint };

  // A results panel mounts its notation host after the server responds, so a
  // one time DOMContentLoaded paint is not enough. A childList observer catches
  // every later insertion; because paint only touches empty hosts, the observer
  // it triggers finds nothing to do on the second pass and settles at once.
  function start() {
    paint();
    var mo = new MutationObserver(function () { paint(); });
    mo.observe(document.body, { childList: true, subtree: true });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
