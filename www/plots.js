/* Required Notice: Copyright Abhik Roy
   PolyForm Noncommercial License 1.0.0. See LICENSE.md.

   plots.js
   The figures are built by hand as SVG rather than through a chart library so
   the accessibility choices hold in every theme and palette. Four plots live
   here: an overlap plot and a Love balance plot for the matched design, and a
   trend plot and an event study for the before and after design. Colors are
   read from CSS custom properties at draw time, so a palette switch redraws
   with the right values automatically. */

(function () {
  var current = null;   // last payload, kept so a palette change can redraw

  function tok(name) {
    return getComputedStyle(document.body).getPropertyValue(name).trim() || '#888';
  }
  function el(tag, attrs, txt) {
    var e = document.createElementNS('http://www.w3.org/2000/svg', tag);
    for (var k in attrs) if (attrs.hasOwnProperty(k)) e.setAttribute(k, attrs[k]);
    if (txt !== undefined) e.textContent = txt;
    return e;
  }
  function frame(hostId, H) {
    var host = document.getElementById(hostId);
    if (!host) return null;
    host.innerHTML = '';
    var W = host.clientWidth || 640;
    var svg = el('svg', { viewBox: '0 0 ' + W + ' ' + H, width: '100%',
      height: H, role: 'img' });
    host.appendChild(svg);
    return { svg: svg, W: W, H: H };
  }

  // ---- Overlap plot: two propensity score histograms facing each other. ----
  // Two histograms share a baseline and grow in opposite directions, so the
  // eye reads overlap as the region where both fill in rather than by comparing
  // two separate charts. The shaded band marks common support, the only place
  // a match can form.
  function drawOverlap(data) {
    var f = frame('plot-overlap', 240);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var tHist = data.hist.treated, cHist = data.hist.control;
    var pad = { l: 46, r: 16, t: 16, b: 40 };
    var plotW = W - pad.l - pad.r;
    var mid = pad.t + (H - pad.t - pad.b) / 2;
    var maxCount = Math.max.apply(null, tHist.concat(cHist)) || 1;
    var barW = plotW / tHist.length;
    var half = mid - pad.t;

    var s0 = data.support[0], s1 = data.support[1];
    svg.appendChild(el('rect', { x: pad.l + s0 * plotW, y: pad.t,
      width: (s1 - s0) * plotW, height: H - pad.t - pad.b,
      fill: tok('--sf2'), opacity: 0.7 }));

    function bars(hist, up, color) {
      hist.forEach(function (c, i) {
        var h = (c / maxCount) * half * 0.92;
        var x = pad.l + i * barW;
        var y = up ? (mid - h) : mid;
        svg.appendChild(el('rect', { x: x + 1, y: y, width: Math.max(barW - 2, 1),
          height: h, fill: color, rx: 2, stroke: tok('--bg'), 'stroke-width': 0.5 }));
      });
    }
    bars(tHist, true, tok('--treat'));
    bars(cHist, false, tok('--compare'));

    svg.appendChild(el('line', { x1: pad.l, y1: mid, x2: W - pad.r, y2: mid,
      stroke: tok('--br'), 'stroke-width': 1 }));
    svg.appendChild(el('text', { x: pad.l + 2, y: pad.t + 12,
      fill: tok('--treat'), 'font-size': 11, 'font-family': tok('--ff-mono') },
      'Served group'));
    svg.appendChild(el('text', { x: pad.l + 2, y: H - pad.b + 6,
      fill: tok('--compare'), 'font-size': 11, 'font-family': tok('--ff-mono') },
      'Comparison'));
    svg.appendChild(el('text', { x: W / 2, y: H - 8, fill: tok('--m'),
      'font-size': 11, 'text-anchor': 'middle', 'font-family': tok('--ff-mono') },
      'Estimated chance of receiving the program'));
  }

  // ---- Love plot: standardized differences before and after matching. ----
  // A fixed axis of plus or minus 0.6 keeps the plot comparable across runs, so
  // a caliper change moves the markers against a stable ruler rather than a
  // rescaled one that would hide the movement.
  function drawLove(data) {
    var rows = data.balance;
    var f = frame('plot-love', 40 + rows.length * 34);
    if (!f) return;
    var svg = f.svg, W = f.W;
    var pad = { l: 120, r: 20, t: 14 };
    var plotW = W - pad.l - pad.r;
    var maxAbs = 0.6;
    function X(v) { return pad.l + (v / maxAbs) * plotW; }

    [-0.1, 0.1].forEach(function (t) {
      svg.appendChild(el('line', { x1: X(t), y1: pad.t, x2: X(t),
        y2: pad.t + rows.length * 34, stroke: tok('--br'),
        'stroke-width': 1, 'stroke-dasharray': '3 3' }));
    });
    svg.appendChild(el('line', { x1: X(0), y1: pad.t, x2: X(0),
      y2: pad.t + rows.length * 34, stroke: tok('--br'), 'stroke-width': 1 }));

    rows.forEach(function (r, i) {
      var y = pad.t + i * 34 + 17;
      svg.appendChild(el('text', { x: pad.l - 10, y: y + 4, 'text-anchor': 'end',
        fill: tok('--t'), 'font-size': 12, 'font-family': tok('--ff-body') },
        r.covariate));
      var xb = X(Math.max(-maxAbs, Math.min(maxAbs, r.before)));
      var xa = X(Math.max(-maxAbs, Math.min(maxAbs, r.after)));
      svg.appendChild(el('line', { x1: xb, y1: y, x2: xa, y2: y,
        stroke: tok('--m'), 'stroke-width': 1.5 }));
      svg.appendChild(el('circle', { cx: xb, cy: y, r: 5, fill: 'none',
        stroke: tok('--compare'), 'stroke-width': 2 }));
      var afterOk = Math.abs(r.after) < 0.1;
      svg.appendChild(el('circle', { cx: xa, cy: y, r: 5,
        fill: afterOk ? tok('--good') : tok('--warn'),
        stroke: tok('--bg'), 'stroke-width': 1 }));
    });
    var yb = pad.t + rows.length * 34 + 14;
    svg.appendChild(el('text', { x: X(0), y: yb, 'text-anchor': 'middle',
      fill: tok('--m'), 'font-size': 10, 'font-family': tok('--ff-mono') }, '0'));
    svg.appendChild(el('text', { x: X(0.1) + 2, y: yb, fill: tok('--m'),
      'font-size': 10, 'font-family': tok('--ff-mono') }, '0.1 line'));
  }

  // ---- Trend plot: mean outcome per period for each group. ----
  // The y range is padded by fifteen percent so the lines never touch the frame
  // edge, which otherwise reads as clipping. Group identity is carried by marker
  // shape as well as color so the figure survives a monochrome palette.
  function drawTrend(data) {
    var f = frame('plot-trend', 250);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var s = data.series;
    var pad = { l: 48, r: 18, t: 18, b: 30 };
    var xs = s.map(function (d) { return d.t; });
    var ys = s.map(function (d) { return d.m; });
    var xmin = Math.min.apply(null, xs), xmax = Math.max.apply(null, xs);
    var ymin = Math.min.apply(null, ys), ymax = Math.max.apply(null, ys);
    var yp = (ymax - ymin) * 0.15 || 1; ymin -= yp; ymax += yp;
    function X(t) { return pad.l + (t - xmin) / (xmax - xmin) * (W - pad.l - pad.r); }
    function Y(v) { return H - pad.b - (v - ymin) / (ymax - ymin) * (H - pad.t - pad.b); }

    var ax = X(data.adopt);
    svg.appendChild(el('line', { x1: ax, y1: pad.t, x2: ax, y2: H - pad.b,
      stroke: tok('--accent'), 'stroke-width': 1.5, 'stroke-dasharray': '5 4' }));
    svg.appendChild(el('text', { x: ax + 4, y: pad.t + 10, fill: tok('--accent'),
      'font-size': 10, 'font-family': tok('--ff-mono') }, 'X begins'));

    function series(gval, color, shape) {
      var pts = s.filter(function (d) { return d.g === gval; })
                 .sort(function (a, b) { return a.t - b.t; });
      var path = pts.map(function (d, i) {
        return (i ? 'L' : 'M') + X(d.t) + ' ' + Y(d.m); }).join(' ');
      svg.appendChild(el('path', { d: path, fill: 'none', stroke: color,
        'stroke-width': 2 }));
      pts.forEach(function (d) {
        if (shape === 'circle')
          svg.appendChild(el('circle', { cx: X(d.t), cy: Y(d.m), r: 3.5,
            fill: color, stroke: tok('--bg'), 'stroke-width': 1 }));
        else
          svg.appendChild(el('rect', { x: X(d.t) - 3.5, y: Y(d.m) - 3.5,
            width: 7, height: 7, fill: 'none', stroke: color, 'stroke-width': 2 }));
      });
    }
    series(1, tok('--treat'), 'circle');
    series(0, tok('--compare'), 'square');
    svg.appendChild(el('line', { x1: pad.l, y1: H - pad.b, x2: W - pad.r,
      y2: H - pad.b, stroke: tok('--br'), 'stroke-width': 1 }));
    svg.appendChild(el('line', { x1: pad.l, y1: pad.t, x2: pad.l, y2: H - pad.b,
      stroke: tok('--br'), 'stroke-width': 1 }));
  }

  // ---- Event study: gap between groups per period, against the pre-adoption
  //      period. Flat before, a step after, is the parallel paths story. ----
  // The vertical range is symmetric around zero so a rise and a fall of equal
  // size look equal, and the pre-adoption points are hollow to separate the
  // assumption check from the effect it licenses.
  function drawEvent(data) {
    var f = frame('plot-event', 220);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var ev = data.event;
    var pad = { l: 48, r: 16, t: 18, b: 34 };
    var ps = ev.map(function (d) { return d.period; });
    var rs = ev.map(function (d) { return d.rel; });
    var pmin = Math.min.apply(null, ps), pmax = Math.max.apply(null, ps);
    var rmax = Math.max(Math.abs(Math.min.apply(null, rs)),
                        Math.abs(Math.max.apply(null, rs))) * 1.2 || 1;
    function X(p) { return pad.l + (p - pmin) / (pmax - pmin) * (W - pad.l - pad.r); }
    function Y(v) { return pad.t + (rmax - v) / (2 * rmax) * (H - pad.t - pad.b); }

    svg.appendChild(el('line', { x1: pad.l, y1: Y(0), x2: W - pad.r, y2: Y(0),
      stroke: tok('--br'), 'stroke-width': 1 }));
    svg.appendChild(el('line', { x1: X(0), y1: pad.t, x2: X(0), y2: H - pad.b,
      stroke: tok('--accent'), 'stroke-width': 1.5, 'stroke-dasharray': '5 4' }));

    var path = ev.map(function (d, i) {
      return (i ? 'L' : 'M') + X(d.period) + ' ' + Y(d.rel); }).join(' ');
    svg.appendChild(el('path', { d: path, fill: 'none', stroke: tok('--treat'),
      'stroke-width': 2 }));
    ev.forEach(function (d) {
      var pre = d.period < 0;
      svg.appendChild(el('circle', { cx: X(d.period), cy: Y(d.rel), r: 4,
        fill: pre ? tok('--compare') : tok('--treat'),
        stroke: tok('--bg'), 'stroke-width': 1 }));
    });
    svg.appendChild(el('text', { x: W / 2, y: H - 8, fill: tok('--m'),
      'font-size': 11, 'text-anchor': 'middle', 'font-family': tok('--ff-mono') },
      'Periods relative to adoption'));
  }

  // ---- Interrupted time series: the series, the fitted segments, and the
  //      pre trend carried forward as a dashed counterfactual. The distance
  //      between the series and that dashed line after the interruption is the
  //      effect the design claims. ----
  function drawITS(data) {
    var f = frame('plot-its', 260);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var s = data.series;
    var pad = { l: 48, r: 18, t: 18, b: 34 };
    var xs = s.map(function (d) { return d.t; });
    var vals = s.map(function (d) { return d.y; })
      .concat(s.map(function (d) { return d.cf; }));
    var xmin = Math.min.apply(null, xs), xmax = Math.max.apply(null, xs);
    var ymin = Math.min.apply(null, vals), ymax = Math.max.apply(null, vals);
    var yp = (ymax - ymin) * 0.12 || 1; ymin -= yp; ymax += yp;
    function X(t) { return pad.l + (t - xmin) / (xmax - xmin) * (W - pad.l - pad.r); }
    function Y(v) { return H - pad.b - (v - ymin) / (ymax - ymin) * (H - pad.t - pad.b); }

    var iv = data.intervention;
    svg.appendChild(el('line', { x1: X(iv), y1: pad.t, x2: X(iv), y2: H - pad.b,
      stroke: tok('--accent'), 'stroke-width': 1.5, 'stroke-dasharray': '5 4' }));
    svg.appendChild(el('text', { x: X(iv) + 4, y: pad.t + 10, fill: tok('--accent'),
      'font-size': 10, 'font-family': tok('--ff-mono') }, 'X begins'));

    // Counterfactual: the pre trend carried across the whole window.
    var cfPath = s.map(function (d, i) {
      return (i ? 'L' : 'M') + X(d.t) + ' ' + Y(d.cf); }).join(' ');
    svg.appendChild(el('path', { d: cfPath, fill: 'none', stroke: tok('--compare'),
      'stroke-width': 1.5, 'stroke-dasharray': '4 4' }));

    // Fitted segments, split at the interruption so the break shows.
    function seg(pre) {
      var pts = s.filter(function (d) { return pre ? d.post === 0 : d.post === 1; });
      if (!pts.length) return;
      var p = pts.map(function (d, i) {
        return (i ? 'L' : 'M') + X(d.t) + ' ' + Y(d.fit); }).join(' ');
      svg.appendChild(el('path', { d: p, fill: 'none', stroke: tok('--treat'),
        'stroke-width': 2 }));
    }
    seg(true); seg(false);

    // The observed points, small, so the fit and counterfactual lead the eye.
    s.forEach(function (d) {
      svg.appendChild(el('circle', { cx: X(d.t), cy: Y(d.y), r: 2.4,
        fill: tok('--m') }));
    });

    svg.appendChild(el('text', { x: pad.l, y: H - 8, fill: tok('--compare'),
      'font-size': 10, 'font-family': tok('--ff-mono') },
      'dashed line: earlier trend carried forward'));
  }

  // ---- Regression discontinuity: binned averages of the outcome against the
  //      running variable, with a fitted line on each side of the cutoff. The
  //      vertical gap at the cutoff is the estimate. ----
  function drawRDD(data) {
    var f = frame('plot-rdd', 260);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var b = data.binned, lo = data.line_lo, hi = data.line_hi;
    var pad = { l: 48, r: 18, t: 18, b: 36 };
    var allx = b.map(function (d) { return d.x; })
      .concat(lo.map(function (d) { return d.x; }), hi.map(function (d) { return d.x; }));
    var ally = b.map(function (d) { return d.y; })
      .concat(lo.map(function (d) { return d.y; }), hi.map(function (d) { return d.y; }));
    var xmin = Math.min.apply(null, allx), xmax = Math.max.apply(null, allx);
    var ymin = Math.min.apply(null, ally), ymax = Math.max.apply(null, ally);
    var yp = (ymax - ymin) * 0.12 || 1; ymin -= yp; ymax += yp;
    function X(v) { return pad.l + (v - xmin) / (xmax - xmin) * (W - pad.l - pad.r); }
    function Y(v) { return H - pad.b - (v - ymin) / (ymax - ymin) * (H - pad.t - pad.b); }

    // The cutoff sits at zero in centered coordinates.
    svg.appendChild(el('line', { x1: X(0), y1: pad.t, x2: X(0), y2: H - pad.b,
      stroke: tok('--accent'), 'stroke-width': 1.5, 'stroke-dasharray': '5 4' }));
    svg.appendChild(el('text', { x: X(0) + 4, y: pad.t + 10, fill: tok('--accent'),
      'font-size': 10, 'font-family': tok('--ff-mono') }, 'cutoff'));

    // Binned points, colored by side so the split reads without relying on
    // position alone.
    b.forEach(function (d) {
      svg.appendChild(el('circle', { cx: X(d.x), cy: Y(d.y), r: 3,
        fill: d.x >= 0 ? tok('--treat') : tok('--compare'),
        stroke: tok('--bg'), 'stroke-width': 0.5 }));
    });

    function fitLine(pts, color) {
      var p = pts.map(function (d, i) {
        return (i ? 'L' : 'M') + X(d.x) + ' ' + Y(d.y); }).join(' ');
      svg.appendChild(el('path', { d: p, fill: 'none', stroke: color,
        'stroke-width': 2.5 }));
    }
    fitLine(lo, tok('--compare'));
    fitLine(hi, tok('--treat'));

    svg.appendChild(el('text', { x: W / 2, y: H - 8, fill: tok('--m'),
      'font-size': 11, 'text-anchor': 'middle', 'font-family': tok('--ff-mono') },
      'Running variable, centered at the cutoff'));
  }

  // ---- Instrumental variables: the first stage. Program take up against the
  //      instrument, which is the variation the estimate is built on. A visibly
  //      sloped cloud is a strong instrument; a flat one is the weak instrument
  //      the F statistic warns about. ----
  function drawIV(data) {
    var f = frame('plot-iv', 250);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var b = data.by_z;
    var pad = { l: 52, r: 18, t: 18, b: 40 };
    var xs = b.map(function (d) { return d.z; });
    var ds = b.map(function (d) { return d.d; });
    var xmin = Math.min.apply(null, xs), xmax = Math.max.apply(null, xs);
    var ymin = Math.min.apply(null, ds), ymax = Math.max.apply(null, ds);
    var yp = (ymax - ymin) * 0.15 || 0.1; ymin = Math.max(0, ymin - yp); ymax = Math.min(1, ymax + yp);
    function X(v) { return pad.l + (v - xmin) / (xmax - xmin) * (W - pad.l - pad.r); }
    function Y(v) { return H - pad.b - (v - ymin) / (ymax - ymin) * (H - pad.t - pad.b); }

    svg.appendChild(el('line', { x1: pad.l, y1: H - pad.b, x2: W - pad.r,
      y2: H - pad.b, stroke: tok('--br'), 'stroke-width': 1 }));
    svg.appendChild(el('line', { x1: pad.l, y1: pad.t, x2: pad.l, y2: H - pad.b,
      stroke: tok('--br'), 'stroke-width': 1 }));

    // A simple least squares line through the binned take up rates.
    var n = b.length, sx = 0, sy = 0, sxx = 0, sxy = 0;
    b.forEach(function (d) { sx += d.z; sy += d.d; sxx += d.z * d.z; sxy += d.z * d.d; });
    var slope = (n * sxy - sx * sy) / (n * sxx - sx * sx);
    var icept = (sy - slope * sx) / n;
    svg.appendChild(el('line', { x1: X(xmin), y1: Y(icept + slope * xmin),
      x2: X(xmax), y2: Y(icept + slope * xmax),
      stroke: tok('--accent'), 'stroke-width': 2 }));

    b.forEach(function (d) {
      svg.appendChild(el('circle', { cx: X(d.z), cy: Y(d.d), r: 3.5,
        fill: tok('--treat'), stroke: tok('--bg'), 'stroke-width': 0.5 }));
    });

    svg.appendChild(el('text', { x: 8, y: pad.t + 4, fill: tok('--m'),
      'font-size': 11, 'font-family': tok('--ff-mono') }, 'take up'));
    svg.appendChild(el('text', { x: W / 2, y: H - 10, fill: tok('--m'),
      'font-size': 11, 'text-anchor': 'middle', 'font-family': tok('--ff-mono') },
      'The instrument'));
    var lbl = data.weak ? 'first stage F ' + data.first_F.toFixed(1) + ': weak'
                        : 'first stage F ' + data.first_F.toFixed(1) + ': strong';
    svg.appendChild(el('text', { x: W - pad.r, y: pad.t + 4, 'text-anchor': 'end',
      fill: data.weak ? tok('--bad') : tok('--good'),
      'font-size': 11, 'font-family': tok('--ff-mono') }, lbl));
  }

  // ---- The weaker designs get one honest bar chart. There is no adjustment
  //      to show and no counterfactual to draw, so the figure states plainly
  //      what the data hold and no more. ----
  function drawSimple(data) {
    var f = frame('plot-simple', 220);
    if (!f) return;
    var svg = f.svg, W = f.W, H = f.H;
    var bars;
    if (data.kind === 'oneshot') {
      bars = [{ label: 'Group average', v: data.mean, hue: tok('--treat') }];
    } else if (data.kind === 'prepost') {
      bars = [{ label: 'Before', v: data.pre, hue: tok('--compare') },
              { label: 'After', v: data.post, hue: tok('--treat') }];
    } else {
      bars = [{ label: 'Comparison', v: data.control, hue: tok('--compare') },
              { label: 'Program', v: data.treated, hue: tok('--treat') }];
    }
    var pad = { l: 60, r: 20, t: 20, b: 40 };
    var maxv = Math.max.apply(null, bars.map(function (b) { return b.v; })) * 1.15;
    var plotH = H - pad.t - pad.b;
    var slot = (W - pad.l - pad.r) / bars.length;
    var bw = Math.min(90, slot * 0.5);

    svg.appendChild(el('line', { x1: pad.l, y1: H - pad.b, x2: W - pad.r,
      y2: H - pad.b, stroke: tok('--br'), 'stroke-width': 1 }));
    bars.forEach(function (b, i) {
      var h = (b.v / maxv) * plotH;
      var cx = pad.l + slot * i + slot / 2;
      svg.appendChild(el('rect', { x: cx - bw / 2, y: H - pad.b - h,
        width: bw, height: h, fill: b.hue, rx: 3 }));
      svg.appendChild(el('text', { x: cx, y: H - pad.b - h - 6,
        'text-anchor': 'middle', fill: tok('--t'), 'font-size': 12,
        'font-family': tok('--ff-mono') }, b.v.toFixed(1)));
      svg.appendChild(el('text', { x: cx, y: H - pad.b + 16,
        'text-anchor': 'middle', fill: tok('--m'), 'font-size': 11,
        'font-family': tok('--ff-body') }, b.label));
    });
  }

  function hostsReady() {
    if (!current) return false;
    if (current.kind === 'matching') return !!document.getElementById('plot-overlap');
    if (current.kind === 'did') return !!document.getElementById('plot-trend');
    if (current.kind === 'its') return !!document.getElementById('plot-its');
    if (current.kind === 'rdd') return !!document.getElementById('plot-rdd');
    if (current.kind === 'iv') return !!document.getElementById('plot-iv');
    return !!document.getElementById('plot-simple');
  }

  // The results DOM mounts a moment after the plot message arrives, so render
  // waits for its hosts to exist, retrying a few times before giving up rather
  // than drawing into nothing.
  function render(tries) {
    tries = tries || 0;
    if (!current) return;
    if (!hostsReady()) {
      if (tries < 20) setTimeout(function () { render(tries + 1); }, 40);
      return;
    }
    if (current.kind === 'matching') { drawOverlap(current); drawLove(current); }
    else if (current.kind === 'did') { drawTrend(current); drawEvent(current); }
    else if (current.kind === 'its') { drawITS(current); }
    else if (current.kind === 'rdd') { drawRDD(current); }
    else if (current.kind === 'iv') { drawIV(current); }
    else { drawSimple(current); }
  }

  // Redraw handle so a palette or theme change can repaint with new tokens.
  window.CausalityPlots = { redraw: function () { render(0); } };

  // Resilient registration: keep trying until Shiny is present, so load order
  // against shiny.js can never leave the handler unregistered.
  (function register() {
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('causality-plot', function (payload) {
        current = payload;
        render(0);
      });
    } else {
      setTimeout(register, 50);
    }
  })();

  var rt;
  window.addEventListener('resize', function () {
    clearTimeout(rt); rt = setTimeout(function () { render(0); }, 120);
  });
})();
