# Causality: structured comparisons without random assignment.
# Required Notice: Copyright Abhik Roy
# Licensed under the PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# app.R
# Causality. A workbench for reading effects from studies you could not
# randomize. Two quasi-experimental designs ship in it, matched comparison and
# before and after, named and set in the notation of Shadish, Cook, and
# Campbell. The statistics are ordinary and transparent; the only place a
# language model is allowed to help is restating a finished answer in plainer
# words, and even that runs locally and never touches a number.
#
# A note on how the browser code is delivered. Every script is embedded into
# the page rather than linked as a separate file, because a linked file can
# fail to load in some run environments and leave the whole interface inert.
# Inlining removes that failure mode: if the page renders, the behavior is
# present. Button actions are wired as inline handlers for the same reason, so
# nothing depends on a listener attaching at the right moment.

library(shiny)
library(dplyr)
library(readr)
library(jsonlite)

source("R/causal_math.R")
source("R/samples.R")
source("R/interpret.R")
source("R/refute.R")
source("R/export.R")
source("R/modals.R")

app_name <- "Causality"
app_version <- "0.7.1"

# The designs the app can run, in the order they appear in the picker. Each is
# a quasi-experimental design from Shadish, Cook, and Campbell, paired with the
# method that estimates it. The list is the single source the picker, the
# detector, and the run handler all read from.
# The designs the app can run, grouped as Shadish, Cook, and Campbell group them.
# The group names follow the chapter structure of the text: designs without
# control groups, designs with a control group but no pretest, designs with both,
# interrupted time series, and regression discontinuity. Instrumental variables
# sits in its own group, since the text treats it as a separate approach to the
# same problem rather than one of the design families above.
DESIGN_GROUPS <- list(
  list(
    group = "Without a control group",
    note = "One group only. These identify no effect on their own; the app runs them to show what is missing.",
    designs = c("oneshot", "prepost")
  ),
  list(
    group = "Control group, no pretest",
    note = "Two groups compared after the fact. Selection cannot be checked without a pretest.",
    designs = c("posttest_only")
  ),
  list(
    group = "Control group and pretest",
    note = "The workhorse quasi-experiments. A comparison group plus a before measure supports a real estimate.",
    designs = c("matching", "did")
  ),
  list(
    group = "Interrupted time series",
    note = "One or more series with a known interruption point.",
    designs = c("its")
  ),
  list(
    group = "Regression discontinuity",
    note = "Assignment decided by a cutoff on a measured score.",
    designs = c("rdd")
  ),
  list(
    group = "Instrumental variables",
    note = "For take up tangled with the outcome through something unmeasured.",
    designs = c("iv")
  )
)

DESIGNS <- list(
  oneshot  = list(name = "One group posttest only",
                  notation = "X  O",
                  sub = "One group, measured once after. No effect is identifiable.",
                  spec = "onegroup", strength = "none"),
  prepost  = list(name = "One group pretest posttest",
                  notation = "O  X  O",
                  sub = "One group, before and after. Growth and history are not separable.",
                  spec = "beforeafter1", strength = "weak"),
  posttest_only = list(name = "Posttest only, nonequivalent groups",
                  notation = "X  O / _  O",
                  sub = "Two groups, measured once after. Selection is unchecked.",
                  spec = "twogroup", strength = "weak"),
  matching = list(name = "Untreated control group, matched",
                  notation = "O  X  O / O  _  O",
                  sub = "Pair served people with similar others on measured traits.",
                  spec = "training", strength = "strong"),
  did      = list(name = "Untreated control group over time",
                  notation = "O O X O O / O O _ O O",
                  sub = "Two groups tracked across a change.",
                  spec = "policy", strength = "strong"),
  its      = list(name = "Simple interrupted time series",
                  notation = "O O O X O O O",
                  sub = "One series, a change at a known time.",
                  spec = "series", strength = "moderate"),
  rdd      = list(name = "Regression discontinuity",
                  notation = "C  O  X  O",
                  sub = "Assignment by a score cutoff.",
                  spec = "cutoff", strength = "strong"),
  iv       = list(name = "Instrumental variables",
                  notation = "Z to D to Y",
                  sub = "An instrument for tangled take up.",
                  spec = "instrument", strength = "strong")
)

# The stylesheet is carried as a single quoted string, so no apostrophe may
# appear inside it. The file was written with that rule in mind.
app_css <- paste(readLines("www/app.css", warn = FALSE), collapse = "\n")

# Read a browser script from disk so it can be embedded directly in the head.
read_js <- function(name) paste(readLines(file.path("www", name), warn = FALSE),
                                collapse = "\n")

`%||%` <- function(a, b) if (is.null(a)) b else a

# The small interaction layer. It runs the appearance controls and the design
# switch with no server round trip, and it is written here as inline text so it
# is present the moment the page renders.
bootstrap_js <- "
window.Causality = (function () {
  function repaintVisuals() {
    if (window.CausalityNotation) window.CausalityNotation.repaint();
    if (window.CausalityPlots) window.CausalityPlots.redraw();
  }

  // The tour is defined right here rather than in a separate file. Earlier
  // versions relied on an external script to define the tour object, and when
  // that script failed to evaluate the button had nothing to call and appeared
  // dead. Building the slides from plain text and simple shapes, in this same
  // block that the buttons call into, removes that whole class of failure.
  var TOUR = [
    { t: 'What this app is for',
      b: 'Causality answers one kind of question: did a program, a policy, or a change cause a difference in some outcome. Not whether they moved together, but whether one caused the other.' },
    { t: 'Why a plain comparison misleads',
      b: 'People who take a program often differ from people who do not, before the program touches them. Compare the two groups directly and you measure both the program and those differences at once.' },
    { t: 'What a design does',
      b: 'Each study design is a way to stand in for what would have happened otherwise. Matching finds lookalikes. Before and after uses a comparison group over time. A cutoff design uses people either side of a threshold.' },
    { t: 'Two numbers, every time',
      b: 'Every result shows the plain comparison crossed out beside the adjusted estimate. The distance between them is the confounding the design removed, shown rather than asserted.' },
    { t: 'The assumption is always on screen',
      b: 'Each design leans on one assumption hardest. The app tests it and shows the verdict beside the estimate, so a number never appears without the check that licenses it.' },
    { t: 'Everything stays on your machine',
      b: 'Every statistic is computed here. Nothing is sent anywhere. An optional local model can reword a finished result, and even that runs on your own computer.' },
    { t: 'Ready',
      b: 'Pick a design on the left, load a CSV or use a built in sample, then run the analysis. If you load a file, the app suggests a design from its columns and you can change it.' }
  ];
  var tourIdx = 0, tourRoot = null;

  function tourClose() {
    if (tourRoot && tourRoot.parentNode) tourRoot.parentNode.removeChild(tourRoot);
    tourRoot = null;
    document.removeEventListener('keydown', tourKey);
  }
  function tourKey(e) {
    if (e.key === 'Escape') tourClose();
    if (e.key === 'ArrowRight') tourStep(1);
    if (e.key === 'ArrowLeft') tourStep(-1);
  }
  function tourStep(d) {
    tourIdx = Math.min(TOUR.length - 1, Math.max(0, tourIdx + d));
    tourPaint();
  }
  function tourPaint() {
    if (!tourRoot) return;
    var s = TOUR[tourIdx];
    var last = tourIdx === TOUR.length - 1;
    // Built with DOM calls rather than an HTML string, so no nested quoting is
    // needed anywhere and there is nothing for an escape to get wrong.
    tourRoot.innerHTML = '';
    var card = document.createElement('div');
    card.className = 'wt-card';
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-modal', 'true');

    var step = document.createElement('div');
    step.className = 'wt-step';
    step.textContent = 'Step ' + (tourIdx + 1) + ' of ' + TOUR.length;
    card.appendChild(step);

    var h = document.createElement('h2');
    h.className = 'wt-title';
    h.textContent = s.t;
    card.appendChild(h);

    var p = document.createElement('p');
    p.className = 'wt-body';
    p.textContent = s.b;
    card.appendChild(p);

    var foot = document.createElement('div');
    foot.className = 'wt-foot';
    var dots = document.createElement('div');
    dots.className = 'wt-dots';
    for (var i = 0; i < TOUR.length; i++) {
      var dot = document.createElement('span');
      dot.className = i === tourIdx ? 'wt-dot on' : 'wt-dot';
      dots.appendChild(dot);
    }
    foot.appendChild(dots);

    var acts = document.createElement('div');
    acts.className = 'wt-actions';
    // Skip sits to the left of the other actions on every slide but the last,
    // where the primary action already closes the tour. Without it the only
    // ways out were the Escape key and a click on the backdrop, neither of
    // which is visible to someone who has not been told about them.
    if (!last) {
      var skip = document.createElement('button');
      skip.className = 'wt-btn quiet';
      skip.textContent = 'Skip';
      skip.onclick = function () { tourClose(); };
      acts.appendChild(skip);
    }
    if (tourIdx > 0) {
      var back = document.createElement('button');
      back.className = 'wt-btn';
      back.textContent = 'Back';
      back.onclick = function () { tourStep(-1); };
      acts.appendChild(back);
    }
    var fwd = document.createElement('button');
    fwd.className = 'wt-btn primary';
    fwd.textContent = last ? 'Start' : 'Next';
    fwd.onclick = function () { if (last) tourClose(); else tourStep(1); };
    acts.appendChild(fwd);
    foot.appendChild(acts);
    card.appendChild(foot);
    tourRoot.appendChild(card);
  }

  function tourOpen() {
    tourClose();
    tourIdx = 0;
    tourRoot = document.createElement('div');
    tourRoot.className = 'wt-overlay';
    tourRoot.onclick = function (e) { if (e.target === tourRoot) tourClose(); };
    document.body.appendChild(tourRoot);
    document.addEventListener('keydown', tourKey);
    tourPaint();
  }

  return {
    toggleTheme: function (btn) {
      var toLight = !document.body.classList.contains('light-mode');
      document.body.classList.toggle('light-mode', toLight);
      var lbl = btn.querySelector('.hbtn-label');
      if (lbl) lbl.textContent = toLight ? 'Dark' : 'Light';
      repaintVisuals();
    },
    setPalette: function (cls, btn) {
      ['cb-deut', 'cb-trit', 'cb-mono'].forEach(function (c) {
        document.body.classList.remove(c);
      });
      if (cls && cls !== 'none') document.body.classList.add(cls);
      var all = btn.parentNode.querySelectorAll('[data-palette]');
      for (var i = 0; i < all.length; i++) all[i].setAttribute('aria-pressed', 'false');
      btn.setAttribute('aria-pressed', 'true');
      repaintVisuals();
    },
    selectDesign: function (which) {
      var opts = document.querySelectorAll('[data-design]');
      for (var i = 0; i < opts.length; i++) {
        opts[i].setAttribute('aria-pressed',
          String(opts[i].getAttribute('data-design') === which));
      }
      if (window.Shiny && Shiny.setInputValue)
        Shiny.setInputValue('design', which, { priority: 'event' });
      closeChooser();
    },
    openTour: tourOpen,
    openChooser: function () {
      var c = document.getElementById('design-chooser');
      if (c) { c.classList.add('open'); c.setAttribute('aria-hidden', 'false'); }
    },
    closeChooser: closeChooser
  };

  function closeChooser() {
    var c = document.getElementById('design-chooser');
    if (c) { c.classList.remove('open'); c.setAttribute('aria-hidden', 'true'); }
  }
})();

document.addEventListener('DOMContentLoaded', function () {
  document.body.classList.add('dark-default');
  var seen = null;
  try { seen = window.localStorage.getItem('causality-tour-seen'); } catch (e) {}
  if (!seen) {
    setTimeout(function () {
      window.Causality.openTour();
      try { window.localStorage.setItem('causality-tour-seen', '1'); } catch (e) {}
    }, 500);
  }
  // Escape closes the design chooser as well as the tour.
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') window.Causality.closeChooser();
  });
});
"

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

# The interface is one page with a fixed header, a sidebar of controls, and a
# reading stage that fills with results. The design chooser sits outside all
# three so it can cover them when open.
ui <- fluidPage(
  tags$head(
    tags$title("Causality"),
    tags$style(HTML(app_css)),
    # All browser code is embedded, in dependency order: the renderers first,
    # then the interaction layer that calls into them.
    tags$script(HTML(read_js("notation.js"))),
    tags$script(HTML(read_js("plots.js"))),
    tags$script(HTML(bootstrap_js))
  ),

  div(class = "app-header",
    div(class = "brand",
      span(class = "brand-mark", "Causality"),
      span(class = "brand-rule"),
      span(class = "brand-tag", "structured comparisons without random assignment")
    ),
    div(class = "header-spacer"),
    div(class = "header-tools",
      tags$button(class = "textbtn", type = "button",
        onclick = "Causality.openTour()", "Tour"),
      actionButton("show_guide", "Data format", class = "textbtn"),
      actionButton("show_models", "Local models", class = "textbtn")
    ),
    div(class = "header-appearance",
      tags$button(id = "toggle_theme", class = "hbtn", type = "button",
        onclick = "Causality.toggleTheme(this)",
        span(class = "hbtn-label", "Light")),
      div(class = "palette-group", role = "group", `aria-label` = "Color setting",
        tags$button(class = "palette-btn", type = "button", `data-palette` = "none",
          `aria-pressed` = "true", onclick = "Causality.setPalette('none', this)", "Standard"),
        tags$button(class = "palette-btn", type = "button", `data-palette` = "cb-deut",
          `aria-pressed` = "false", onclick = "Causality.setPalette('cb-deut', this)", "Deut/Prot"),
        tags$button(class = "palette-btn", type = "button", `data-palette` = "cb-trit",
          `aria-pressed` = "false", onclick = "Causality.setPalette('cb-trit', this)", "Trit"),
        tags$button(class = "palette-btn", type = "button", `data-palette` = "cb-mono",
          `aria-pressed` = "false", onclick = "Causality.setPalette('cb-mono', this)", "Mono")
      )
    )
  ),

  div(class = "shell",
    div(class = "sidebar",
      div(class = "side-label", "Study design"),
      uiOutput("design_current"),
      uiOutput("detect_note"),

      div(class = "side-label", "Data"),
      div(class = "field",
        tags$label(`for` = "data_source", "Source"),
        # Your own CSV is the default. The built in sample is one selection away
        # for anyone who wants to see the app work against a known answer first.
        tags$select(id = "data_source",
          onchange = "Shiny.setInputValue('data_source', this.value)",
          tags$option(value = "upload", selected = NA, "Your own CSV"),
          tags$option(value = "sample", "Built in sample")
        )
      ),
      uiOutput("source_detail"),
      uiOutput("design_controls"),

      actionButton("run", "Run the analysis", class = "run-btn"),
      div(class = "btn-row",
        actionButton("reset", "Reset", class = "ghost-btn danger")
      ),
      div(class = "sidebar-foot", HTML(paste0(
          app_name, " ", app_version, ". Built with ",
          '<a href="https://www.r-project.org" target="_blank" ',
          'rel="noopener">R</a> and ',
          '<a href="https://shiny.posit.co" target="_blank" ',
          'rel="noopener">Shiny</a>. Copyright Abhik Roy, released under the ',
          '<a href="https://polyformproject.org/licenses/noncommercial/1.0.0" ',
          'target="_blank" rel="noopener">PolyForm Noncommercial ',
          "License 1.0.0</a>.")))
    ),

    div(class = "stage",
      uiOutput("results")
    )
  ),

  # The design chooser. A popup grouped exactly as the text groups the designs,
  # so a reader who knows the book can find a design where they expect it. It
  # lives outside the sidebar so it can sit above everything when open.
  div(id = "design-chooser", class = "chooser", `aria-hidden` = "true",
    div(class = "chooser-panel", role = "dialog", `aria-modal` = "true",
      `aria-label` = "Choose a study design",
      div(class = "chooser-head",
        div(
          h2("Choose a study design"),
          div(class = "chooser-sub",
            "Grouped as in Shadish, Cook, and Campbell. Notation reads left to right in time: O is an observation, X is the treatment.")),
        tags$button(class = "chooser-x", type = "button",
          onclick = "Causality.closeChooser()", `aria-label` = "Close", "Close")),
      div(class = "chooser-body", uiOutput("design_groups"))
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # One place holds everything the session knows: which design is in force,
  # the last result and its reading, the frame that produced it (kept so the
  # placebo test can reuse it without a second read), and any detection note.
  state <- reactiveValues(design = "matching", result = NULL, interp = NULL,
                          detect = NULL)

  observeEvent(input$design, {
    if (input$design %in% names(DESIGNS)) {
      state$design <- input$design
      state$detect <- NULL   # a manual choice clears the detection note
    }
  })

  # The sidebar shows the design in force, with its notation, and a button that
  # opens the grouped chooser. This keeps the sidebar short while leaving every
  # design one click away.
  output$design_current <- renderUI({
    d <- DESIGNS[[state$design]]
    div(class = "current-design",
      div(class = "cd-top",
        span(class = "cd-name", d$name),
        tags$button(class = "cd-change", type = "button",
          onclick = "Causality.openChooser()", "Change")),
      div(class = "cd-sub", d$sub),
      div(class = "notation-host in-card", `data-notation` = state$design,
          `data-compact` = NA))
  })

  # The chooser body, built from the group list so the ordering and the group
  # names come from one place and match the text.
  output$design_groups <- renderUI({
    cur <- state$design
    tagList(lapply(DESIGN_GROUPS, function(g) {
      div(class = "chooser-group",
        div(class = "cg-head",
          span(class = "cg-name", g$group),
          span(class = "cg-note", g$note)),
        div(class = "cg-items",
          lapply(g$designs, function(key) {
            d <- DESIGNS[[key]]
            tags$button(class = paste("design-opt", paste0("strength-", d$strength)),
              type = "button", `data-design` = key,
              `aria-pressed` = tolower(as.character(key == cur)),
              onclick = sprintf("Causality.selectDesign('%s')", key),
              span(class = "d-title", d$name),
              span(class = "d-notation", d$notation),
              span(class = "d-sub", d$sub))
          })))
    }))
  })

  # When a file arrives, guess the design from its columns and preselect it,
  # with a note that names the guess and invites a change. The guess never
  # locks anything: the picker above stays fully live.
  observeEvent(input$file, {
    fp <- input$file
    if (is.null(fp)) return(NULL)
    df <- tryCatch(
      suppressWarnings(readr::read_csv(fp$datapath, show_col_types = FALSE,
                                       n_max = 2000)),
      error = function(e) NULL)
    if (is.null(df) || ncol(df) < 2) return(NULL)
    det <- detect_design(df)
    state$design <- det$design
    state$detect <- det
  })

  output$detect_note <- renderUI({
    det <- state$detect
    if (is.null(det)) return(NULL)
    div(class = paste0("detect-note conf-", det$confidence),
      span(class = "dn-head",
        sprintf("Detected: %s", DESIGNS[[det$design]]$name)),
      span(class = "dn-reason", det$reason),
      span(class = "dn-tip", "Not right? Pick any design above."))
  })

  observeEvent(input$show_models, { showModal(models_modal()) })
  observeEvent(input$show_guide, { showModal(guide_modal(state$design)) })

  # Reset asks first. Clearing a result is cheap to undo only by rerunning,
  # which on a large upload is not cheap at all.
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Reset the analysis",
      div(class = "card-body",
        "This clears the current result and returns to the starting screen. Your data source and design choice stay as they are."),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Keep it"),
        actionButton("reset_confirm", "Reset", class = "ghost-btn danger")
      )
    ))
  })
  observeEvent(input$reset_confirm, {
    state$result <- NULL
    state$interp <- NULL
    removeModal()
  })

  # The sidebar detail is a separate output so it reacts to both the design and
  # the source without rebuilding the whole sidebar on every change.
  output$source_detail <- renderUI({
    src <- input$data_source %||% "upload"
    spec <- design_spec(state$design)
    if (src == "sample") {
      div(class = "field",
        tags$label(spec$label),
        div(class = "hint", spec$blurb))
    } else {
      div(class = "field",
        tags$label(`for` = "file", "CSV file"),
        tags$input(id = "file", type = "file", accept = ".csv",
          onchange = "Shiny.setInputValue('file_name', this.files[0] ? this.files[0].name : '')"),
        div(class = "hint", upload_hint(state$design)),
        div(class = "hint",
          "New to this? Open Data format in the header for the exact columns and an example, or switch Source to the built in sample to see a worked run."))
    }
  })

  # Each design shows only the controls it needs: a caliper for matching, the
  # interruption period for a time series, the cutoff for a discontinuity.
  output$design_controls <- renderUI({
    d <- state$design
    if (d == "matching") {
      tagList(
        div(class = "side-label", "Matching rule"),
        div(class = "control-row",
          tags$label(`for` = "caliper",
            HTML("Caliper width <span class='control-val' id='caliper_val'>0.20</span>")),
          tags$input(id = "caliper", type = "range", min = "0.05", max = "0.5",
            step = "0.05", value = "0.20",
            oninput = paste(
              "document.getElementById('caliper_val').textContent = (+this.value).toFixed(2);",
              "Shiny.setInputValue('caliper', this.value)")),
          div(class = "hint",
            "How close a match must be, in standard deviations of the score. Smaller is stricter."))
      )
    } else if (d == "its") {
      spec <- sample_specs$series
      div(class = "field",
        tags$label(`for` = "intervention", "Interruption period"),
        tags$input(id = "intervention", type = "number", value = spec$intervention,
          onchange = "Shiny.setInputValue('intervention', this.value)"),
        div(class = "hint",
          "The period when the program starts. The sample starts at week twenty seven."))
    } else if (d == "rdd") {
      spec <- sample_specs$cutoff
      div(class = "field",
        tags$label(`for` = "cutoff", "Cutoff value"),
        tags$input(id = "cutoff", type = "number", value = spec$cutoff, step = "any",
          onchange = "Shiny.setInputValue('cutoff', this.value)"),
        div(class = "hint",
          "The score at or above which the program applies. The sample cutoff is sixty."))
    } else NULL
  })

  # The run handler is the one place data enters the analysis. It dispatches to
  # the engine for the chosen design and pushes the plot payload separately from
  # the result so the figures can redraw on a palette change without recomputing.
  observeEvent(input$run, {
    design <- state$design
    df <- load_data(input, design, session)
    if (is.null(df)) return(NULL)

    if (design == "oneshot") {
      spec <- design_spec("oneshot")
      res <- run_oneshot(df, spec$outcome)
      interp <- interpret_oneshot(res)
      session$sendCustomMessage("causality-plot", wire_oneshot(res))
    } else if (design == "prepost") {
      spec <- design_spec("prepost")
      res <- run_prepost(df, spec$pre, spec$post)
      interp <- interpret_prepost(res)
      session$sendCustomMessage("causality-plot", wire_prepost(res))
    } else if (design == "posttest_only") {
      spec <- design_spec("posttest_only")
      res <- run_posttest_only(df, spec$outcome, spec$treat)
      interp <- interpret_posttest_only(res)
      session$sendCustomMessage("causality-plot", wire_posttest_only(res))
    } else if (design == "matching") {
      spec <- resolve_spec(input, df, "matching")
      cal <- as.numeric(input$caliper %||% 0.2)
      res <- run_matching(df, spec$outcome, spec$treat, spec$covariates,
                          caliper_mult = cal)
      interp <- interpret_matching(res)
      session$sendCustomMessage("causality-plot", wire_matching(res))
    } else if (design == "did") {
      spec <- resolve_spec(input, df, "did")
      res <- run_did(df, spec$outcome, spec$unit, spec$time, spec$treat,
                     adopt_time = spec$adopt)
      interp <- interpret_did(res)
      session$sendCustomMessage("causality-plot", wire_did(res))
    } else if (design == "its") {
      spec <- resolve_spec(input, df, "its")
      iv <- suppressWarnings(as.numeric(input$intervention))
      if (is.na(iv)) iv <- spec$intervention
      res <- run_its(df, spec$outcome, spec$time, intervention_time = iv)
      interp <- interpret_its(res)
      session$sendCustomMessage("causality-plot", wire_its(res))
    } else if (design == "rdd") {
      spec <- resolve_spec(input, df, "rdd")
      cut <- suppressWarnings(as.numeric(input$cutoff))
      if (is.na(cut)) cut <- spec$cutoff
      res <- run_rdd(df, spec$outcome, spec$running, cutoff = cut)
      interp <- interpret_rdd(res)
      session$sendCustomMessage("causality-plot", wire_rdd(res))
    } else {
      spec <- resolve_spec(input, df, "iv")
      res <- run_iv(df, spec$outcome, spec$treat, spec$instrument,
                    controls = spec$controls %||% character(0))
      interp <- interpret_iv(res)
      session$sendCustomMessage("causality-plot", wire_iv(res))
    }
    state$result <- res
    state$interp <- interp
    state$rundata <- df
  })

  # The whole reading is one output, so a fresh run replaces it wholesale and
  # no stale panel from a previous design can survive underneath.
  output$results <- renderUI({
    res <- state$result
    if (is.null(res)) return(empty_state(state$design))
    interp <- state$interp
    nc <- naive_contrast(res)

    # Card tone follows the identification check rather than the wording of a
    # card title. An earlier version matched on words in the title, which broke
    # as soon as new designs arrived with different titles, and left the designs
    # that identify nothing looking as though they had passed.
    chk <- identification_check(res)
    card_tone <- if (isFALSE(res$identified)) "verdict-warn"
                 else if (chk$ok) "verdict-good" else "verdict-warn"
    cards <- lapply(interp$cards, function(c) {
      div(class = paste("card", card_tone),
        div(class = "card-title", c$title),
        div(class = "card-body", c$body))
    })

    tagList(
      div(class = "reading",
        div(class = "result-designtag",
          span(class = "rd-label", "Design"),
          span(class = "rd-name", design_full_name(res$design))),
        div(class = "notation-host result-notation", `data-notation` = res$design),
        div(class = "plot-note",
          "O is an observation, X is the program, and the dashed line marks that the two groups were not formed by random assignment."),

        identification_banner(res),

        h1(class = "read-head", interp$headline),
        div(class = "read-sub", results_subhead(res)),

        if (res$design == "oneshot") NULL else div(class = "contrast-row",
          div(class = "contrast-cell is-naive",
            div(class = "cc-label", "Plain comparison"),
            div(class = "cc-num", fmt_signed(nc$value))),
          div(class = "contrast-arrow", HTML("&rarr;")),
          div(class = "contrast-cell is-honest",
            div(class = "cc-label", "Adjusted estimate"),
            div(class = "cc-num", fmt_signed(nc$adjusted)))
        ),
        div(class = "card", div(class = "card-body", nc$line)),

        if (res$design %in% c("oneshot", "prepost", "posttest_only"))
          plot_card(
            if (res$design == "oneshot") "The one number this design yields"
            else if (res$design == "prepost") "Before and after, one group"
            else "The two group means, measured only after",
            if (res$design == "oneshot")
              "There is nothing to compare against, so the figure shows the average alone."
            else "The bars are what the design gives. What it cannot give is a reason to credit the program.",
            "plot-simple")
        else if (res$design == "matching")
          div(class = "plot-grid",
            plot_card("Where the two groups overlap",
              "Matches can only form inside the shaded band, where both groups appear.",
              "plot-overlap"),
            plot_card("Balance before and after matching",
              "Each trait as an open marker before and a filled marker after. Filled markers inside the 0.1 lines pass.",
              "plot-love"))
        else if (res$design == "did")
          div(class = "plot-grid",
            plot_card("How each group moved over time",
              "Treated sites are filled circles, comparison sites are open squares.",
              "plot-trend"),
            plot_card("The gap between groups, against adoption",
              "Flat before the dashed line and a step after is the parallel paths story.",
              "plot-event"))
        else if (res$design == "its")
          plot_card("The series, its fit, and the trend carried forward",
            "The dashed line is the earlier trend extended past the interruption. The distance from it after the mark is the effect.",
            "plot-its")
        else if (res$design == "rdd")
          plot_card("The jump at the cutoff",
            "A line is fit on each side of the cutoff. The vertical gap where they meet is the estimate.",
            "plot-rdd")
        else
          plot_card("The first stage: does the instrument move take up?",
            "Program take up against the instrument. A visible slope is a strong instrument; a flat cloud is the weak instrument the F statistic warns about.",
            "plot-iv"),

        if (res$design == "matching") sensitivity_strip(res) else NULL,
        if (is.null(res$se)) NULL else mde_strip(res),

        cards,

        div(class = "caveat-box",
          h3("Caveats an analyst would attach"),
          tags$ul(lapply(interp$caveats, function(x) tags$li(x))),
          div(class = "caveat-src",
            "Written from the computed result. If a local model is running, use the button below to restate these in plainer words.")
        ),

        div(class = "btn-row",
          if (res$design %in% c("did", "its", "rdd"))
            actionButton("placebo", "Run a placebo test", class = "hbtn") else NULL,
          downloadButton("dl_report", "Download the write up", class = "hbtn"),
          downloadButton("dl_code", "Export reproducible R code", class = "hbtn"),
          actionButton("restate", "Restate with local model", class = "hbtn"))
      )
    )
  })

  # Refutation. Reshuffle the assignment many times, re estimate, and show how
  # the real effect compares with the placebo distribution that carries no true
  # effect. Available for the designs where the reshuffle is both fast and clean.
  observeEvent(input$placebo, {
    res <- state$result; df <- state$rundata
    if (is.null(res) || is.null(df)) return(NULL)
    spec <- design_spec(res$design)
    real <- switch(res$design, did = res$did, its = res$level, rdd = res$jump)
    iv_time <- if (res$design == "its") res$intervention_time else NULL
    B <- 200
    p <- run_placebo(res$design, df, spec, real, B = B, intervention = iv_time)
    showModal(placebo_modal(p, res$design))
  })

  observeEvent(input$restate, {
    interp <- state$interp
    if (is.null(interp)) return(NULL)
    prompt <- build_llm_prompt(interp, "general audience")
    out <- try_ollama(prompt)
    showModal(modalDialog(
      title = "Restated for a general audience",
      if (is.null(out))
        div(class = "card-body",
          "No local model answered. Causality looks for Ollama on this machine at its default address. The Local models button in the header explains how to set one up. The analysis above does not need it; every number was already computed.")
      else div(class = "card-body", HTML(gsub("\n", "<br>", htmlEscape(out)))),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  # Downloads. Each builds its file at request time from current state rather
  # than holding a prepared copy, so what leaves the app always matches what
  # is on screen.
  output$dl_report <- downloadHandler(
    filename = function() paste0("causality-", state$result$design, "-writeup.txt"),
    content = function(path) writeLines(build_report(state$result, state$interp), path)
  )
  # Reproducible R. A standalone base R script that reruns the current design on
  # the same data and prints the estimate, for the researcher who wants the
  # pipeline outside the app.
  output$dl_code <- downloadHandler(
    filename = function() paste0("causality-", state$result$design, "-reproduce.R"),
    content = function(path) {
      src <- if ((input$data_source %||% "upload") == "sample") "sample" else "upload"
      writeLines(build_r_code(state$design, src, input), path)
    }
  )
  output$dl_template_matching <- downloadHandler(
    filename = function() "causality-template-matching.csv",
    content = function(path) readr::write_csv(make_training_sample(), path)
  )
  output$dl_template_did <- downloadHandler(
    filename = function() "causality-template-before-after.csv",
    content = function(path) readr::write_csv(make_policy_sample(), path)
  )
}

# ---------------------------------------------------------------------------
# UI helper pieces
# ---------------------------------------------------------------------------

htmlEscape <- function(x) {
  x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x)
}

plot_card <- function(title, note, host_id) {
  div(class = "plot-wrap",
    div(class = "plot-title", title),
    div(class = "plot-note", note),
    div(id = host_id, class = "plot-host"))
}
identification_banner <- function(res) {
  chk <- identification_check(res)
  cls <- if (chk$ok) "ident-ok" else "ident-warn"
  mark <- if (chk$ok) "check" else "hold"
  div(class = paste("ident-banner", cls),
    div(class = "ident-head",
      span(class = paste("ident-mark", mark)),
      span(class = "ident-name",
        paste0(if (chk$ok) "Identification check passed: " else "Identification warning: ",
               chk$name))),
    div(class = "ident-msg", chk$message))
}

results_subhead <- function(res) {
  switch(res$design,
    oneshot = sprintf("One group posttest only. %d observations, no comparison.", res$n_used),
    prepost = sprintf("One group pretest posttest. %d paired observations.", res$n_used),
    posttest_only = sprintf("Posttest only with nonequivalent groups. %d treated and %d comparison.",
            res$n_treated, res$n_control),
    matching = sprintf("Matched comparison. %d matched pairs from %d served people, caliper %s.",
            res$n_pairs, res$n_treated, fmt_num(res$caliper_mult, 2)),
    did = sprintf("Before and after. %d adopting and %d comparison units across %d periods.",
            res$n_units_treated, res$n_units_control, res$n_pre + res$n_post),
    its = sprintf("Interrupted time series. %d periods before the interruption and %d after.",
            res$n_pre, res$n_post),
    rdd = sprintf("Regression discontinuity. %d of %d points fall within the bandwidth around the cutoff.",
            res$n_in_band, res$n_used),
    iv = sprintf("Instrumental variables, two stage least squares. %d observations, first stage F %s.",
            res$n_used, fmt_num(res$first_F, 1)))
}

# The plain language name of a design, for the tag at the top of a result.
design_full_name <- function(design) {
  switch(design,
    oneshot = "One group, treatment then one observation",
    prepost = "One group, observed before and after",
    posttest_only = "Two groups, posttest only, no pretest",
    matching = "Untreated comparison group, pretest and posttest",
    did = "Comparison group across repeated observations",
    its = "One series, interrupted at a known time",
    rdd = "Assignment by a cutoff on a running variable",
    iv = "An instrument for an endogenous program")
}

sensitivity_strip <- function(res) {
  g <- res$sensitivity$breaking_gamma
  num <- if (is.na(g)) "under 1.1" else fmt_num(g, 1)
  div(class = "stat-strip",
    div(class = "stat-num", num),
    div(class = "stat-lbl",
      "is how strong an unmeasured confounder would need to be, as an odds factor, to overturn this result. Higher is sturdier."))
}

# The forward looking counterpart to power. Rather than observed power, which
# computed after the fact only restates the p value, this reports the smallest
# true effect the sample could catch four times in five. It reads a near zero
# estimate as unproven when it sits below this floor, not as proof of nothing.
# Reported instead of observed power, which is computed after the fact from
# the effect you already have and so only restates the p value. This asks the
# forward looking question: what is the smallest true effect this sample could
# have caught.
mde_strip <- function(res) {
  se <- if (!is.null(res$se)) res$se
        else (res$ci[2] - res$ci[1]) / (2 * qnorm(0.975))
  m <- (qnorm(0.975) + qnorm(0.8)) * se
  div(class = "stat-strip",
    div(class = "stat-num", fmt_num(abs(m), 1)),
    div(class = "stat-lbl",
      "is the smallest true effect this sample could detect four times in five. An estimate smaller than this could be real yet missed, so read a near zero result here as unproven, not disproven."))
}

upload_hint <- function(design) {
  switch(design,
    oneshot = "Columns: an outcome. One row per person, measured after the program.",
    prepost = "Columns: a before measure and an after measure, one row per person.",
    posttest_only = "Columns: an outcome and a 0 or 1 column for who received the program.",
    matching = "Columns: an outcome, a 0 or 1 treatment column, and one or more background traits to match on.",
    did = "Columns: an outcome, a unit id, a time column, and a 0 or 1 column marking which units ever adopt.",
    its = "Columns: an outcome and a time column, one row per period, for a single series.",
    rdd = "Columns: an outcome and a running variable. Set the cutoff below.",
    iv = "Columns: an outcome, a 0 or 1 treatment column, and an instrument that shifts the treatment.")
}

empty_state <- function(design) {
  div(class = "reading",
    div(class = "result-designtag",
      span(class = "rd-label", "Design"),
      span(class = "rd-name", design_full_name(design))),
    div(class = "notation-host result-notation", `data-notation` = design),
    div(class = "plot-note",
      "O is an observation, X is the program, and the dashed line, where present, marks nonequivalent groups, in the notation of Shadish, Cook, and Campbell."),
    h1(class = "read-head", "Choose a design and run it"),
    div(class = "read-sub",
      "Pick a study design on the left, load your own CSV or switch to the built in sample, then run the analysis. If you load a file, the app suggests a design from its columns. The tour in the header walks through the idea in a minute."))
}

# Read whichever source is selected and check that the columns the chosen
# design needs are present. A missing column stops the run with a message
# naming exactly what was absent, rather than failing later inside a model.
load_data <- function(input, design, session) {
  src <- input$data_source %||% "upload"
  if (src == "sample") {
    return(switch(design,
      oneshot = make_onegroup_sample(),
      prepost = make_beforeafter1_sample(),
      posttest_only = make_twogroup_sample(),
      matching = make_training_sample(),
      did = make_policy_sample(),
      its = make_series_sample(),
      rdd = make_cutoff_sample(),
      iv = make_instrument_sample()))
  }
  fp <- input$file
  if (is.null(fp)) {
    showNotification("Choose a CSV file first, or switch Source to the built in sample.",
                     type = "warning")
    return(NULL)
  }
  df <- suppressWarnings(readr::read_csv(fp$datapath, show_col_types = FALSE))
  spec <- design_spec(design)
  need <- switch(design,
    oneshot = c(spec$outcome),
    prepost = c(spec$pre, spec$post),
    posttest_only = c(spec$outcome, spec$treat),
    matching = c(spec$outcome, spec$treat, spec$covariates),
    did = c(spec$outcome, spec$unit, spec$time, spec$treat),
    its = c(spec$outcome, spec$time),
    rdd = c(spec$outcome, spec$running),
    iv = c(spec$outcome, spec$treat, spec$instrument))
  missing <- setdiff(need, names(df))
  if (length(missing) > 0) {
    showNotification(
      paste("The file is missing these columns:", paste(missing, collapse = ", "),
            ". Open Data format in the header for the exact shape."),
      type = "error", duration = NULL)
    return(NULL)
  }
  df
}

# The sample spec that goes with a design key.
design_spec <- function(design) sample_specs[[DESIGNS[[design]]$spec]]

# Uploads are matched on every numeric column beyond the outcome and treatment,
# which avoids forcing a column picker on the user while still letting a real
# file work. Samples always use their known spec so the planted effect is exact.
resolve_spec <- function(input, df, design) {
  spec <- design_spec(design)
  if ((input$data_source %||% "upload") == "sample") return(spec)
  if (design == "matching") {
    known <- c(spec$outcome, spec$treat)
    covs <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], known)
    if (length(covs) == 0) covs <- spec$covariates
    list(outcome = spec$outcome, treat = spec$treat, covariates = covs)
  } else if (design == "rdd") {
    # Running variable: the numeric column, other than the outcome, that a
    # binary column splits at a threshold; otherwise the widest spread column.
    num <- names(df)[vapply(df, is.numeric, logical(1))]
    cont <- setdiff(num, spec$outcome)
    running <- if (spec$running %in% names(df)) spec$running
               else if (length(cont)) cont[which.max(vapply(cont,
                 function(c) stats::sd(df[[c]], na.rm = TRUE), numeric(1)))]
               else spec$running
    list(outcome = spec$outcome, running = running, cutoff = spec$cutoff)
  } else spec
}

# Ask a local model to reword a finished reading. Every failure path returns
# NULL so the caller can simply show the computed prose instead: a missing
# model is a normal condition here, not an error worth surfacing.
try_ollama <- function(prompt, model = "llama3.2") {
  body <- toJSON(list(model = model, prompt = prompt, stream = FALSE),
                 auto_unbox = TRUE)
  tryCatch({
    if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
    r <- httr::POST("http://127.0.0.1:11434/api/generate",
                    body = body, encode = "raw", httr::timeout(20))
    if (httr::status_code(r) != 200) return(NULL)
    fromJSON(httr::content(r, "text", encoding = "UTF-8"))$response
  }, error = function(e) NULL)
}

shinyApp(ui, server)
