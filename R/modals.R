# Required Notice: Copyright Abhik Roy
# PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# modals.R
# The pop up windows: the placebo result, the data format guide, and the local
# model explainer. These are long stretches of prose carrying very little logic,
# so they sit apart from the interface and the server to keep both readable.


# The identification banner. It names the design's key assumption and shows
# whether the test of it passed, in a calm strip when it holds and a warning
# strip when it does not. It always appears, so an effect never stands without
# the state of the check that licenses it.
# The placebo result, read plainly. A real effect that dwarfs the placebo
# spread, with a small pseudo p, is what a sound design should show.
placebo_modal <- function(p, design) {
  verdict <- if (p$pseudo_p < 0.05) {
    "The real effect stands well clear of the placebo spread. Reshuffled assignments almost never produce an effect this large, which is what a sound design should show."
  } else if (p$pseudo_p < 0.15) {
    "The real effect is larger than most placebo runs, but not by a wide margin. Read it with some caution."
  } else {
    "The real effect sits within the range that reshuffled assignments produce by chance. That is a warning: the design may be picking up noise rather than a true effect."
  }
  modalDialog(
    title = NULL, easyClose = TRUE, size = "l", footer = modalButton("Close"),
    div(class = "modal-card",
      h2("Placebo refutation"),
      tags$p(class = "read-sub",
        sprintf("Assignment was reshuffled %d times and the design re estimated on each, building a distribution of effects that should carry no true signal.",
                p$n_runs)),
      div(class = "stat-strip",
        div(class = "stat-num", fmt_p(p$pseudo_p)),
        div(class = "stat-lbl",
          "is the share of placebo runs whose effect matched or beat the real one. Small is good: it means the real effect is hard to reproduce by chance.")),
      tags$p(sprintf("Real effect: %s. Placebo effects centered at %s with a spread of %s.",
                     fmt_signed(p$real), fmt_signed(p$placebo_mean), fmt_num(p$placebo_sd, 2))),
      tags$p(verdict)
    )
  )
}

# ---------------------------------------------------------------------------
# Modals
# ---------------------------------------------------------------------------

# The data guide is deliberately concrete: a schema table with a worked example
# row for each design, plus a downloadable template, because an abstract column
# list leaves a first time user guessing at the exact shape.
guide_modal <- function(design) {
  modalDialog(
    title = NULL, easyClose = TRUE, size = "l",
    footer = modalButton("Close"),
    div(class = "modal-card",
      h2("How to format your data"),
      tags$p(class = "read-sub",
        "Causality reads a plain CSV: one row per observation, a header row of column names, and no merged cells or summary rows. Which columns it needs depends on the design."),

      h3("One group, measured once after"),
      tags$p("The simplest file there is: one outcome column, one row per person. This design identifies no effect, so the app reports the average and says so."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "wellbeing")), tags$td("the outcome"), tags$td("71.2")))),

      h3("One group, before and after"),
      tags$p("Two columns holding the same measure at two times, one row per person. The change is reported, its cause left open."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "score_before")), tags$td("the earlier measure"), tags$td("61.0")),
          tags$tr(tags$td(span(class = "schema-req", "score_after")), tags$td("the later measure"), tags$td("67.5")))),

      h3("Two groups, measured only after"),
      tags$p("An outcome and a marker for who received the program. With no pretest the groups cannot be shown comparable, so the gap is reported with that stated."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "confidence")), tags$td("the outcome"), tags$td("66.4")),
          tags$tr(tags$td(span(class = "schema-req", "attended")), tags$td("1 if in the program, else 0"), tags$td("1")))),

      h3("Matched comparison"),
      tags$p("One row per person. You need an outcome, a column marking who received the program as 1 and who did not as 0, and one or more background traits to match on. Any extra numeric columns are used as matching traits."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "assessment")), tags$td("the outcome"), tags$td("74")),
          tags$tr(tags$td(span(class = "schema-req", "enrolled")), tags$td("1 if served, else 0"), tags$td("1")),
          tags$tr(tags$td("tenure_years"), tags$td("a trait to match on"), tags$td("6.2")),
          tags$tr(tags$td("prior_score"), tags$td("a trait to match on"), tags$td("68")))),
      downloadButton("dl_template_matching", "Download a matching template CSV", class = "hbtn"),

      h3("Before and after"),
      tags$p("One row per unit per time period. You need an outcome, a unit id, a time column, and a column marking which units ever adopt the change as 1 and which never do as 0. The same unit appears on many rows, once per period."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "wait_minutes")), tags$td("the outcome"), tags$td("39.1")),
          tags$tr(tags$td(span(class = "schema-req", "site_id")), tags$td("the unit id"), tags$td("Clinic 03")),
          tags$tr(tags$td(span(class = "schema-req", "month")), tags$td("the time period"), tags$td("14")),
          tags$tr(tags$td(span(class = "schema-req", "adopts")), tags$td("1 if the unit ever adopts, else 0"), tags$td("1")))),
      downloadButton("dl_template_did", "Download a before and after template CSV", class = "hbtn"),

      h3("Interrupted time series"),
      tags$p("One row per period, for a single series. You need an outcome and a time column. The interruption period is set in the sidebar, not in the file."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "daily_visits")), tags$td("the outcome"), tags$td("221.4")),
          tags$tr(tags$td(span(class = "schema-req", "week")), tags$td("the time period"), tags$td("27")))),

      h3("Regression discontinuity"),
      tags$p("One row per person. You need an outcome and a running variable, the score that decides who receives the program. The cutoff is set in the sidebar. A column marking who received the program is optional; the cutoff defines it."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "gpa_next_year")), tags$td("the outcome"), tags$td("2.9")),
          tags$tr(tags$td(span(class = "schema-req", "entrance_score")), tags$td("the running variable"), tags$td("61.5")))),

      h3("Instrumental variables"),
      tags$p("One row per person. You need an outcome, a marker for who took the program, and an instrument: something that shifts take up but has no path to the outcome except through the program."),
      tags$table(class = "schema-table",
        tags$thead(tags$tr(tags$th("column"), tags$th("meaning"), tags$th("example"))),
        tags$tbody(
          tags$tr(tags$td(span(class = "schema-req", "earnings")), tags$td("the outcome"), tags$td("48.3")),
          tags$tr(tags$td(span(class = "schema-req", "enrolled")), tags$td("1 if took the program, else 0"), tags$td("1")),
          tags$tr(tags$td(span(class = "schema-req", "distance_miles")), tags$td("the instrument"), tags$td("4.10")))),

      h3("A few rules that keep results honest"),
      tags$ul(
        tags$li("The treatment column must be exactly 0 and 1, not text labels."),
        tags$li("Missing values are allowed; rows missing a needed column are set aside and counted for you."),
        tags$li("For before and after, every unit needs at least one period before and one after the change to contribute."))
    )
  )
}

models_modal <- function() {
  modalDialog(
    title = NULL, easyClose = TRUE, size = "l",
    footer = modalButton("Close"),
    div(class = "modal-card",
      h2("Adding a local language model"),
      tags$p(class = "read-sub",
        "This is optional. Causality computes every number on its own. A local model only rewrites the finished result in plainer words, and it runs on your machine, so nothing leaves it."),
      h3("What a local model is"),
      tags$p("A language model is the kind of program behind chat assistants. Most run on a company server. A local model runs on your own computer instead. You download it once, and after that it works with no internet and sends nothing anywhere. For this app that matters, because your data and your results stay put."),
      h3("How to set one up"),
      tags$p("The simplest path is a free tool called Ollama."),
      tags$ol(
        tags$li("Go to ollama.com and download the app for your system."),
        tags$li("Open it. It runs quietly in the background."),
        tags$li("In a terminal, type: ollama pull llama3.2  and press enter. That fetches one model."),
        tags$li("Come back here and use Restate with local model. Causality finds it automatically.")),
      h3("Which model to choose"),
      tags$p("Bigger models write a little more smoothly but need more memory and disk. For restating a few paragraphs, a small one is plenty. Sizes are downloads; memory is roughly what the model needs while running."),
      div(class = "model-row",
        div(class = "model-name", "Llama 3.2 (3B)"),
        div(class = "model-meta", "2 GB download, needs 8 GB memory"),
        div(class = "model-note", "The recommended starting point. Fast on most laptops from the last few years and more than good enough for rewriting text. Type ollama pull llama3.2 to get it.")),
      div(class = "model-row",
        div(class = "model-name", "Phi 3 Mini"),
        div(class = "model-meta", "2.3 GB download, needs 8 GB memory"),
        div(class = "model-note", "A close alternative, sometimes crisper on short instructions. A fine choice if Llama feels wordy. Type ollama pull phi3.")),
      div(class = "model-row",
        div(class = "model-name", "Mistral 7B"),
        div(class = "model-meta", "4.1 GB download, needs 16 GB memory"),
        div(class = "model-note", "A step up in fluency at a real cost in memory. Worth it only if your machine has room to spare. Type ollama pull mistral.")),
      div(class = "model-row",
        div(class = "model-name", "Gemma 2 (2B)"),
        div(class = "model-meta", "1.6 GB download, needs 6 GB memory"),
        div(class = "model-note", "The lightest option, for older or smaller machines. A touch plainer in tone but reliable. Type ollama pull gemma2:2b.")),
      h3("The one rule that never changes"),
      tags$p("Whichever model you add, or none at all, every measure and every sentence of the analysis is computed by this app on this machine. A model only ever restates what the app already found. It cannot change a number, and it never sees your data unless it is running locally on your own computer.")
    )
  )
}

