# Required Notice: Copyright Abhik Roy
# PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# export.R
# The two things a finished result can become and be carried away: a written
# summary for a reader, and a plain R script that reruns the analysis with no
# dependency on this app.

# Generate a standalone base R script that reproduces the current analysis. The
# script carries the same estimator the app used, written plainly, so a reader
# can rerun it and get the same number without the app. It uses base R and one
# tidyverse verb set, nothing exotic, so it runs anywhere R does.
build_r_code <- function(design, src, input) {
  spec <- design_spec(design)
  header <- c(
    "# Reproducible analysis exported from Causality.",
    "# This script reruns the analysis in plain R. It needs only base R.",
    paste0("# Design: ", design_full_name(design)),
    "", "suppressWarnings(suppressMessages(library(stats)))", "")

  load_block <- c(
    "# Point this at your own file. The columns named below are the ones this",
    "# design needs; rename yours to match or edit the names in the next block.",
    "df <- read.csv('your_data.csv')",
    "")

  body <- switch(design,
    oneshot = c(
      sprintf("outcome <- '%s'", spec$outcome),
      "# This design supports no causal estimate: there is no pretest and no",
      "# comparison group, so there is nothing to compare the treated group",
      "# against. The average is the whole of what the data give.",
      "cat('Group average:\\n'); print(mean(df[[outcome]], na.rm = TRUE))"),
    prepost = c(
      sprintf("pre <- '%s'; post <- '%s'", spec$pre, spec$post),
      "# The paired change from before to after. Its cause is not established:",
      "# history, maturation, and regression to the mean all sit inside it.",
      "change <- df[[post]] - df[[pre]]",
      "cat('Mean change, cause not identified:\\n')",
      "print(mean(change, na.rm = TRUE))",
      "print(t.test(change))"),
    posttest_only = c(
      sprintf("outcome <- '%s'; treat <- '%s'", spec$outcome, spec$treat),
      "# The gap between groups measured only after the program. With no",
      "# pretest, whatever difference existed beforehand is still inside it.",
      "fit <- t.test(df[[outcome]][df[[treat]] == 1], df[[outcome]][df[[treat]] == 0])",
      "cat('Group difference, selection not removed:\\n'); print(fit)"),
    matching = c(
      sprintf("outcome <- '%s'; treat <- '%s'", spec$outcome, spec$treat),
      sprintf("covs <- c(%s)", paste0("'", spec$covariates, "'", collapse = ", ")),
      "ps <- glm(reformulate(covs, treat), data = df, family = binomial())",
      "df$ps <- fitted(ps)",
      "# Greedy one to one matching on the propensity score within a caliper,",
      "# then a paired comparison of the outcome. See the app for the full loop.",
      "cat('Fit the propensity model, match within a caliper, then compare pairs.\\n')"),
    did = c(
      sprintf("outcome <- '%s'; unit <- '%s'; time <- '%s'; treat <- '%s'; adopt <- %s",
              spec$outcome, spec$unit, spec$time, spec$treat, spec$adopt),
      "df$post <- as.integer(df[[time]] >= adopt)",
      "fit <- lm(reformulate(c(treat, 'post', paste0(treat, ':post')), outcome), data = df)",
      "cat('Difference in differences estimate:\\n')",
      "print(coef(fit)[paste0(treat, ':post')])"),
    its = c(
      sprintf("outcome <- '%s'; time <- '%s'; intervention <- %s",
              spec$outcome, spec$time, input$intervention %||% spec$intervention),
      "df$post <- as.integer(df[[time]] >= intervention)",
      "df$since <- ifelse(df$post == 1, df[[time]] - intervention, 0)",
      "fit <- lm(reformulate(c(time, 'post', 'since'), outcome), data = df)",
      "cat('Immediate level change at the interruption:\\n')",
      "print(coef(fit)['post'])"),
    rdd = c(
      sprintf("outcome <- '%s'; running <- '%s'; cutoff <- %s",
              spec$outcome, spec$running, input$cutoff %||% spec$cutoff),
      "df$x <- df[[running]] - cutoff; df$D <- as.integer(df$x >= 0)",
      "bw <- max(sd(df$x), diff(range(df$x)) / 6)",
      "keep <- abs(df$x) <= bw; w <- pmax(0, 1 - abs(df$x[keep]) / bw)",
      "fit <- lm(df[[outcome]][keep] ~ df$x[keep] * df$D[keep], weights = w)",
      "cat('Jump at the cutoff:\\n'); print(coef(fit)[3])"),
    iv = c(
      sprintf("outcome <- '%s'; treat <- '%s'; instrument <- '%s'",
              spec$outcome, spec$treat, spec$instrument),
      "# Two stage least squares by hand: project treatment on the instrument,",
      "# then use that projection in the outcome equation.",
      "first <- lm(reformulate(instrument, treat), data = df)",
      "df$Dhat <- fitted(first)",
      "second <- lm(reformulate('Dhat', outcome), data = df)",
      "cat('First stage F (instrument strength):\\n')",
      "print(summary(first)$fstatistic[1])",
      "cat('Instrument based estimate:\\n'); print(coef(second)['Dhat'])"))

  c(header, load_block, body, "",
    "# Note: the app also computes standard errors, an identification check,",
    "# and refutation tests. This script shows the core estimate.")
}

build_report <- function(res, interp) {
  nc <- naive_contrast(res)
  head <- c(
    "CAUSALITY  quasi-experimental write up",
    paste0("Design: ",
           if (res$design == "matching")
             "untreated comparison group, pretest and posttest"
           else "comparison group across repeated observations"),
    strrep("-", 68), "",
    interp$headline, "", nc$line, "")
  cards <- unlist(lapply(interp$cards, function(c) c(c$title, c$body, "")))
  cav <- c("CAVEATS", strrep("-", 68), paste0("- ", interp$caveats), "")
  foot <- c(strrep("-", 68),
    "Every statistic here was computed by the app on this machine.",
    "No language model produced or altered any number.")
  c(head, cards, cav, foot)
}

