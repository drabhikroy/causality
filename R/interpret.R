# Required Notice: Copyright Abhik Roy
# PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# interpret.R
# The reading layer. Every sentence here is assembled from numbers the
# analysis already computed. The engine chooses wording by where a result
# falls against fixed thresholds, so its confidence tracks the evidence
# rather than a tone dial. This is the last mile: the statistics are done,
# and this turns them into prose a non specialist can act on.

library(dplyr)

# How strongly to speak, decided by whether the interval clears zero and by
# how much of the interval sits on one side. A result whose interval straddles
# zero never gets causal language, no matter how large the point estimate.
confidence_word <- function(estimate, ci) {
  crosses_zero <- ci[1] <= 0 && ci[2] >= 0
  if (crosses_zero) return("inconclusive")
  width <- ci[2] - ci[1]
  ratio <- abs(estimate) / (width / 2)
  if (ratio >= 2.5) "clear" else if (ratio >= 1.3) "moderate" else "tentative"
}

# A balance table reads as good only when every trait sits under the 0.1
# rule of thumb after matching. The worst remaining trait sets the verdict,
# because one unbalanced confounder is enough to bias the estimate.
balance_verdict <- function(balance) {
  worst <- max(abs(balance$after), na.rm = TRUE)
  if (worst < 0.1) list(ok = TRUE, worst = worst)
  else list(ok = FALSE, worst = worst,
            trait = balance$covariate[which.max(abs(balance$after))])
}

# ---------------------------------------------------------------------------
# Matching interpretation
# ---------------------------------------------------------------------------

interpret_matching <- function(res) {
  conf <- confidence_word(res$att, res$ci)
  bal <- balance_verdict(res$balance)
  unit <- res$outcome_col

  headline <- if (conf == "inconclusive") {
    sprintf(
      "Among matched pairs, the program shows no clear effect on %s. The estimate is %s points, but the range of uncertainty runs from %s to %s and includes zero.",
      unit, fmt_signed(res$att), fmt_num(res$ci[1]), fmt_num(res$ci[2])
    )
  } else {
    sprintf(
      "Among people the program served, matching to comparable non participants points to a %s effect of %s points on %s, with a plausible range from %s to %s.",
      conf, fmt_signed(res$att), unit, fmt_num(res$ci[1]), fmt_num(res$ci[2])
    )
  }

  balance_line <- if (bal$ok) {
    sprintf(
      "Matching worked as intended. After pairing, every background trait differs between the groups by less than a tenth of a standard deviation, so the two groups are alike on what was measured."
    )
  } else {
    sprintf(
      "Matching left some imbalance. The trait %s still differs by %s standard deviations after pairing, above the 0.1 mark, so part of any gap could still trace to that difference rather than to the program.",
      bal$trait, fmt_num(bal$worst)
    )
  }

  coverage <- res$n_pairs / res$n_treated
  coverage_line <- if (coverage >= 0.85) {
    sprintf(
      "The result speaks for most of the served group: %s of %s treated people found a close match.",
      res$n_pairs, res$n_treated
    )
  } else {
    sprintf(
      "The result speaks for a subset. Only %s of %s treated people found a close enough match, so it describes those who resemble the comparison pool, not everyone served.",
      res$n_pairs, res$n_treated
    )
  }

  g <- res$sensitivity$breaking_gamma
  sens_line <- if (conf == "inconclusive") {
    "Because the effect is already inconclusive, a sensitivity bound would not add anything: there is no finding to overturn."
  } else if (is.na(g)) {
    "The result is fragile to hidden bias. Even a confounder no stronger than chance could account for it, so read it as suggestive at most."
  } else if (g <= 1.2) {
    sprintf(
      "The result is fragile. A hidden confounder that shifted the odds of receiving the program by as little as %s to one would be enough to explain it away, so an unmeasured cause is a live worry.",
      fmt_num(g, 1))
  } else if (g <= 2) {
    sprintf(
      "The result holds up to moderate hidden bias. A confounder would need to shift the odds of receiving the program by about %s to one before the finding loses significance.",
      fmt_num(g, 1))
  } else {
    sprintf(
      "The result is sturdy against hidden bias. It would take a strong unmeasured confounder, one shifting the odds of receiving the program by roughly %s to one, to overturn it.",
      fmt_num(g, 1))
  }

  list(
    headline = headline,
    cards = list(
      list(title = "What the matched comparison found", body = headline),
      list(title = "Whether the groups ended up comparable", body = balance_line),
      list(title = "Who the answer covers", body = coverage_line),
      list(title = "How much a hidden confounder could matter", body = sens_line)
    ),
    caveats = matching_caveats(res, bal, coverage)
  )
}

# The caveat text an analyst would attach before showing this to anyone.
# It names what the design cannot rule out, in plain terms, sized to the
# actual result rather than pasted boilerplate.
matching_caveats <- function(res, bal, coverage) {
  items <- c(
    "Matching balances only the traits provided here. Anything unmeasured that pushed people toward the program and also moved the outcome would still bias this estimate. Matching is not the same as a randomized trial.",
    sprintf("The estimate describes the people the program served, not the population at large. It is an effect for those who enrolled and found a match, %s of the treated group.",
            scales_pct(coverage))
  )
  if (!bal$ok) {
    items <- c(items, sprintf(
      "Residual imbalance on %s means the headline number should be read as an upper or lower bound rather than a point you can bank on.", bal$trait))
  }
  if (res$dropped_missing > 0) {
    items <- c(items, sprintf(
      "%s rows were set aside for missing values before matching. If those rows differ systematically, the matched sample is not a random slice of the whole.",
      fmt_num(res$dropped_missing, 0)))
  }
  items
}

# ---------------------------------------------------------------------------
# Difference in differences interpretation
# ---------------------------------------------------------------------------

interpret_did <- function(res) {
  conf <- confidence_word(res$did, res$ci)
  unit <- res$outcome_col

  headline <- if (conf == "inconclusive") {
    sprintf(
      "The before and after comparison shows no clear change in %s attributable to the policy. The estimate is %s, but its range from %s to %s includes zero.",
      unit, fmt_signed(res$did), fmt_num(res$ci[1]), fmt_num(res$ci[2])
    )
  } else {
    sprintf(
      "Compared with sites that never adopted, the policy is associated with a %s change of %s in %s once it took effect, with a plausible range from %s to %s.",
      conf, fmt_signed(res$did), unit, fmt_num(res$ci[1]), fmt_num(res$ci[2])
    )
  }

  trend_line <- if (is.na(res$trend_p)) {
    "There is only one period before adoption, so the parallel paths assumption cannot be checked. Read the result with that gap in mind."
  } else if (res$trend_p < 0.05) {
    sprintf(
      "The parallel paths check raises a flag. Before adoption the two groups were already drifting apart at different rates (gap of %s per period, p %s), which is the main thing this design needs to rule out. Treat the headline with caution.",
      fmt_signed(res$trend_gap), fmt_p(res$trend_p)
    )
  } else {
    sprintf(
      "The parallel paths check passes. Before adoption the two groups moved in step, with no meaningful difference in their trends (p %s), which is what this design relies on.",
      fmt_p(res$trend_p)
    )
  }

  placebo_line <- if (is.null(res$placebo)) {
    "There were too few pre periods to run a placebo test."
  } else if (abs(res$placebo$est) < 0.5 * abs(res$did) || res$did == 0) {
    sprintf(
      "A placebo test using a fake adoption point inside the pre period found little effect (%s), as it should when the comparison group is sound.",
      fmt_signed(res$placebo$est)
    )
  } else {
    sprintf(
      "A placebo test using a fake adoption point inside the pre period still found a sizable effect (%s), where none should exist. That points to a comparison group that was not tracking the treated sites even before the policy.",
      fmt_signed(res$placebo$est)
    )
  }

  list(
    headline = headline,
    cards = list(
      list(title = "What the before and after comparison found", body = headline),
      list(title = "Whether the groups moved in step beforehand", body = trend_line),
      list(title = "The placebo check", body = placebo_line)
    ),
    caveats = did_caveats(res)
  )
}

did_caveats <- function(res) {
  items <- c(
    "This design assumes the two groups would have kept moving in step if the policy had never happened. That cannot be proven, only checked against the pre period. Anything that hit one group and not the other at the same time as the policy would be mixed into this number.",
    "The result is an average across adopting sites. Individual sites may have gained more or less, and this does not describe any single one."
  )
  if (!is.na(res$trend_p) && res$trend_p < 0.05) {
    items <- c(items, "Because the pre period trends already diverged, the headline likely mixes a real effect with a difference that was building regardless. It should not be read as the policy alone.")
  }
  if (res$dropped_missing > 0) {
    items <- c(items, sprintf(
      "%s rows were set aside for missing values. Uneven missingness across sites or months can tilt the averages that feed this estimate.",
      fmt_num(res$dropped_missing, 0)))
  }
  items
}

# A small formatting helper used only in prose.
scales_pct <- function(x) paste0(round(100 * x), " percent")

# ---------------------------------------------------------------------------
# The naive contrast, shown on purpose
# ---------------------------------------------------------------------------

# The number someone would report without any design at all. Showing it next
# to the honest estimate is the whole point: it makes the confounding visible
# instead of leaving it implied.
naive_contrast <- function(res) {
  if (res$design == "oneshot") {
    return(list(value = res$mean, adjusted = res$mean,
      line = "There is no adjusted estimate to set beside the raw number, because this design supports none. The average is what the data give, and nothing about the program follows from it."))
  }
  if (res$design == "prepost") {
    return(list(value = res$est, adjusted = res$est,
      line = "The before to after change is shown twice because this design offers no adjustment: there is no comparison group to subtract off ordinary growth or shared events. The raw change is the whole of it."))
  }
  if (res$design == "posttest_only") {
    return(list(value = res$est, adjusted = res$est,
      line = "The gap is shown twice because this design offers no adjustment: with no pretest, nothing can be removed from it. Whatever difference existed between these groups beforehand is still inside this number."))
  }
  if (res$design == "matching") {
    d <- res$data
    yt <- d[[res$outcome_col]][d[[res$treat_col]] == 1]
    yc <- d[[res$outcome_col]][d[[res$treat_col]] == 0]
    raw <- mean(yt) - mean(yc)
    list(value = raw, adjusted = res$att,
         line = sprintf(
           "A plain comparison of enrollees against everyone else gives %s points. The matched estimate is %s. The gap between them is the confounding that matching removed.",
           fmt_signed(raw), fmt_signed(res$att)))
  } else if (res$design == "did") {
    d <- res$data
    post <- d %>% filter(post == 1)
    raw <- mean(post[[res$outcome_col]][post[[res$treat_col]] == 1]) -
      mean(post[[res$outcome_col]][post[[res$treat_col]] == 0])
    list(value = raw, adjusted = res$did,
         line = sprintf(
           "Comparing the two groups only after adoption gives %s. The before and after estimate is %s. The difference is the fixed gap between groups that this design subtracts out.",
           fmt_signed(raw), fmt_signed(res$did)))
  } else if (res$design == "its") {
    list(value = res$naive, adjusted = res$level,
         line = sprintf(
           "Simply averaging after against before gives %s. The segmented estimate of the immediate change is %s. The difference is the trend the series was already on, which a before and after average mistakes for an effect.",
           fmt_signed(res$naive), fmt_signed(res$level)))
  } else if (res$design == "rdd") {
    list(value = res$naive, adjusted = res$jump,
         line = sprintf(
           "Comparing everyone above the cutoff against everyone below gives %s. The estimate at the cutoff is %s. The difference is the score itself: people well above the cutoff differ from people well below for reasons the program did not cause, and only the jump at the cutoff sets that aside.",
           fmt_signed(res$naive), fmt_signed(res$jump)))
  } else {
    list(value = res$ols, adjusted = res$late,
         line = sprintf(
           "An ordinary comparison of enrollees against everyone else gives %s. The instrument based estimate is %s. The gap is the confounding that ordinary least squares cannot see and the instrument sidesteps, by using only the part of enrollment the instrument explains.",
           fmt_signed(res$ols), fmt_signed(res$late)))
  }
}

# ---------------------------------------------------------------------------
# The prompt handed to an optional local model
# ---------------------------------------------------------------------------

# The model is given the finished sentences and asked only to smooth them for
# one audience. It is told in plain terms never to invent or change a number.
# If no model is running, the app shows the computed prose unchanged.
build_llm_prompt <- function(interp, audience) {
  facts <- paste(c(interp$headline,
                   vapply(interp$cards, function(c) c$body, character(1)),
                   interp$caveats), collapse = "\n")
  paste0(
    "You are helping restate a completed statistical analysis for a ",
    audience, ". Rewrite the notes below as short, plain paragraphs. ",
    "Keep every number exactly as written. Do not add findings, do not ",
    "drop any caveat, and do not sound more certain than the notes. ",
    "If a note says a result is inconclusive, keep it inconclusive.\n\n",
    facts
  )
}

# ---------------------------------------------------------------------------
# Interrupted time series interpretation
# ---------------------------------------------------------------------------

interpret_its <- function(res) {
  conf <- confidence_word(res$level, res$ci)
  dir <- if (res$level < 0) "drop" else "rise"
  headline <- if (conf == "inconclusive") {
    sprintf("At the interruption the level changed by %s, but the interval crosses zero, so an immediate effect is not established.",
            fmt_signed(res$level))
  } else {
    sprintf("At the interruption the series shows an immediate %s of %s, a change the interval keeps clear of zero.",
            dir, fmt_signed(res$level))
  }

  slope_line <- if (abs(res$slope_change / res$slope_se) < 2) {
    "The trend afterward runs roughly parallel to the trend before, so the effect reads as a one time shift in level rather than a change in direction."
  } else {
    sprintf("The trend also changed: the slope moved by %s per period after the interruption, so the effect grows or fades over time rather than holding steady.",
            fmt_signed(res$slope_change, 2))
  }

  level_line <- sprintf(
    "The estimate compares the series after the interruption against its own earlier trend carried forward, not against a simple before and after average. That is what separates the program from wherever the series was already heading.")

  list(
    headline = headline,
    cards = list(
      list(title = "What the interruption did to the level", body = headline),
      list(title = "Whether the trend changed too", body = slope_line),
      list(title = "What the estimate is measured against", body = level_line)
    ),
    caveats = its_caveats(res)
  )
}

its_caveats <- function(res) {
  cav <- c(
    "A single series has no comparison group, so anything else that changed at the same time as the program would be folded into this estimate. A control series that did not get the program would guard against that.",
    "Standard errors here are ordinary least squares. Time series data usually carry serial correlation, which tends to make intervals look narrower than they should. Read a borderline result with that in mind.")
  if (res$n_pre < 8 || res$n_post < 8) {
    cav <- c(cav,
      sprintf("The series is short: %d periods before and %d after. A trend estimated from few points is unsteady, so the level change rests on a shaky baseline.",
              res$n_pre, res$n_post))
  }
  cav
}

# ---------------------------------------------------------------------------
# Regression discontinuity interpretation
# ---------------------------------------------------------------------------

interpret_rdd <- function(res) {
  conf <- confidence_word(res$jump, res$ci)
  dir <- if (res$jump < 0) "drop" else "jump"
  headline <- if (conf == "inconclusive") {
    sprintf("At the cutoff the outcome changes by %s, but the interval crosses zero, so a jump at the threshold is not established.",
            fmt_signed(res$jump))
  } else {
    sprintf("At the cutoff the outcome shows a %s of %s, a break the interval keeps clear of zero.",
            dir, fmt_signed(res$jump))
  }

  who_line <- "This is the effect for people right at the cutoff. It says nothing certain about people far above or far below it, where the program may work differently. That narrowness is the price the design pays for its strength at the threshold."

  strength_line <- "Because the cutoff decides who gets the program, people just above and just below it are alike but for the program, which is why the jump at the line reads as an effect rather than a difference in who these people are."

  list(
    headline = headline,
    cards = list(
      list(title = "What happens at the cutoff", body = headline),
      list(title = "Who the answer covers", body = who_line),
      list(title = "Why the jump reads as an effect", body = strength_line)
    ),
    caveats = rdd_caveats(res)
  )
}

rdd_caveats <- function(res) {
  cav <- c(
    sprintf("The estimate uses the %d points within the bandwidth around the cutoff, not the whole file. Points far from the cutoff do not inform the jump.",
            res$n_in_band),
    "The design assumes people cannot precisely control which side of the cutoff they land on. If they can, for instance by retaking a test to clear a threshold, the two sides stop being comparable and the jump is no longer clean.",
    "A curved relationship between the running variable and the outcome can masquerade as a jump. The straight lines fit here are a local approximation, trustworthy only near the cutoff.")
  cav
}

# ---------------------------------------------------------------------------
# Instrumental variables interpretation
# ---------------------------------------------------------------------------

interpret_iv <- function(res) {
  conf <- confidence_word(res$late, res$ci)
  headline <- if (res$weak) {
    sprintf("The instrument is too weak to trust: its first stage F is %s, below the value of ten that signals a usable instrument. The estimate of %s should be set aside until a stronger instrument is found.",
            fmt_num(res$first_F, 1), fmt_signed(res$late))
  } else if (conf == "inconclusive") {
    sprintf("Using the instrument, the effect is estimated at %s, but the interval crosses zero, so no effect is established.",
            fmt_signed(res$late))
  } else {
    sprintf("Using only the enrollment the instrument explains, the effect is %s, an estimate the interval keeps clear of zero.",
            fmt_signed(res$late))
  }

  strength_line <- if (res$weak) {
    sprintf("The first stage F statistic is %s. A weak instrument makes the estimate unstable and its interval untrustworthy, so the result above is reported but not to be relied on.",
            fmt_num(res$first_F, 1))
  } else {
    sprintf("The first stage F statistic is %s, comfortably above ten, so the instrument moves enrollment strongly enough for the estimate to stand.",
            fmt_num(res$first_F, 1))
  }

  who_line <- "An instrument speaks only for the people it actually sways: those who enroll because of the instrument and would not otherwise. It is silent about people who would always enroll or never would, so the number is a local effect, not an average across everyone."

  list(
    headline = headline,
    cards = list(
      list(title = "What the instrument based estimate is", body = headline),
      list(title = "Whether the instrument is strong enough", body = strength_line),
      list(title = "Who the answer covers", body = who_line)
    ),
    caveats = iv_caveats(res)
  )
}

iv_caveats <- function(res) {
  cav <- c(
    "The whole approach rests on the instrument having no path to the outcome except through the program. That assumption cannot be tested from the data; it has to be argued from how the instrument works. If distance to the site also affects earnings on its own, the estimate is biased.",
    "The estimate is a local one, for those the instrument moves. It need not match the effect for everyone, or the effect a broad rollout would produce.")
  if (res$weak) {
    cav <- c("The instrument is weak, first stage F below ten. Everything below should be read as unreliable until a stronger instrument is available.", cav)
  }
  cav
}

# ---------------------------------------------------------------------------
# Identification check
# ---------------------------------------------------------------------------

# The one assumption each design leans on hardest, surfaced as a pass or a
# warning that sits with the estimate rather than buried in the caveats. This
# is the guardrail: an effect is never shown without the state of the test that
# licenses it. A failure does not hide the number, it flags it.
identification_check <- function(res) {
  if (res$design %in% c("oneshot", "prepost", "posttest_only")) {
    return(identification_check_weak(res))
  }
  if (res$design == "matching") {
    bv <- balance_verdict(res$balance)
    list(ok = bv$ok, name = "Covariate balance after matching",
      message = if (bv$ok)
        "Every matched trait sits inside the 0.1 standard difference band, so the two groups are comparable on what was measured."
      else sprintf("The trait %s still differs by %s standard differences after matching, past the 0.1 mark, so the groups are not yet comparable and the estimate leans on that imbalance.", bv$trait, fmt_num(bv$worst, 2)))
  } else if (res$design == "did") {
    ok <- is.na(res$trend_p) || res$trend_p >= 0.05
    list(ok = ok, name = "Parallel trends before adoption",
      message = if (ok)
        sprintf("Before adoption the two groups moved in step, test p %s, which is the assumption this design rests on.", fmt_p(res$trend_p))
      else sprintf("Before adoption the two groups were already diverging, test p %s, so the parallel paths assumption is in doubt and the estimate may reflect that drift.", fmt_p(res$trend_p)))
  } else if (res$design == "its") {
    ok <- res$n_pre >= 8 && res$n_post >= 8
    list(ok = ok, name = "Enough periods to fix a trend",
      message = if (ok)
        sprintf("With %d periods before and %d after, the pre trend is estimated from enough points to carry it forward with some confidence.", res$n_pre, res$n_post)
      else sprintf("Only %d periods before and %d after. A trend fit to so few points is unsteady, so the level change rests on a shaky baseline.", res$n_pre, res$n_post))
  } else if (res$design == "rdd") {
    # A light manipulation check: the count just below the cutoff against just
    # above. A large imbalance hints that people sorted across the line.
    b <- res$binned
    near <- b[abs(b$x) <= res$bandwidth / 3, ]
    lo <- sum(near$x < 0); hi <- sum(near$x >= 0)
    ok <- lo > 0 && hi > 0 && max(lo, hi) / min(lo, hi) < 2.5
    list(ok = ok, name = "No obvious sorting at the cutoff",
      message = if (ok)
        "The points are spread across the cutoff without a pile up on one side, so there is no obvious sign that people sorted themselves across the line."
      else "The points bunch on one side of the cutoff, which can mean people sorted across the line and the two sides are not comparable.")
  } else {
    list(ok = !res$weak, name = "Instrument strength, first stage F",
      message = if (!res$weak)
        sprintf("The first stage F is %s, above ten, so the instrument moves take up strongly enough for the estimate to stand.", fmt_num(res$first_F, 1))
      else sprintf("The first stage F is %s, below ten. The instrument is weak, so the estimate is unstable and should not be relied on.", fmt_num(res$first_F, 1)))
  }
}

# ---------------------------------------------------------------------------
# The weaker designs: report the number, foreground the threats
# ---------------------------------------------------------------------------

interpret_oneshot <- function(res) {
  headline <- sprintf("The group averaged %s after the program. With no before measure and no comparison group, no part of this can be credited to the program.",
                      fmt_num(res$mean, 1))
  list(headline = headline,
    cards = list(
      list(title = "What the data show", body = headline),
      list(title = "Why no effect can be claimed", body = "An effect is a comparison against what would have happened otherwise. Here there is nothing to compare against: no earlier reading, no untreated group. The average is a description, not a result."),
      list(title = "What would rescue it", body = "Even one pretest, or a single comparison group, would open the door to an estimate. The book keeps this design mainly to make that point.")),
    caveats = c(
      "This is the weakest design in the book. It cannot separate the program from anything else that was true of these people.",
      "Report the average as a description only. Any causal reading of it would be unsupported."))
}

interpret_prepost <- function(res) {
  dir <- if (res$est >= 0) "rose" else "fell"
  headline <- sprintf("Scores %s by %s from before to after. Whether the program caused the change cannot be told apart from ordinary growth or other events over the same span.",
                      dir, fmt_signed(res$est))
  list(headline = headline,
    cards = list(
      list(title = "The before to after change", body = headline),
      list(title = "What could explain it besides the program", body = "History, meaning anything else that happened over the same period; maturation, meaning natural growth; and regression to the mean, meaning a low starting group drifting up on its own. A single group before and after cannot rule these out."),
      list(title = "What would rescue it", body = "A comparison group measured over the same span would let you subtract off growth and shared events, which is the before and after design further down the list.")),
    caveats = c(
      "The pre to post change is real, but its cause is open. Do not read it as the program effect.",
      "Regression to the mean is the quiet trap here: if the group was chosen for low scores, some rise was coming regardless."))
}

interpret_posttest_only <- function(res) {
  headline <- sprintf("The two groups differ by %s after the program. With no before measure, there is no way to know they were alike to begin with, so selection is folded into this number.",
                      fmt_signed(res$est))
  list(headline = headline,
    cards = list(
      list(title = "The gap between the groups", body = headline),
      list(title = "Why selection is the worry", body = "Whoever chose or was chosen for the program may have differed from the comparison group from the start. With no pretest, none of that difference can be measured or removed, so it sits inside the gap."),
      list(title = "What would rescue it", body = "A pretest on both groups, or matching on background traits, would let you check and adjust for who started where. Those are the stronger designs above.")),
    caveats = c(
      "Without a pretest the groups cannot be shown comparable, so read the gap as an upper bound tangled with selection, not a clean effect.",
      "If people chose the program themselves, the gap likely overstates it."))
}

# Identification checks for the weaker designs always warn, because their
# defining feature is a missing safeguard.
identification_check_weak <- function(res) {
  if (res$design == "oneshot") {
    list(ok = FALSE, name = "No counterfactual",
      message = "With no pretest and no comparison group, there is nothing to compare against, so no causal effect is identified. The average is descriptive only.")
  } else if (res$design == "prepost") {
    list(ok = FALSE, name = "No comparison group",
      message = "One group before and after cannot separate the program from history, maturation, or regression to the mean. The change is reported, but its cause is not established.")
  } else {
    list(ok = FALSE, name = "No pretest to check comparability",
      message = "With no before measure, there is no way to confirm the groups started alike, so selection is folded into the gap. The difference is reported, but not as a clean effect.")
  }
}
