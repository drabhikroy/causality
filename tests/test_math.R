# test_math.R
# The analysis is only worth trusting if it recovers an answer we planted.
# Both samples carry a known true effect, so these checks assert the engines
# land near it and that the diagnostics flag what they should. A future change
# that quietly breaks the arithmetic fails here.

suppressMessages({
  library(dplyr)
  source("R/causal_math.R"); source("R/samples.R"); source("R/interpret.R")
})

fails <- 0
expect <- function(label, cond) {
  if (isTRUE(cond)) cat(sprintf("ok    %s\n", label))
  else { fails <<- fails + 1; cat(sprintf("FAIL  %s\n", label)) }
}

# ---- Matching: planted true effect on the served group is +4.0 ----
tr <- make_training_sample()
m <- run_matching(tr, "assessment", "enrolled",
                  c("tenure_years", "prior_score", "age", "remote"))

expect("matching recovers the planted effect within a point",
       abs(m$att - 4.0) < 1.0)
expect("matched interval excludes zero for a real effect",
       m$ci[1] > 0)
expect("matching balances every trait under 0.1 after pairing",
       max(abs(m$balance$after)) < 0.1)
expect("matching removed confounding: adjusted below the naive contrast",
       naive_contrast(m)$value > m$att)
expect("every matched control is used at most once",
       !any(duplicated(m$pairs$control_row)))
expect("caliper kept at least most treated units",
       m$n_pairs >= 0.8 * m$n_treated)

# ---- Difference in differences: planted true effect is -6.0 ----
po <- make_policy_sample()
d <- run_did(po, "wait_minutes", "site_id", "month", "adopts",
             adopt_time = 13, reps = 499)

expect("did recovers a clear reduction near the planted effect",
       d$did < -3 && d$did > -8)
expect("did interval excludes zero for a real effect",
       d$ci[2] < 0)
expect("parallel paths check passes on the clean sample",
       is.na(d$trend_p) || d$trend_p >= 0.05)
expect("placebo effect is small where none should exist",
       is.null(d$placebo) || abs(d$placebo$est) < 2)

# ---- Interpretation calibration: language must track the evidence ----
# A fabricated null result must read as inconclusive, never causal.
fake_null <- list(att = 0.2, ci = c(-3, 3.4), outcome_col = "y",
                  balance = data.frame(covariate = "x", before = 0.3, after = 0.05),
                  n_pairs = 50, n_treated = 55, dropped_missing = 0,
                  design = "matching", treat_col = "t",
                  data = data.frame(y = c(1, 2), t = c(0, 1)))
class(fake_null) <- "list"
ni <- interpret_matching(fake_null)
expect("a straddling interval reads as inconclusive",
       grepl("no clear effect", ni$headline))

expect("a strong effect reads with confidence",
       grepl("clear|moderate", interpret_did(d)$headline))

# ---- Wire format must be row records, not columns ----
w <- wire_matching(m)
expect("wire balance is a list of row records",
       is.list(w$balance) && !is.null(w$balance[[1]]$covariate))
wd <- wire_did(d)
expect("wire series is a list of row records",
       is.list(wd$series) && !is.null(wd$series[[1]]$m))

# ---- Interrupted time series recovers a planted level drop ----
its <- run_its(make_series_sample(), "daily_visits", "week", 27)
expect("ITS recovers the planted level drop within two of minus eighteen",
       abs(its$level - (-18)) < 2)
expect("ITS level change is significant", its$p < 0.01)
expect("ITS slope change is near zero", abs(its$slope_change) < 0.5)
expect("ITS wire is row records",
       is.list(wire_its(its)$series) && !is.null(wire_its(its)$series[[1]]$y))

# ---- Regression discontinuity recovers a planted jump ----
rd <- run_rdd(make_cutoff_sample(), "gpa_next_year", "entrance_score", 60)
expect("RDD recovers the planted jump within 0.1 of 0.35",
       abs(rd$jump - 0.35) < 0.1)
expect("RDD jump is significant", rd$p < 0.01)
expect("RDD uses only points inside the bandwidth", rd$n_in_band < rd$n_used)

# ---- Detection names the right design for each sample ----
expect("detects matching", detect_design(make_training_sample())$design == "matching")
expect("detects did", detect_design(make_policy_sample())$design == "did")
expect("detects its", detect_design(make_series_sample())$design == "its")
expect("detects rdd", detect_design(make_cutoff_sample())$design == "rdd")
expect("detects iv", detect_design(make_instrument_sample())$design == "iv")

# ---- Instrumental variables recovers the planted effect where OLS is biased ----
iv <- run_iv(make_instrument_sample(), "earnings", "enrolled", "distance_miles")
expect("IV recovers the planted effect within one of eight", abs(iv$late - 8) < 1.5)
expect("OLS is biased upward, above the true effect", iv$ols > 9)
expect("first stage F is strong, above ten", iv$first_F > 10)
expect("strong instrument is not flagged weak", !iv$weak)

# ---- Identification check reports a state for every design ----
source("R/refute.R")
for (r in list(m, d, its, rd, iv)) {
  ic <- identification_check(r)
  expect(paste("identification check has a verdict for", r$design),
         is.logical(ic$ok) && nchar(ic$message) > 20)
}

# ---- Placebo refutation: real effect beats the placebo spread on a clean design ----
pl <- run_placebo("rdd", make_cutoff_sample(), sample_specs$cutoff, 0.38, B = 120)
expect("RDD placebo effects center near zero", abs(pl$placebo_mean) < 0.15)
expect("RDD real effect beats the placebo spread", pl$pseudo_p < 0.05)

# ---- The weaker designs report a number and refuse to identify an effect ----
os <- run_oneshot(make_onegroup_sample(), "wellbeing")
expect("one group posttest only reports a mean", is.finite(os$mean))
expect("one group posttest only is not identified", isFALSE(os$identified))
pp <- run_prepost(make_beforeafter1_sample(), "score_before", "score_after")
expect("pretest posttest recovers the built in change near six", abs(pp$est - 6) < 1.5)
expect("pretest posttest is not identified", isFALSE(pp$identified))
po2 <- run_posttest_only(make_twogroup_sample(), "confidence", "attended")
expect("posttest only reports a group gap", is.finite(po2$est))
expect("posttest only is not identified", isFALSE(po2$identified))

# Their identification checks must all warn, since that is their whole lesson.
for (r in list(os, pp, po2)) {
  expect(paste("weak design warns:", r$design), isFALSE(identification_check(r)$ok))
}
# And the identified designs must not warn on the built in samples.
for (r in list(m, d, its, rd, iv)) {
  expect(paste("identified design passes:", r$design), isTRUE(identification_check(r)$ok))
}

# ---- Detection covers all eight structures ----
expect("detects oneshot", detect_design(make_onegroup_sample())$design == "oneshot")
expect("detects prepost", detect_design(make_beforeafter1_sample())$design == "prepost")
expect("detects posttest only",
       detect_design(make_twogroup_sample())$design == "posttest_only")

cat(sprintf("\nMath: %d failures\n", fails))
if (fails > 0) quit(status = 1)
