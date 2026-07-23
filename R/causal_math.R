# Required Notice: Copyright Abhik Roy
# PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# causal_math.R
# The analysis layer for Otherwise. Two study designs live here: a matched
# comparison built on propensity scores, and a before and after comparison
# across two groups (difference in differences). Everything is computed with
# ordinary regression and arithmetic so a reviewer can trace every number.
# No model, local or remote, ever produces a statistic in this app.

library(dplyr)
library(tidyr)
library(purrr)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

# Formatting lives in one place so the reading panel, the report, and the
# table never disagree about how a number looks.
fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_signed <- function(x, digits = 2) {
  paste0(ifelse(x >= 0, "+", ""), fmt_num(x, digits))
}

fmt_p <- function(p) {
  if (is.na(p)) return("not available")
  if (p < 0.001) return("below 0.001")
  fmt_num(p, 3)
}

# The logit of the propensity score is the standard scale for the caliper
# because distances there behave the same at the middle and the edges of
# the probability range.
logit <- function(p) log(p / (1 - p))

# ---------------------------------------------------------------------------
# Standardized mean differences
# ---------------------------------------------------------------------------

# One number per trait describing how far apart the two groups sit, in
# units of spread. The denominator always comes from the full sample before
# matching so the before and after values share a scale and can be compared
# on the same axis.
smd_table <- function(data, covariates, treat_col, pooled_from = NULL) {
  base <- if (is.null(pooled_from)) data else pooled_from
  map_dfr(covariates, function(cv) {
    x <- data[[cv]]
    t <- data[[treat_col]] == 1
    xb <- base[[cv]]
    tb <- base[[treat_col]] == 1
    # A pooled spread from both groups keeps one group from dominating the
    # scale when their variances differ.
    sd_pool <- sqrt((var(xb[tb]) + var(xb[!tb])) / 2)
    d <- if (sd_pool > 0) (mean(x[t]) - mean(x[!t])) / sd_pool else 0
    tibble(covariate = cv, smd = d)
  })
}

# ---------------------------------------------------------------------------
# Matched comparison (propensity scores)
# ---------------------------------------------------------------------------

# Greedy one to one matching without replacement. Treated rows are visited
# from the hardest to match (highest score) downward, because those rows
# run out of close comparisons first if left for last. A caliper of 0.2
# standard deviations of the logit score is the widely used default from
# Austin (2011) and keeps poor matches out rather than papering over them.
run_matching <- function(data, outcome_col, treat_col, covariates,
                         caliper_mult = 0.2) {
  d <- data %>%
    filter(if_all(all_of(c(outcome_col, treat_col, covariates)), ~ !is.na(.x))) %>%
    mutate(.row = row_number())
  dropped_missing <- nrow(data) - nrow(d)

  form <- reformulate(covariates, response = treat_col)
  ps_model <- glm(form, data = d, family = binomial())
  d$ps <- as.numeric(fitted(ps_model))
  d$ps_logit <- logit(pmin(pmax(d$ps, 1e-6), 1 - 1e-6))

  # The caliper is the multiplier times the spread of the logit score. The
  # multiplier is exposed so a user can see how a stricter or looser rule
  # changes coverage and balance; 0.2 is the Austin (2011) default.
  caliper <- caliper_mult * sd(d$ps_logit)

  treated <- d %>% filter(.data[[treat_col]] == 1) %>% arrange(desc(ps))
  controls <- d %>% filter(.data[[treat_col]] == 0)
  available <- rep(TRUE, nrow(controls))

  pairs <- map_dfr(seq_len(nrow(treated)), function(i) {
    gaps <- abs(controls$ps_logit - treated$ps_logit[i])
    gaps[!available] <- Inf
    j <- which.min(gaps)
    if (is.finite(gaps[j]) && gaps[j] <= caliper) {
      available[j] <<- FALSE
      tibble(treated_row = treated$.row[i], control_row = controls$.row[j],
             gap = gaps[j])
    } else {
      tibble(treated_row = treated$.row[i], control_row = NA_integer_,
             gap = NA_real_)
    }
  })

  matched <- pairs %>% filter(!is.na(control_row))
  unmatched_n <- sum(is.na(pairs$control_row))

  # The effect for the people the program actually served: each treated
  # person against their statistical twin, then the average of those gaps.
  yt <- d[[outcome_col]][match(matched$treated_row, d$.row)]
  yc <- d[[outcome_col]][match(matched$control_row, d$.row)]
  diffs <- yt - yc
  att <- mean(diffs)
  se <- sd(diffs) / sqrt(length(diffs))
  tcrit <- qt(0.975, df = length(diffs) - 1)

  matched_rows <- c(matched$treated_row, matched$control_row)
  d_matched <- d %>% filter(.row %in% matched_rows)

  balance_before <- smd_table(d, covariates, treat_col)
  balance_after <- smd_table(d_matched, covariates, treat_col, pooled_from = d)
  balance <- balance_before %>%
    rename(before = smd) %>%
    left_join(rename(balance_after, after = smd), by = "covariate")

  # Common support: the score range where both groups actually appear.
  # Estimates outside that range would rest on extrapolation, not evidence.
  support <- c(max(min(d$ps[d[[treat_col]] == 1]), min(d$ps[d[[treat_col]] == 0])),
               min(max(d$ps[d[[treat_col]] == 1]), max(d$ps[d[[treat_col]] == 0])))

  breaks <- seq(0, 1, by = 0.05)
  hist_t <- hist(d$ps[d[[treat_col]] == 1], breaks = breaks, plot = FALSE)$counts
  hist_c <- hist(d$ps[d[[treat_col]] == 0], breaks = breaks, plot = FALSE)$counts

  sens <- rosenbaum_bounds(diffs)

  list(
    design = "matching",
    n_total = nrow(data), n_used = nrow(d), dropped_missing = dropped_missing,
    n_treated = nrow(treated), n_control = nrow(controls),
    n_pairs = nrow(matched), unmatched = unmatched_n,
    caliper = caliper, caliper_mult = caliper_mult, att = att, se = se,
    ci = c(att - tcrit * se, att + tcrit * se),
    p = 2 * pt(-abs(att / se), df = length(diffs) - 1),
    balance = balance, support = support, sensitivity = sens,
    hist = list(breaks = breaks, treated = hist_t, control = hist_c),
    pairs = matched, data = d, outcome_col = outcome_col,
    treat_col = treat_col, covariates = covariates
  )
}

# ---------------------------------------------------------------------------
# Rosenbaum sensitivity bounds
# ---------------------------------------------------------------------------

# Matching removes bias from the traits you measured, never from the ones you
# did not. This asks the natural follow up: how strong would an unmeasured
# confounder have to be to overturn the result. Gamma is the factor by which
# such a confounder would raise one pair member odds of receiving the program.
# The bound is built on the Wilcoxon signed rank statistic of the pair
# differences, following Rosenbaum. A higher breaking gamma means a sturdier
# finding, because only a stronger hidden bias could explain it away.
rosenbaum_bounds <- function(diffs, gammas = seq(1, 3, by = 0.1)) {
  d <- diffs[diffs != 0]                 # signed rank drops exact ties
  if (length(d) < 3) {
    return(list(breaking_gamma = NA_real_, table = NULL))
  }
  r <- rank(abs(d))
  sr <- sum(r); sr2 <- sum(r^2)
  w_obs <- sum(r[d > 0])                 # sum of ranks where the pair gained

  rows <- lapply(gammas, function(g) {
    # The conservative bound uses the assignment probability that most inflates
    # the null expectation, making the observed statistic least surprising.
    mu_plus <- (g / (1 + g)) * sr
    v <- (g / ((1 + g) ^ 2)) * sr2
    z_cons <- (w_obs - mu_plus) / sqrt(v)
    data.frame(gamma = g, p_upper = 1 - pnorm(z_cons))
  })
  tab <- do.call(rbind, rows)
  # The breaking point is the first gamma whose conservative p passes 0.05.
  broke <- tab$gamma[which(tab$p_upper > 0.05)]
  list(breaking_gamma = if (length(broke)) broke[1] else NA_real_,
       table = tab)
}

# ---------------------------------------------------------------------------
# Before and after with a comparison group (difference in differences)
# ---------------------------------------------------------------------------

# The estimate is the classic four means calculation. The uncertainty comes
# from resampling whole units rather than rows, because repeated readings
# from one site move together and treating them as independent would shrink
# the interval below what the data supports.
run_did <- function(data, outcome_col, unit_col, time_col, treat_col,
                    adopt_time, reps = 999, seed = 20260720) {
  d <- data %>%
    filter(if_all(all_of(c(outcome_col, unit_col, time_col, treat_col)), ~ !is.na(.x))) %>%
    mutate(post = as.integer(.data[[time_col]] >= adopt_time))
  dropped_missing <- nrow(data) - nrow(d)

  cell_means <- d %>%
    group_by(g = .data[[treat_col]], post) %>%
    summarize(m = mean(.data[[outcome_col]]), .groups = "drop")
  pick <- function(g, p) cell_means$m[cell_means$g == g & cell_means$post == p]
  did <- (pick(1, 1) - pick(1, 0)) - (pick(0, 1) - pick(0, 0))

  units <- d %>% distinct(.data[[unit_col]], .data[[treat_col]])
  names(units) <- c("unit", "g")

  one_rep <- function() {
    # Resampling within each group keeps both groups present in every
    # replicate; a replicate with no comparison units says nothing.
    su <- units %>% group_by(g) %>% slice_sample(prop = 1, replace = TRUE) %>% ungroup()
    dd <- su %>%
      mutate(copy = row_number()) %>%
      inner_join(d, by = c(unit = unit_col), relationship = "many-to-many")
    cm <- dd %>% group_by(g, post) %>%
      summarize(m = mean(.data[[outcome_col]]), .groups = "drop")
    pk <- function(gg, pp) cm$m[cm$g == gg & cm$post == pp]
    (pk(1, 1) - pk(1, 0)) - (pk(0, 1) - pk(0, 0))
  }
  set.seed(seed)
  boots <- map_dbl(seq_len(reps), ~ one_rep())
  ci <- unname(quantile(boots, c(0.025, 0.975)))
  p_boot <- 2 * min(mean(boots <= 0), mean(boots >= 0))

  # Parallel paths check: within the periods before adoption, do the two
  # groups drift apart already? A slope gap there undercuts the design.
  pre <- d %>% filter(post == 0)
  trend_p <- NA_real_
  trend_gap <- NA_real_
  if (n_distinct(pre[[time_col]]) >= 2) {
    tf <- lm(reformulate(sprintf("%s * %s", time_col, treat_col), response = outcome_col),
             data = pre)
    co <- summary(tf)$coefficients
    ix <- grep(":", rownames(co))
    if (length(ix) == 1) {
      trend_gap <- co[ix, 1]
      trend_p <- co[ix, 4]
    }
  }

  # Placebo: run the same calculation entirely inside the pre period with a
  # made up adoption point. A real looking effect where none can exist is a
  # warning about the comparison group.
  pre_times <- sort(unique(pre[[time_col]]))
  placebo <- NULL
  if (length(pre_times) >= 4) {
    fake <- pre_times[ceiling(length(pre_times) / 2) + 1]
    pp <- pre %>% mutate(post = as.integer(.data[[time_col]] >= fake))
    pm <- pp %>% group_by(g = .data[[treat_col]], post) %>%
      summarize(m = mean(.data[[outcome_col]]), .groups = "drop")
    pkk <- function(gg, ppp) pm$m[pm$g == gg & pm$post == ppp]
    placebo <- list(time = fake,
                    est = (pkk(1, 1) - pkk(1, 0)) - (pkk(0, 1) - pkk(0, 0)))
  }

  series <- d %>%
    group_by(t = .data[[time_col]], g = .data[[treat_col]]) %>%
    summarize(m = mean(.data[[outcome_col]]), .groups = "drop") %>%
    arrange(t, g)

  # Event study view: the gap between the two groups at each period, measured
  # against the last period before adoption. A flat line before adoption is
  # the parallel paths assumption made visible; a drop after is the effect.
  gaps <- series %>%
    tidyr::pivot_wider(names_from = g, values_from = m, names_prefix = "g") %>%
    mutate(gap = g1 - g0)
  ref_time <- max(gaps$t[gaps$t < adopt_time])
  ref_gap <- gaps$gap[gaps$t == ref_time]
  event <- gaps %>%
    mutate(rel = gap - ref_gap,
           period = t - adopt_time) %>%
    select(t, period, rel)

  list(
    design = "did",
    n_total = nrow(data), n_used = nrow(d), dropped_missing = dropped_missing,
    n_units_treated = sum(units$g == 1), n_units_control = sum(units$g == 0),
    n_pre = sum(unique(d[[time_col]]) < adopt_time),
    n_post = sum(unique(d[[time_col]]) >= adopt_time),
    adopt_time = adopt_time, did = did, ci = ci, p = p_boot,
    trend_p = trend_p, trend_gap = trend_gap, placebo = placebo,
    series = series, event = event, data = d, outcome_col = outcome_col,
    unit_col = unit_col, time_col = time_col, treat_col = treat_col
  )
}

# ---------------------------------------------------------------------------
# Wire format
# ---------------------------------------------------------------------------

# Shiny serializes data frames column wise on the websocket, which is not
# the shape the renderer reads. Converting to row records here keeps the
# browser code free of unpacking logic.
df_rows <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

wire_matching <- function(res) {
  list(
    kind = "matching",
    balance = df_rows(res$balance),
    hist = res$hist,
    support = res$support,
    sensitivity = if (!is.null(res$sensitivity$table))
      df_rows(res$sensitivity$table) else NULL,
    breaking_gamma = res$sensitivity$breaking_gamma,
    n_pairs = res$n_pairs, unmatched = res$unmatched
  )
}

wire_did <- function(res) {
  list(
    kind = "did",
    series = df_rows(res$series),
    event = df_rows(res$event),
    adopt = res$adopt_time,
    placebo = res$placebo
  )
}

# ---------------------------------------------------------------------------
# Interrupted time series
# ---------------------------------------------------------------------------

# A single series observed over many periods, with a program starting at a
# known time. Segmented regression fits one line before and one after, and
# reports two effects: the immediate change in level at the interruption, and
# the change in the slope thereafter. The counterfactual is the pre period
# trend carried forward, and the effect is the distance between the series and
# that carried forward line. Standard errors are ordinary least squares here,
# so the result carries a caveat about serial correlation, which time series
# residuals usually show and which a fuller model would correct.
run_its <- function(data, outcome_col, time_col, intervention_time) {
  d <- data %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(.data[[time_col]])) %>%
    arrange(.data[[time_col]])
  t <- d[[time_col]]
  y <- d[[outcome_col]]

  post <- as.integer(t >= intervention_time)
  # Time since the interruption, zero at the interruption point, so the level
  # coefficient reads as the jump exactly at the interruption.
  since <- ifelse(post == 1, t - intervention_time, 0)

  fit <- lm(y ~ t + post + since)
  co <- summary(fit)$coefficients
  level <- co["post", "Estimate"]; level_se <- co["post", "Std. Error"]
  slope_change <- co["since", "Estimate"]; slope_se <- co["since", "Std. Error"]
  pre_slope <- co["t", "Estimate"]
  df_resid <- fit$df.residual
  tcrit <- qt(0.975, df_resid)

  # The counterfactual line: baseline intercept plus pre trend, evaluated at
  # every time, ignoring the two post terms.
  b0 <- co["(Intercept)", "Estimate"]
  counterfactual <- b0 + pre_slope * t
  fitted_vals <- as.numeric(fitted(fit))

  series <- data.frame(t = t, y = y, fit = fitted_vals, cf = counterfactual,
                       post = post)

  list(
    design = "its",
    n_total = nrow(data), n_used = nrow(d),
    n_pre = sum(post == 0), n_post = sum(post == 1),
    intervention_time = intervention_time,
    level = level, se = level_se,
    ci = c(level - tcrit * level_se, level + tcrit * level_se),
    p = 2 * pt(-abs(level / level_se), df = df_resid),
    slope_change = slope_change, slope_se = slope_se,
    slope_p = 2 * pt(-abs(slope_change / slope_se), df = df_resid),
    pre_slope = pre_slope, series = series,
    naive = mean(y[post == 1]) - mean(y[post == 0]),
    outcome_col = outcome_col, time_col = time_col
  )
}

# ---------------------------------------------------------------------------
# Regression discontinuity
# ---------------------------------------------------------------------------

# Assignment by a cutoff on a running variable: everyone at or above a
# threshold receives the program, everyone below does not. Near the cutoff the
# two sides are alike except for the program, so the vertical gap between the
# fitted lines at the cutoff estimates the effect for people right at the
# threshold. A triangular kernel gives points nearer the cutoff more weight,
# and only points inside the bandwidth take part.
run_rdd <- function(data, outcome_col, running_col, cutoff, bandwidth = NULL) {
  d <- data %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(.data[[running_col]]))
  x <- d[[running_col]] - cutoff
  y <- d[[outcome_col]]
  above <- as.integer(x >= 0)

  if (is.null(bandwidth)) {
    # A plain rule of thumb bandwidth: enough spread to fit a line on each side
    # without reaching so far that curvature distorts the gap at the cutoff.
    bandwidth <- max(sd(x), diff(range(x)) / 6)
  }
  keep <- abs(x) <= bandwidth
  xk <- x[keep]; yk <- y[keep]; dk <- above[keep]
  w <- pmax(0, 1 - abs(xk) / bandwidth)   # triangular kernel

  fit <- lm(yk ~ xk * dk, weights = w)
  co <- summary(fit)$coefficients
  jump <- co["dk", "Estimate"]; jump_se <- co["dk", "Std. Error"]
  df_resid <- fit$df.residual
  tcrit <- qt(0.975, df_resid)

  # A binned scatter for the figure: average the outcome within evenly spaced
  # slices of the running variable, so the plot reads without thousands of dots.
  nb <- 24
  brks <- seq(min(x), max(x), length.out = nb + 1)
  bin <- cut(x, brks, include.lowest = TRUE)
  binned <- data.frame(x = x, bin = bin) %>%
    mutate(y = y) %>%
    group_by(bin) %>%
    summarize(x = mean(x), y = mean(y), .groups = "drop") %>%
    filter(!is.na(x))

  # Fitted lines on each side within the bandwidth, for drawing.
  gx_lo <- seq(-bandwidth, 0, length.out = 30)
  gx_hi <- seq(0, bandwidth, length.out = 30)
  pred <- function(xx, dd) {
    cf <- coef(fit)
    cf["(Intercept)"] + cf["xk"] * xx + cf["dk"] * dd + cf["xk:dk"] * xx * dd
  }
  line_lo <- data.frame(x = gx_lo, y = pred(gx_lo, 0))
  line_hi <- data.frame(x = gx_hi, y = pred(gx_hi, 1))

  list(
    design = "rdd",
    n_total = nrow(data), n_used = nrow(d), n_in_band = sum(keep),
    cutoff = cutoff, bandwidth = bandwidth,
    jump = jump, se = jump_se,
    ci = c(jump - tcrit * jump_se, jump + tcrit * jump_se),
    p = 2 * pt(-abs(jump / jump_se), df = df_resid),
    binned = binned, line_lo = line_lo, line_hi = line_hi,
    naive = mean(y[above == 1]) - mean(y[above == 0]),
    outcome_col = outcome_col, running_col = running_col
  )
}

wire_its <- function(res) {
  list(kind = "its", series = df_rows(res$series),
       intervention = res$intervention_time)
}

wire_rdd <- function(res) {
  list(kind = "rdd", binned = df_rows(res$binned),
       line_lo = df_rows(res$line_lo), line_hi = df_rows(res$line_hi),
       cutoff = 0)   # the plot works in centered coordinates
}

# ---------------------------------------------------------------------------
# Design detection
# ---------------------------------------------------------------------------

# A best guess at which design a file suits, from the shape of its columns.
# This is a suggestion, never a decision: the caller shows it and lets the user
# pick any design instead. The order of the checks matters, because a file can
# fit more than one and the earlier checks name the more specific structure.
detect_design <- function(df) {
  nm <- names(df)
  n <- nrow(df)
  num <- nm[vapply(df, is.numeric, logical(1))]

  is_binary <- function(col) {
    u <- unique(stats::na.omit(df[[col]]))
    length(u) <= 2 && all(u %in% c(0, 1))
  }
  bins <- num[vapply(num, is_binary, logical(1))]
  cont <- setdiff(num, bins)

  hint <- function(pats) nm[grepl(paste(pats, collapse = "|"), tolower(nm))]
  time_like <- hint(c("time", "month", "week", "period", "day", "year", "quarter", "wave"))
  unit_like <- hint(c("unit", "site", "clinic", "school", "id", "county", "store", "region"))
  run_like  <- hint(c("score", "running", "rating", "index", "income", "age", "gpa", "distance"))

  # A running variable that a binary column splits at a threshold points to a
  # regression discontinuity, the most specific structure to spot.
  for (b in bins) {
    for (c in cont) {
      lo <- suppressWarnings(max(df[[c]][df[[b]] == 0], na.rm = TRUE))
      hi <- suppressWarnings(min(df[[c]][df[[b]] == 1], na.rm = TRUE))
      if (is.finite(lo) && is.finite(hi) && hi >= lo) {
        return(list(design = "rdd",
          reason = sprintf("Values of %s split cleanly at a cutoff on %s, which is the mark of a regression discontinuity.", b, c),
          confidence = "high"))
      }
    }
  }

  # A time column with a repeated unit column and a treatment marker is a
  # comparison group before and after.
  has_time <- length(time_like) > 0
  has_unit <- length(unit_like) > 0
  repeats <- has_unit && any(vapply(unit_like, function(u) {
    any(table(df[[u]]) > 1)
  }, logical(1)))
  if (has_time && repeats && length(bins) > 0) {
    return(list(design = "did",
      reason = "Units are observed over time and a marker separates those that adopt a change, which suits a before and after comparison.",
      confidence = "high"))
  }

  # A time column whose values are mostly one per period is a single series,
  # which suits an interrupted time series.
  if (has_time) {
    tcol <- time_like[1]
    if (length(unique(df[[tcol]])) >= 0.8 * n) {
      return(list(design = "its",
        reason = "One value per time period suggests a single series, which suits an interrupted time series.",
        confidence = "medium"))
    }
  }

  # A binary treatment alongside a column that looks like an instrument, and a
  # separate outcome, points to instrumental variables.
  instr_like <- hint(c("instrument", "distance", "lottery", "assigned", "encouragement", "draft"))
  if (length(bins) > 0 && length(instr_like) > 0) {
    return(list(design = "iv",
      reason = sprintf("A treatment marker with a column that reads like an instrument (%s) suits instrumental variables.", instr_like[1]),
      confidence = "medium"))
  }

  # A pair of columns that look like a before and after measure of the same
  # thing, with no group marker, is the one group pretest posttest design.
  pre_like <- hint(c("before", "pre_", "_pre", "baseline", "t1"))
  post_like <- hint(c("after", "post_", "_post", "followup", "t2"))
  if (length(bins) == 0 && length(pre_like) > 0 && length(post_like) > 0) {
    return(list(design = "prepost",
      reason = "A before measure and an after measure with no comparison group is the one group pretest posttest design.",
      confidence = "medium"))
  }

  # A treatment marker with a single outcome and nothing else to adjust with
  # is the posttest only design with nonequivalent groups.
  if (length(bins) > 0 && length(cont) == 1) {
    return(list(design = "posttest_only",
      reason = "A treatment marker and a single outcome, with no pretest and no traits to match on, is the posttest only design with nonequivalent groups.",
      confidence = "medium"))
  }

  # A single numeric column and nothing else is the one group posttest only
  # design, the weakest in the book.
  if (length(num) == 1 && ncol(df) <= 2) {
    return(list(design = "oneshot",
      reason = "One outcome column with no group marker and no time column is the one group posttest only design, which identifies no effect.",
      confidence = "high"))
  }

  # A binary treatment with other traits to match on suggests matched comparison.
  if (length(bins) > 0 && length(cont) >= 1) {
    return(list(design = "matching",
      reason = "A treatment marker with background traits suits a matched comparison.",
      confidence = "medium"))
  }

  list(design = "matching",
    reason = "No clear structure stood out, so the most general design is offered as a starting point.",
    confidence = "low")
}

# ---------------------------------------------------------------------------
# Instrumental variables, two stage least squares
# ---------------------------------------------------------------------------

# When take up of a program is tangled with the outcome through something
# unmeasured, an instrument offers a way out: a variable that shifts who takes
# the program but has no path to the outcome except through it. Two stage least
# squares uses only the part of take up that the instrument explains, setting
# aside the confounded remainder. The estimate is worthless if the instrument
# barely moves take up, so the first stage F statistic is computed and gated:
# below ten, by the Staiger and Stock rule of thumb, the instrument is weak and
# the result is not to be trusted.
run_iv <- function(data, outcome_col, treat_col, instrument_col, controls = character(0)) {
  d <- data %>%
    filter(if_all(all_of(c(outcome_col, treat_col, instrument_col, controls)),
                  ~ !is.na(.x)))
  n <- nrow(d)
  y <- d[[outcome_col]]
  D <- d[[treat_col]]
  Z <- as.matrix(d[, instrument_col, drop = FALSE])
  Xc <- if (length(controls)) as.matrix(d[, controls, drop = FALSE]) else NULL

  one <- rep(1, n)
  W <- cbind(one, Xc)                       # exogenous columns, with intercept
  Zmat <- cbind(W, Z)                       # full instrument set
  Xmat <- cbind(W, D)                       # structural regressors
  k <- ncol(Xmat)

  # Projection onto the instrument space, the heart of two stage least squares.
  Pz <- Zmat %*% solve(crossprod(Zmat)) %*% t(Zmat)
  XtPzX <- t(Xmat) %*% Pz %*% Xmat
  beta <- solve(XtPzX, t(Xmat) %*% Pz %*% y)
  # Residuals use the actual treatment, not its projection, as theory requires.
  resid <- as.numeric(y - Xmat %*% beta)
  sigma2 <- sum(resid^2) / (n - k)
  vcov <- sigma2 * solve(XtPzX)
  b_iv <- beta[k]; se_iv <- sqrt(vcov[k, k])

  # First stage: regress take up on the full instrument set, then on the
  # exogenous columns alone, and compare. The F for the excluded instrument is
  # the strength of the instrument.
  fs_full <- lm.fit(Zmat, D); fs_rest <- lm.fit(W, D)
  rss_u <- sum(fs_full$residuals^2); rss_r <- sum(fs_rest$residuals^2)
  q <- ncol(Z)
  first_F <- ((rss_r - rss_u) / q) / (rss_u / (n - ncol(Zmat)))

  # Ordinary least squares for contrast: the biased estimate the instrument fixes.
  ols <- lm.fit(Xmat, y)
  b_ols <- ols$coefficients[k]

  tcrit <- qt(0.975, n - k)
  # Points for a reduced form picture: how the outcome and take up each move
  # with the instrument, averaged within instrument levels.
  iv_levels <- sort(unique(round(Z[, 1], 3)))
  by_z <- data.frame(z = Z[, 1], y = y, d = D) %>%
    mutate(zbin = cut(z, breaks = min(24, length(iv_levels)))) %>%
    group_by(zbin) %>%
    summarize(z = mean(z), y = mean(y), d = mean(d), .groups = "drop") %>%
    filter(!is.na(z))

  list(
    design = "iv",
    n_total = nrow(data), n_used = n,
    late = as.numeric(b_iv), se = se_iv,
    ci = c(b_iv - tcrit * se_iv, b_iv + tcrit * se_iv),
    p = 2 * pt(-abs(b_iv / se_iv), df = n - k),
    first_F = first_F, weak = first_F < 10,
    ols = as.numeric(b_ols),
    naive = as.numeric(b_ols),
    by_z = by_z, outcome_col = outcome_col, treat_col = treat_col,
    instrument_col = instrument_col, controls = controls
  )
}

wire_iv <- function(res) {
  list(kind = "iv", by_z = df_rows(res$by_z),
       first_F = res$first_F, weak = res$weak)
}

# ---------------------------------------------------------------------------
# Designs from the weaker end of the Shadish, Cook, and Campbell taxonomy
# ---------------------------------------------------------------------------

# One group, one measure after the program, nothing to compare against. The
# book keeps this design mostly to show why it fails: with no pretest and no
# control, there is no counterfactual, so no effect can be identified. The app
# includes it for the same reason, reporting only the treated average and
# saying plainly that a causal claim is out of reach.
run_oneshot <- function(data, outcome_col) {
  y <- data[[outcome_col]][!is.na(data[[outcome_col]])]
  list(design = "oneshot", n_used = length(y),
       mean = mean(y), sd = stats::sd(y),
       identified = FALSE, outcome_col = outcome_col)
}

# One group measured before and after the program. The pre to post change is
# easy to compute, but history, maturation, and regression to the mean all sit
# inside it, so the design cannot say the program caused the change. The paired
# difference is reported with those threats named.
run_prepost <- function(data, pre_col, post_col) {
  d <- data[!is.na(data[[pre_col]]) & !is.na(data[[post_col]]), ]
  diff <- d[[post_col]] - d[[pre_col]]
  n <- length(diff); est <- mean(diff); se <- stats::sd(diff) / sqrt(n)
  tcrit <- qt(0.975, n - 1)
  list(design = "prepost", n_used = n, est = est, se = se,
       ci = c(est - tcrit * se, est + tcrit * se),
       p = 2 * pt(-abs(est / se), df = n - 1),
       pre_mean = mean(d[[pre_col]]), post_mean = mean(d[[post_col]]),
       identified = FALSE, pre_col = pre_col, post_col = post_col)
}

# A treated group and a comparison group, each measured once after the program,
# with no pretest. The difference in means is easy, but with no pretest there is
# no way to check whether the groups started alike, so selection is wide open.
run_posttest_only <- function(data, outcome_col, treat_col) {
  d <- data[!is.na(data[[outcome_col]]) & !is.na(data[[treat_col]]), ]
  yt <- d[[outcome_col]][d[[treat_col]] == 1]
  yc <- d[[outcome_col]][d[[treat_col]] == 0]
  est <- mean(yt) - mean(yc)
  se <- sqrt(stats::var(yt) / length(yt) + stats::var(yc) / length(yc))
  dfw <- length(yt) + length(yc) - 2
  tcrit <- qt(0.975, dfw)
  list(design = "posttest_only", n_used = nrow(d),
       n_treated = length(yt), n_control = length(yc),
       est = est, se = se, ci = c(est - tcrit * se, est + tcrit * se),
       p = 2 * pt(-abs(est / se), df = dfw),
       treated_mean = mean(yt), control_mean = mean(yc),
       identified = FALSE, outcome_col = outcome_col, treat_col = treat_col)
}

wire_prepost <- function(res) {
  list(kind = "prepost", pre = res$pre_mean, post = res$post_mean, est = res$est)
}
wire_posttest_only <- function(res) {
  list(kind = "posttest_only", treated = res$treated_mean,
       control = res$control_mean, est = res$est)
}
wire_oneshot <- function(res) list(kind = "oneshot", mean = res$mean)
