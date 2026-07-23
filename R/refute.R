# Required Notice: Copyright Abhik Roy
# PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# refute.R
# Placebo refutation. If a design is finding a real effect, a fake treatment
# that carries no true effect should turn up nothing. Reshuffling the assignment
# many times and re estimating gives a distribution of placebo effects that
# ought to sit around zero. The share of placebo runs whose effect rivals the
# real one is a permutation style p value: small means the real effect stands
# out from noise, large means it does not. Fast point estimators keep the whole
# battery quick.

run_placebo <- function(design, df, spec, real_effect, B = 200,
                        intervention = NULL, cutoff = NULL) {
  set.seed(20260722)
  pe <- switch(design,
    matching = function(d) {
      d[[spec$treat]] <- sample(d[[spec$treat]])
      tryCatch(run_matching(d, spec$outcome, spec$treat, spec$covariates)$att,
               error = function(e) NA_real_)
    },
    did = function(d) {
      units <- unique(d[[spec$unit]])
      n_ad <- length(unique(d[[spec$unit]][d[[spec$treat]] == 1]))
      fake <- sample(units, n_ad)
      g <- as.integer(d[[spec$unit]] %in% fake)
      post <- as.integer(d[[spec$time]] >= spec$adopt)
      y <- d[[spec$outcome]]
      (mean(y[g == 1 & post == 1]) - mean(y[g == 1 & post == 0])) -
        (mean(y[g == 0 & post == 1]) - mean(y[g == 0 & post == 0]))
    },
    its = function(d) {
      # A placebo interruption inside the pre period only, so the real change at
      # the true interruption cannot leak into the post window and inflate it.
      t <- d[[spec$time]]; y <- d[[spec$outcome]]
      keep <- t < intervention
      t <- t[keep]; y <- y[keep]
      pool <- t[t > min(t) + 2 & t < max(t) - 2]
      if (length(pool) < 2) return(NA_real_)
      iv <- sample(pool, 1)
      post <- as.integer(t >= iv); since <- ifelse(post == 1, t - iv, 0)
      as.numeric(coef(lm(y ~ t + post + since))["post"])
    },
    rdd = function(d) {
      # A placebo cutoff away from the real one, where no jump should appear.
      x0 <- d[[spec$running]]; y <- d[[spec$outcome]]
      pool <- stats::quantile(x0, c(0.25, 0.75))
      fake <- stats::runif(1, pool[1], pool[2])
      x <- x0 - fake; D <- as.integer(x >= 0)
      bw <- max(stats::sd(x), diff(range(x)) / 6); keep <- abs(x) <= bw
      w <- pmax(0, 1 - abs(x[keep]) / bw)
      as.numeric(coef(lm(y[keep] ~ x[keep] * D[keep], weights = w))[3])
    },
    iv = function(d) {
      d[[spec$instrument]] <- sample(d[[spec$instrument]])
      tryCatch(run_iv(d, spec$outcome, spec$treat, spec$instrument,
                      controls = if (is.null(spec$controls)) character(0) else spec$controls)$late,
               error = function(e) NA_real_)
    })

  placebos <- vapply(seq_len(B), function(b) as.numeric(pe(df)), numeric(1))
  placebos <- placebos[is.finite(placebos)]
  pseudo_p <- mean(abs(placebos) >= abs(real_effect))
  list(real = real_effect, placebos = placebos, pseudo_p = pseudo_p,
       n_runs = length(placebos),
       placebo_mean = mean(placebos), placebo_sd = stats::sd(placebos))
}
