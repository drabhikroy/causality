# Required Notice: Copyright Abhik Roy
# PolyForm Noncommercial License 1.0.0. See LICENSE.md.

# samples.R
# Two built in datasets, each with a true effect the app should recover so a
# reviewer can check the arithmetic against a known answer. Both are seeded so
# every launch shows the same numbers.

library(dplyr)
library(tidyr)
library(purrr)

# ---------------------------------------------------------------------------
# Matching sample: a workplace training program
# ---------------------------------------------------------------------------

# A voluntary skills course. People who enroll differ from those who do not
# on traits that also move the outcome, which is exactly the confounding a
# naive comparison would mistake for an effect. The planted true effect on
# the served group is +4.0 points; a raw difference of means will read much
# higher because higher tenure and prior score people enroll more.
make_training_sample <- function(n = 700, seed = 4105) {
  set.seed(seed)
  tenure <- round(rgamma(n, shape = 2, scale = 3), 1)
  prior_score <- round(rnorm(n, 68, 11))
  age <- round(rnorm(n, 40, 9))
  remote <- rbinom(n, 1, 0.35)

  # Enrollment leans on tenure and prior score, so those two traits are the
  # confounders the matching has to balance away.
  lp <- -2.4 + 0.10 * tenure + 0.03 * (prior_score - 68) + 0.15 * remote
  enrolled <- rbinom(n, 1, plogis(lp))

  # Outcome is a later assessment. The program adds a flat 4 points to those
  # who took it, on top of the same traits that drove enrollment.
  outcome <- round(
    52 + 0.55 * prior_score + 0.4 * tenure - 0.05 * age +
      4.0 * enrolled + rnorm(n, 0, 6)
  )

  tibble(
    person_id = sprintf("P%03d", seq_len(n)),
    tenure_years = tenure, prior_score = prior_score, age = age,
    remote = remote, enrolled = enrolled, assessment = outcome
  )
}

# ---------------------------------------------------------------------------
# Difference in differences sample: a policy rollout across sites
# ---------------------------------------------------------------------------

# Twenty clinics report a monthly wait time for two years. Eight adopt a new
# scheduling system at month 13; twelve never do. The planted true effect is
# a 6 minute reduction that begins at adoption. The pre period paths are close
# to parallel by design, so the parallel paths check should pass.
make_policy_sample <- function(n_treated = 8, n_control = 12,
                               months = 24, adopt = 13, seed = 7220) {
  set.seed(seed)
  sites <- tibble(
    site_id = sprintf("Clinic %02d", seq_len(n_treated + n_control)),
    adopts = c(rep(1L, n_treated), rep(0L, n_control)),
    # Each site sits at its own baseline level; treated sites run a touch
    # busier, the kind of fixed gap difference in differences is built to
    # tolerate because it looks at change, not level.
    base = c(rnorm(n_treated, 46, 4), rnorm(n_control, 42, 4))
  )

  grid <- crossing(site_id = sites$site_id, month = seq_len(months)) %>%
    left_join(sites, by = "site_id") %>%
    mutate(
      post = as.integer(month >= adopt),
      # A shared seasonal drift affects everyone, so it cancels in the
      # difference of differences rather than masquerading as an effect.
      season = 3 * sin(2 * pi * month / 12),
      treated_effect = -6 * adopts * post,
      wait_minutes = round(
        base + season + treated_effect + rnorm(n(), 0, 2.5), 1
      )
    ) %>%
    select(site_id, month, adopts, wait_minutes)

  grid
}

# CSV shape hints shown in the upload help, kept next to the generators so
# they never drift from the columns the samples actually carry.
# One series over a year of weeks, with a triage change at week 27 that drops
# the level by 18 visits and leaves the trend unchanged. A little noise keeps
# it from looking synthetic. Seasonality is kept small on purpose, because the
# segmented model the app fits has no seasonal term, and a teaching sample
# should let that model recover the effect it advertises.
make_series_sample <- function(weeks = 52, intervention = 27, seed = 7000) {
  set.seed(seed)
  wk <- 1:weeks
  base <- 240 - 0.4 * wk                       # slow decline over the year
  effect <- ifelse(wk >= intervention, -18, 0) # planted level drop
  visits <- base + effect + rnorm(weeks, 0, 3)
  tibble::tibble(week = wk, daily_visits = round(visits, 1))
}

# Students with an entrance score; a scholarship goes to those at or above 60.
# The outcome jumps by 0.35 grade points right at the cutoff, on top of a
# gentle upward relationship between score and later grades.
make_cutoff_sample <- function(n = 600, cutoff = 60, seed = 9310) {
  set.seed(seed)
  score <- pmin(100, pmax(20, rnorm(n, 62, 14)))
  above <- as.integer(score >= cutoff)
  gpa <- 1.6 + 0.02 * (score - cutoff) + 0.35 * above + rnorm(n, 0, 0.35)
  gpa <- pmin(4, pmax(0, gpa))
  tibble::tibble(entrance_score = round(score, 1),
                 scholarship = above,
                 gpa_next_year = round(gpa, 2))
}

# An instrument sample. A job training program raises later earnings by a true
# eight units, but motivation, which is unobserved, lifts both enrollment and
# earnings, so a plain comparison overstates the gain. Distance to the training
# site shifts enrollment without touching earnings on its own, which makes it a
# usable instrument. Ordinary least squares reads high; the instrument recovers
# the planted effect.
make_instrument_sample <- function(n = 800, seed = 3812) {
  set.seed(seed)
  motivation <- rnorm(n)                          # unobserved confounder
  distance <- runif(n, 0, 10)                     # the instrument, in miles
  # Enrollment rises with motivation and falls with distance.
  enroll_p <- plogis(0.9 * motivation - 0.35 * distance + 0.5)
  enrolled <- rbinom(n, 1, enroll_p)
  # Earnings depend on the program, on motivation, and on noise, but not on
  # distance except through enrollment.
  earnings <- 40 + 8 * enrolled + 6 * motivation + rnorm(n, 0, 6)
  tibble::tibble(
    earnings = round(earnings, 1),
    enrolled = enrolled,
    distance_miles = round(distance, 2)
  )
}

# A one group after only sample: wellbeing ratings with no before and no
# comparison. Nothing here identifies an effect; the mean is all there is.
make_onegroup_sample <- function(n = 200, seed = 6120) {
  set.seed(seed)
  tibble::tibble(wellbeing = round(pmin(100, pmax(0, rnorm(n, 68, 12))), 1))
}

# A one class before and after sample. Scores rise by about six points across
# the term, a rise the design cannot pin on the unit rather than on growth.
make_beforeafter1_sample <- function(n = 30, seed = 4470) {
  set.seed(seed)
  before <- round(pmin(100, pmax(0, rnorm(n, 62, 9))), 1)
  after <- round(pmin(100, pmax(0, before + 6 + rnorm(n, 0, 4))), 1)
  tibble::tibble(score_before = before, score_after = after)
}

# A two group after only sample: workshop attendees rate confidence higher, but
# part of the eight point gap is that more confident people chose to attend.
make_twogroup_sample <- function(n = 200, seed = 8890) {
  set.seed(seed)
  attended <- rbinom(n, 1, 0.5)
  # Attendees are a touch more confident to begin with, which the after only
  # design cannot separate from the workshop itself.
  base <- rnorm(n, 60, 10) + 3 * attended
  confidence <- round(pmin(100, pmax(0, base + 5 * attended + rnorm(n, 0, 6))), 1)
  tibble::tibble(confidence = confidence, attended = attended)
}

sample_specs <- list(
  onegroup = list(
    label = "Wellness sign ups, after only",
    design = "oneshot",
    outcome = "wellbeing",
    blurb = paste(
      "Two hundred people took a wellness class and rated their wellbeing",
      "afterward. There is no before measure and no comparison group, so the",
      "app can report the average but cannot tell you the class caused it.",
      "The design is here to show that limit, straight from the text."
    )
  ),
  beforeafter1 = list(
    label = "Reading scores, one class",
    design = "prepost",
    pre = "score_before", post = "score_after",
    blurb = paste(
      "One class of thirty students, reading scores before and after a new",
      "unit. The scores rose, but with no comparison class the rise could be",
      "ordinary growth over a term rather than the unit. The built in change",
      "is about six points, its cause left open on purpose."
    )
  ),
  twogroup = list(
    label = "Workshop attendees vs others, after only",
    design = "posttest_only",
    outcome = "confidence", treat = "attended",
    blurb = paste(
      "Two hundred workers, some attended a workshop and some did not, with",
      "a confidence rating taken only afterward. With no before measure",
      "there is no way to know the groups started alike. The built in gap is",
      "about eight points, part of which is who chose to attend."
    )
  ),
  training = list(
    label = "Workplace training",
    design = "matching",
    outcome = "assessment", treat = "enrolled",
    covariates = c("tenure_years", "prior_score", "age", "remote"),
    blurb = paste(
      "Six hundred employees. Some enrolled in a voluntary course; the",
      "question is whether the course raised a later assessment. Enrollment",
      "tracks tenure and prior score, so a plain comparison overstates the",
      "gain. The built in true effect on those served is four points."
    )
  ),
  policy = list(
    label = "Clinic scheduling policy",
    design = "did",
    outcome = "wait_minutes", unit = "site_id", time = "month",
    treat = "adopts", adopt = 13,
    blurb = paste(
      "Twenty clinics report a monthly wait time across two years. Eight",
      "adopt a scheduling change at month thirteen. The built in true",
      "effect is a six minute reduction that starts at adoption."
    )
  ),
  series = list(
    label = "Emergency department volume",
    design = "its",
    outcome = "daily_visits", time = "week", intervention = 27,
    blurb = paste(
      "One emergency department reports weekly visits across a year. A",
      "triage change starts at week twenty seven. The built in true effect",
      "is an immediate drop of eighteen visits with no change in trend."
    )
  ),
  cutoff = list(
    label = "Scholarship at a score cutoff",
    design = "rdd",
    outcome = "gpa_next_year", running = "entrance_score", cutoff = 60,
    blurb = paste(
      "Students take an entrance exam. Those scoring sixty or above receive",
      "a scholarship; those below do not. The question is the effect at the",
      "cutoff. The built in true jump is a gain of 0.35 grade points."
    )
  ),
  instrument = list(
    label = "Training with a distance instrument",
    design = "iv",
    outcome = "earnings", treat = "enrolled", instrument = "distance_miles",
    controls = character(0),
    blurb = paste(
      "Eight hundred workers. A training course raises earnings, but",
      "motivation lifts both enrollment and earnings, so a plain comparison",
      "overstates it. Distance to the site shifts enrollment without",
      "touching earnings directly. The built in true effect is eight units."
    )
  )
)
