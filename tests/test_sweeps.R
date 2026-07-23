# test_sweeps.R
# The writing rules, enforced as a build gate. Banned words, dashes, and
# contractions are searched across the R, the JavaScript, the CSS, and the
# runtime generated prose. Runtime prose matters most, because a sentence the
# interpretation engine assembles at run time is the one a reviewer reads.

fails <- 0

report <- function(label, hits) {
  if (length(hits) > 0) {
    fails <<- fails + length(hits)
    cat(sprintf("FAIL  %s:\n", label))
    for (h in hits) cat("      ", h, "\n")
  } else {
    cat(sprintf("ok    %s\n", label))
  }
}

src_files <- c("app.R", "R/causal_math.R", "R/samples.R", "R/interpret.R", "R/refute.R",
               "R/export.R", "R/modals.R",
               "www/plots.js", "www/notation.js",
               "www/app.css")
all_text <- unlist(lapply(src_files, function(f) readLines(f, warn = FALSE)))

# ---- Banned words. The full list, matched as whole words, case insensitive,
#      catching common inflections through a suffix group. ----
banned <- c("actionable", "aim", "align", "bolster", "commendable", "delve",
            "drawn", "enable", "encompass", "enhance", "ensure", "equip",
            "esteemed", "facilitate", "foster", "friendly", "functionality",
            "grasp", "guarantee", "hone", "influence", "instrumental",
            "intersection", "intricate", "invaluable", "journey", "landscape",
            "leverage", "maximize", "meticulous", "multifaceted", "nuance",
            "nuanced", "passionate", "perspective", "pivotal", "plethora",
            "realm", "rigor", "rigorous", "robust", "sacrifice", "seamlessly",
            "showcasing", "strengthen", "strive", "synergy", "techniques",
            "transformative", "translate", "tweak", "utilize", "vital",
            "wishlist")
# CSS align properties are not the banned verb, so they are exempted. This
# covers align-items, text-align, and align-self as they appear in the sheet.
scan_text <- all_text
for (css_prop in c("align-items", "text-align", "align-self", "align-content")) {
  scan_text <- gsub(css_prop, "XXCSS", scan_text, fixed = TRUE)
}
# "Instrumental variables" is the fixed proper name of an econometric method,
# not the banned adjective, so the phrase is exempted while a loose use of the
# word on its own would still be caught.
for (term in c("instrumental variables", "Instrumental variables",
               "instrumental variable")) {
  scan_text <- gsub(term, "XXIVMETHOD", scan_text, fixed = TRUE)
}
# The word "aim" is too short and collides with real substrings; match it and
# other short ones only as standalone words.
banned_hits <- character(0)
for (w in banned) {
  pat <- paste0("\\b", w, "(s|es|ed|ing|d|ment)?\\b")
  found <- grep(pat, scan_text, ignore.case = TRUE, value = TRUE)
  if (length(found) > 0) {
    banned_hits <- c(banned_hits, paste0("[", w, "] ", trimws(substr(found, 1, 70))))
  }
}
report("banned words in source", banned_hits)

# ---- Dashes. No em or en dash anywhere. Hyphen minus is fine. ----
dash_hits <- grep("\u2014|\u2013", all_text, value = TRUE)
report("em or en dashes in source", trimws(substr(dash_hits, 1, 70)))

# ---- Contractions in prose. The apostrophe rule already keeps them out of
#      the CSS string; this checks the R and JS text a user actually sees. ----
contraction_pat <- "\\b(it's|don't|can't|won't|you're|we're|they're|isn't|aren't|doesn't|didn't|hasn't|haven't|wouldn't|couldn't|shouldn't|that's|there's|what's|let's|i'm|i've|we've|you've)\\b"
contraction_hits <- grep(contraction_pat, all_text, ignore.case = TRUE, value = TRUE)
report("contractions in source", trimws(substr(contraction_hits, 1, 70)))

# ---- Runtime prose. Assemble every interpretation string the engine can
#      produce and sweep those too, since they never sit in a source line. ----
suppressMessages({
  library(dplyr)
  source("R/causal_math.R"); source("R/samples.R"); source("R/interpret.R")
})
tr <- make_training_sample()
mres <- run_matching(tr, "assessment", "enrolled",
                     c("tenure_years", "prior_score", "age", "remote"))
po <- make_policy_sample()
dres <- run_did(po, "wait_minutes", "site_id", "month", "adopts",
                adopt_time = 13, reps = 199)
ires <- run_its(make_series_sample(), "daily_visits", "week", 27)
rres <- run_rdd(make_cutoff_sample(), "gpa_next_year", "entrance_score", 60)
vres <- run_iv(make_instrument_sample(), "earnings", "enrolled", "distance_miles")
im <- interpret_matching(mres); id <- interpret_did(dres)
ii <- interpret_its(ires); ir <- interpret_rdd(rres); iv <- interpret_iv(vres)
gather <- function(x) c(x$headline, vapply(x$cards, function(c) c$body, character(1)), x$caveats)
runtime_prose <- c(
  gather(im), gather(id), gather(ii), gather(ir), gather(iv),
  naive_contrast(mres)$line, naive_contrast(dres)$line,
  naive_contrast(ires)$line, naive_contrast(rres)$line, naive_contrast(vres)$line,
  identification_check(mres)$message, identification_check(dres)$message,
  identification_check(vres)$message
)
# Exempt the method name here as well, so the word instrument on its own stays
# checked while the fixed term does not trip the gate.
for (term in c("instrumental variables", "instrumental variable")) {
  runtime_prose <- gsub(term, "XXIVMETHOD", runtime_prose, ignore.case = TRUE)
}

rt_banned <- character(0)
for (w in banned) {
  pat <- paste0("\\b", w, "(s|es|ed|ing|d|ment)?\\b")
  if (any(grepl(pat, runtime_prose, ignore.case = TRUE)))
    rt_banned <- c(rt_banned, w)
}
report("banned words in runtime prose", rt_banned)
report("dashes in runtime prose",
       grep("\u2014|\u2013", runtime_prose, value = TRUE))
report("contractions in runtime prose",
       grep(contraction_pat, runtime_prose, ignore.case = TRUE, value = TRUE))

cat(sprintf("\nSweeps: %d failures\n", fails))
if (fails > 0) quit(status = 1)
