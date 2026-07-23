#!/usr/bin/env Rscript
# run_tests.R
# One entry point for every check. Runs the R suites in process, shells out to
# node for the DOM suite, and finishes with a live boot of the app so a broken
# server is caught here rather than on launch. Any failure sets a nonzero exit
# so this can gate a build.

setwd(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
setwd("..")  # tests/ -> project root

status <- 0
run_r <- function(path) {
  cat("\n=== ", path, " ===\n", sep = "")
  code <- system2("Rscript", path, stdout = "", stderr = "")
  if (code != 0) status <<- 1
}

run_r("tests/test_math.R")
run_r("tests/test_contrast.R")
run_r("tests/test_sweeps.R")

cat("\n=== tests/test_dom.js ===\n")
if (system2("node", "tests/test_dom.js", stdout = "", stderr = "") != 0) status <- 1

# ---- Live boot. Start the app on a port, poll until it answers, confirm the
#      page carries the app name, then stop it. This is the smoke test that a
#      static check cannot give. ----
cat("\n=== live server smoke test ===\n")
port <- 7799
proc <- system2("Rscript",
  c("-e", shQuote(sprintf(
    "shiny::runApp('.', port=%d, host='127.0.0.1', launch.browser=FALSE)", port))),
  stdout = FALSE, stderr = FALSE, wait = FALSE)
Sys.sleep(9)
ok <- FALSE
page <- tryCatch(suppressWarnings(
  readLines(url(sprintf("http://127.0.0.1:%d", port)), warn = FALSE)),
  error = function(e) character(0))
if (any(grepl("Causality", page))) {
  ok <- TRUE; cat("ok    app booted and served its page\n")
} else {
  status <- 1; cat("FAIL  app did not serve its page\n")
}
# Stop the background R process holding the port.
system2("pkill", c("-f", sprintf("port=%d", port)), stdout = FALSE, stderr = FALSE)

cat("\n================================\n")
cat(if (status == 0) "ALL SUITES PASSED\n" else "SOME SUITES FAILED\n")
quit(status = status)
