# Causality

[![License](https://img.shields.io/badge/license-PolyForm%20Noncommercial%201.0.0-blue)](LICENSE)
[![Shiny](https://img.shields.io/badge/Shiny-R-276DC3?logo=r&logoColor=white)](#requirements)
[![Release](https://img.shields.io/badge/release-v0.7.2-blue)](https://github.com/drabhikroy/rank-and-folder/releases/tag/v0.7.2)

Causality is an R Shiny application for quasi-experimental analysis. It
helps researchers examine whether a program, policy, or change may have
caused a difference in an outcome when random assignment is not
available.

A simple comparison can show what happened among people who received an
intervention. Causal analysis asks a harder question: what would have
happened to those same people if the intervention had not occurred?
Causality provides several quasi-experimental designs that help estimate
that missing comparison while making assumptions and limitations
visible.

The app runs entirely on your computer. Nothing is uploaded, and every
statistic is calculated locally using readable code.

![The Causality reading panel, showing a matched comparison
result](docs/screenshot-operator.png)

## How Causality works

A typical workflow:

1.  Load your own CSV file or choose a built-in example.
2.  Select a quasi-experimental design.
3.  Review the assumptions and checks for that design.
4.  Examine the estimated effect and plain-language explanation.
5.  Export the summary and reproducible R script.

Each design includes guidance about what the method can and cannot
identify. When a design cannot support a causal conclusion, the app
states that limitation rather than presenting an unsupported effect.

## Supported quasi-experimental designs

The designs, names, and notation follow Shadish, Cook, and Campbell,
*Experimental and Quasi-Experimental Designs for Generalized Causal
Inference*.

Notation reads left to right in time:

-   **O** represents an observation
-   **X** represents the treatment or intervention
-   A dashed rule between groups indicates groups were not formed
    through random assignment

### Without a control group

-   One group posttest only (`X O`)
-   One group pretest posttest (`O X O`)

These designs do not identify an effect. The app reports what the data
show and explains what information is missing.

### Control group without a pretest

-   Posttest only with nonequivalent groups

The app reports the difference between groups while noting that
selection differences may be part of the result.

### Control group with a pretest

-   Propensity score matching with adjustable caliper, balance table,
    common support plot, Love plot, and Rosenbaum sensitivity bounds
-   Difference in differences with parallel trends testing, placebo
    period checks, and event study output

### Interrupted time series

Segmented regression on a single time series, reporting changes in level
and trend.

### Regression discontinuity

Local linear estimation around a cutoff using a triangular kernel.

### Instrumental variables

Two-stage least squares with a first-stage F statistic check to flag
weak instruments.

## Understanding results

Every result begins with the simple comparison followed by the adjusted
estimate. The difference between them shows the amount of adjustment
made by the design.

Each result includes an identification check based on the main
assumption of the design:

-   Balance for matching
-   Parallel trends for difference in differences
-   Series length for time series
-   Sorting checks at the cutoff for regression discontinuity
-   Instrument strength for instrumental variables

For designs that cannot identify an effect, the app states that
limitation directly.

When appropriate, placebo refutation is available. Assignment is
reshuffled 200 times, the design is rerun, and the proportion of placebo
results that match the observed effect is reported as a
permutation-style p value.

Every result exports two files:

-   A written summary
-   A standalone R script that reruns the analysis

## Running the app

Causality requires R 4.1 or later with:

-   `shiny`
-   `dplyr`
-   `tidyr`
-   `purrr`
-   `readr`
-   `tibble`
-   `jsonlite`

The optional local model feature also uses `httr`, but the app runs
without it.

Install the required packages:

``` r
install.packages(c("shiny", "dplyr", "tidyr", "purrr", "readr", "tibble",
                   "jsonlite"))
```

From the project folder:

``` r
shiny::runApp(".", launch.browser = TRUE)
```

Each design includes a built-in sample with a known true effect so the
calculations can be checked against a fixed answer.

To analyze your own data, choose **Your own CSV**. The app reviews the
columns and suggests a design with an explanation. The suggestion does
not lock the user into that design. All eight designs remain available
through the design selector.

The **Data format** button provides the required columns and an example
row for each design.

## Optional local model support

Causality can connect to Ollama for optional rewriting of completed
explanations.

Every statistic and analysis sentence is produced locally by the
application. The optional local model only receives completed sentences
and is used to make wording easier to read.

The model cannot:

-   Produce a statistical value
-   Change a statistical value
-   Add a finding

If no local model is running, the app displays the original computed
explanation.

## Accessibility

Causality is designed to keep results readable across different visual
settings.

Features include:

-   Dark mode by default
-   Light mode option
-   Color settings for deuteranopia, protanopia, tritanopia, and
    monochrome vision differences
-   WCAG 2.2 contrast checks across themes and palettes
-   Visual information encoded through shape as well as color
-   Visible keyboard focus
-   Reduced motion support
-   Interactive targets meeting accessibility size requirements

## Tests

Run the test suite with:

``` bash
Rscript tests/run_tests.R
```

The test suite includes:

-   Estimator checks that confirm designs recover known effects and
    avoid unsupported identification
-   Contrast checks across themes and palettes
-   Writing checks for source code and generated explanations
-   Browser checks for the interface, appearance controls, and notation
    diagrams
-   Application startup checks

The browser suite requires:

``` bash
cd tests && npm install && cd ..
```

## Project layout

``` text
app.R                 interface, server, and browser code
R/causal_math.R       estimators and calculations
R/interpret.R         explanations, caveats, and identification checks
R/samples.R           built-in samples
R/refute.R            placebo refutation
www/app.css           design system, themes, and colors
www/notation.js       design notation diagrams
www/plots.js          SVG figures
data/                 example CSV files
docs/                 README screenshots
tests/                test suites
```

## Contributing

Issues and pull requests are welcome.

See [CONTRIBUTING.md](CONTRIBUTING.md) for testing requirements, writing
conventions, accessibility checks, and the process for adding study
designs.

## Changes

Release history is available in [CHANGELOG.md](CHANGELOG.md).

## Citing

If this software supports published work, please cite it.

GitHub reads [CITATION.cff](CITATION.cff) and provides a formatted
citation from the repository sidebar.

## Scope

Causality focuses on common quasi-experimental designs and keeps
calculations visible through readable code written with base R and
dplyr.

The current version does not include:

-   Drawing-based DAG tools
-   Causal machine learning methods
-   Synthetic controls
-   Staggered adoption event studies
-   Package-based clustered variance estimators

## License

PolyForm Noncommercial License 1.0.0.

See [LICENSE.md](LICENSE.md).

Required Notice: Copyright Abhik Roy
