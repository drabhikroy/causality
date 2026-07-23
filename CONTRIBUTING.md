# Contributing to Causality

Thank you for considering a contribution. This is a noncommercial open source
project under the PolyForm Noncommercial License 1.0.0, and contributions are
accepted under those same terms.

## Before you start

Open an issue describing the change before writing code. A new study design, a
UI change, and a bug fix each want a different conversation, and an issue saves
you from building something that does not fit the design.

One request in particular. This project takes its design names, groupings, and
notation from Shadish, Cook, and Campbell, *Experimental and Quasi-Experimental
Designs for Generalized Causal Inference*. A proposal that departs from that
source should say so plainly in the issue and give a reason, because matching
the standard text is what lets a reader trust the labels.

## Running the app

```r
shiny::runApp(".", launch.browser = TRUE)
```

Requires R 4.1 or later with shiny, dplyr, tidyr, purrr, readr, and jsonlite.

## Running the tests

```sh
Rscript tests/run_tests.R
```

The gate must pass before a pull request is reviewed. It runs five suites:

- **Estimators.** Every design is checked against an effect planted at a known
  size in its sample data. The three designs that identify nothing are checked
  for the opposite property: that they refuse to report an effect and their
  identification check warns.
- **Contrast.** Every color pair that carries meaning is checked against WCAG
  2.2 at 4.5 to 1, in all eight theme and palette combinations.
- **Writing sweeps.** Banned lexemes, em and en dashes, contractions, and
  comment density, across the source and the prose the app generates at run
  time.
- **DOM.** The browser code under jsdom, including the tour, the appearance
  controls, and every design notation diagram.
- **Live boot.** The app starts and serves its page.

The DOM suite needs jsdom, which is scoped to the test folder:

```sh
cd tests && npm install && cd ..
```

## Adding a study design

Designs are registered in one place, but a complete design touches several
files. The checklist below is the whole contract; the estimator suite will fail
until the parts that produce numbers are present, and the sweeps will fail if
the prose is missing.

1. **Estimator** in `R/causal_math.R`. A function `run_yourdesign()` returning a
   list carrying at least `design`, the point estimate, and either `se` or a
   `ci` pair. Set `identified = FALSE` if the design cannot support a causal
   claim, which is how the interface knows to warn rather than report.
2. **Wire format** in the same file. A `wire_yourdesign()` returning only what
   the figure needs, as row records rather than columns.
3. **Reading** in `R/interpret.R`. An `interpret_yourdesign()` returning
   `headline`, `cards`, and `caveats`, plus a branch in `naive_contrast()` and
   one in `identification_check()` naming the assumption the design leans on
   hardest.
4. **Sample** in `R/samples.R`. A generator with a planted effect of a known
   size and a `sample_specs` entry describing its columns. The planted effect
   must be one the estimator can actually recover, since the suite checks it.
5. **Registration** in `app.R`. An entry in `DESIGNS` with its name, notation,
   and description, membership in the right group in `DESIGN_GROUPS`, and
   branches in the run handler, `load_data()`, `design_full_name()`,
   `results_subhead()`, and `upload_hint()`.
6. **Notation** in `www/notation.js`. The design in O and X form, following the
   text. A design with no comparison group must not draw the dashed rule, since
   that rule means two nonequivalent groups.
7. **Figure** in `www/plots.js`. A draw function, an entry in `hostsReady()`,
   and a branch in the render dispatch.
8. **Detection** in `detect_design()`. A rule that recognizes the column shape,
   placed so that more specific rules run before more general ones.
9. **Documentation.** A schema section in `guide_modal()` in `R/modals.R`, and a
   branch in `build_r_code()` in `R/export.R` so the design can be exported as a
   standalone script.

A design that cannot support a causal claim is welcome. Three of the eight are
there precisely to be refused, because the text keeps them for the same reason.
Report what the data hold, then say plainly what is missing.

## Style

The codebase follows a few conventions that the sweeps enforce:

- No em dashes or en dashes anywhere, including comments.
- No contractions in code, comments, or interface text.
- Comments explain why a choice was made, not what the next line does.
- Comment density between ten and twenty five percent.
- Browser code lives inline in `app.R` or in a file under `www/` that the app
  reads and embeds. Nothing is linked as an external script, because a linked
  script that fails to load leaves the interface silently inert.

## Accessibility

Accessibility is not a later pass. Any contribution that touches the interface
should preserve the following, all of which are checked or reviewed:

- Contrast at 4.5 to 1 across all themes and palettes.
- Meaning carried by shape as well as color.
- Keyboard reachability, with visible focus.
- Touch targets of at least 44 pixels.
- Motion that honors the reduced motion preference.
