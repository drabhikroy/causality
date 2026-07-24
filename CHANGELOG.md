# Changelog

Notable changes to this project are recorded here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.7.2] - 2026-07-23

### Fixed

- The Skip control in the walkthrough had no styling of its own, so it rendered
  at the same weight as Back and Next instead of reading as an exit, and the
  action group was not held to the right of the progress dots. Both rules are
  now present.
- The version recorded in `CITATION.cff` disagreed with the released tag.
- The documented dependency list omitted tibble, which the sample generators
  call directly, and did not mention that httr is needed only for the optional
  local model layer.

### Added

- A changelog, and a screenshot of the reading panel on the project page.

## [0.7.1] - 2026-07-23

First public release. Earlier version numbers belong to development and were
never published.

### Added

- Eight quasi-experimental designs, named and grouped as in Shadish, Cook, and
  Campbell, *Experimental and Quasi-Experimental Designs for Generalized Causal
  Inference*: one group posttest only, one group pretest posttest, posttest only
  with nonequivalent groups, matched comparison, untreated control over time,
  interrupted time series, regression discontinuity, and instrumental variables.
- Design notation in the standard O and X form, shown in the chooser and above
  every result. A single series carries no comparison rule, and instrumental
  variables carries a path diagram instead, since it is not a row design.
- A design chooser grouped under the headings the text uses, opened from the
  sidebar and reachable at any point in a session.
- Design detection from the shape of an uploaded file, offered as a suggestion
  with its reasoning and overridable in one click.
- An identification check on every result, testing the assumption its design
  leans on hardest and reporting the verdict beside the estimate. The three
  designs that cannot support a causal claim report what the data hold and then
  state plainly what is missing.
- The plain comparison shown crossed out beside the adjusted estimate, so the
  confounding a design removed is visible rather than asserted.
- Rosenbaum sensitivity bounds for matched comparison, reporting how strong an
  unmeasured confounder would need to be to overturn the result.
- A minimum detectable effect on every result that supports one, reported in
  place of observed power, which only restates the p value.
- Placebo refutation for the designs where reshuffling is fast and clean,
  reporting the share of placebo runs that rival the real effect.
- Export of any result as a written summary or as a standalone base R script
  that reruns the analysis with no dependency on this app.
- An optional local language model, through Ollama, that restates a finished
  reading in plainer words and cannot produce or alter a number.
- A first run walkthrough, reachable afterward from the header and skippable at
  any slide.
- Dark and light modes with four color settings covering standard vision,
  deuteranopia and protanopia, tritanopia, and monochrome.
- Example data for every design under `data/`, one file per design.
- A test gate of five suites: estimators against planted effects, contrast
  across all eight theme and palette combinations, writing sweeps over source
  and generated prose, browser checks under jsdom, and a live boot.

### Notes

All computation stays on the local machine. Estimators are written in base R and
dplyr rather than pulled from specialist packages, so every number traces to
readable code and the app remains self contained. A drawing based DAG engine,
causal machine learning, synthetic controls, staggered adoption event studies,
and package backed clustered variance estimators are natural extensions and are
not part of this release.

[0.7.2]: https://github.com/drabhikroy/causality/releases/tag/v0.7.2
[0.7.1]: https://github.com/drabhikroy/causality/releases/tag/v0.7.1
