We welcome contributions to `SpaDES.tools`, both as bug reports or package enhancements.

## Report a bug

1. Use the [issue tracker](https://github.com/PredictiveEcology/SpaDES.tools/issues) to report a bug.

2. Please include a minimum [reproducible example](https://stackoverflow.com/q/5963269/1380598) that triggers the bug.

3. Please include the output of `devtools::session_info()`.

## Submit an enhancement

The [`development`](https://github.com/PredictiveEcology/SpaDES.tools/tree/development) branch contains the latest contributions and other code that will appear in the next release.
The [`main`](https://github.com/PredictiveEcology/SpaDES.tools) branch contains the latest release, which is exactly what is currently on [CRAN](https://cran.r-project.org/package=SpaDES.tools).

To make a contribution, create a branch from `development` and send a [pull request](https://docs.github.com/articles/using-pull-requests/) with `development` as the destination branch.
Never target `main`; it only receives releases.
Your PR should pass `R CMD check --as-cran`, which is also run when the PR is submitted.

### Before you open a pull request

* **Tests.** Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R` (testthat 3rd edition).
  Every change in behaviour needs a test.
  Start tests with `testInit()`, and use `needTerraAndRaster()` to cover both raster classes, as the existing tests do.
  Run `devtools::test(filter = "^{name}")` while you work, and `devtools::check()` before you open the PR.
* **Documentation.** Edit the roxygen2 comments, then run `devtools::document()`.
  Never edit `man/*.Rd`, `NAMESPACE`, or the `Collate` field in `DESCRIPTION` by hand: `devtools::document()` regenerates them, along with the Rcpp exports after a change to `src/`.
  Add any new exported topic to `_pkgdown.yml`, and confirm with `pkgdown::check_pkgdown()`.
* **Style.** Don't reformat code you are not changing.
  `air.toml` sets the style for new files, but most existing files are not yet formatted with it, so `air format .` would rewrite them.
  Use the base pipe (`|>`), and Canadian English (`behaviour`, `neighbour`) in documentation and messages.
* **Version.** Increase the last part of `Version` in `DESCRIPTION` by one (e.g., `2.1.3.9015` to `2.1.3.9016`) when your PR changes package code.
* **NEWS.** Add a bullet to `NEWS.md` for each change a user would notice (see below).

### NEWS.md

`NEWS.md` follows the [usethis](https://usethis.r-lib.org/reference/use_news_md.html) and [tidyverse](https://style.tidyverse.org/news.html) conventions.

* All unreleased changes go in one section at the top, headed `# SpaDES.tools (development version)`.
  Never add a heading with a version number, such as `# SpaDES.tools 2.1.3.9016`, even when you increase the version.
  At release, [`usethis::use_version()`](https://usethis.r-lib.org/reference/use_version.html) renames the development heading to the released version.
* Within it, group bullets under `##` headings such as Behaviour changes, New features, Performance, Bug fixes, and Documentation and testing.
* Describe each change as a user of the last CRAN release will see it.
  If you change something that was added since that release, edit its existing bullet rather than adding a new one: a fix to an unreleased feature is not a bug fix for users.
* Keep bullets brief, and name the function early: what changed, and what users need to do differently.
  Benchmarks and implementation details belong in the PR description.
* End each bullet with the issue or PR number: `(#123).`
* Order bullets alphabetically by function name, with bullets that do not name a function first.
* Internal refactoring and small documentation fixes do not need a bullet.

We'll try to review your pull request and provide feedback / merge improvements as quickly as possible.

Thank you!
