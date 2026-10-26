# SpaDES.tools

Follow the contribution rules in CONTRIBUTING.md, imported here:

@CONTRIBUTING.md

## Notes for coding agents

* If the `r-lib` plugin is installed, load the `r-lib:r-package-development` skill before changing package code.
  Where it differs from this file, this file wins:
  * Do not run `air format .`; it would reformat most of the package.
    Format only files you create.
  * Error and warning tests in this package use `expect_error()` and `expect_warning()` with a `regexp`, not `expect_snapshot()`.
* `NEWS.md` has exactly one unreleased heading, `# SpaDES.tools (development version)`.
  Add bullets under its existing `##` headings.
  Never add a numbered heading (`# SpaDES.tools 2.1.3.9016`), and never add a `##` section per change or per version bump.
* `NEWS.md` describes the package as it will be released, not the history of the development branch.
  When you fix or change something added since the last CRAN release, rewrite that feature's bullet rather than adding one.
* Keep NEWS bullets and commit messages short.
  Profiling numbers, benchmarks and the investigation belong in the PR description.
* Branch from `development`, and open PRs into `development`.
  Never commit to `main`.
