# SpaDES.tools — release status

Last updated 2026-08-28, after 2.1.3 was accepted to CRAN.

## Done

**2.1.3 is on CRAN.** The 2026-07-13 archival (caused by `reproducible` being
archived, not by anything wrong here) is reversed.

* submitted from `main` at `d915874`; released as
  [v2.1.3](https://github.com/PredictiveEcology/SpaDES.tools/releases/tag/v2.1.3)
* `main` @ `8cb5db6`, `development` @ `c2cbca8` (2.1.3.9000, `main` merged back in)
* release checklist: closed as #122 (27/27)

Pre-submission evidence, all clean: `R CMD check --as-cran`; a depends-only
(`_R_CHECK_DEPENDS_ONLY_=true`) check; CI on `main` — 5 workflows, 11 legs;
win-builder x3; mac-builder (release); gcc-ASAN+UBSAN, valgrind and rchk in the
r-hub containers; revdepcheck with 0 new problems.

**Two things were never verified, and are not claimed in `cran-comments.md`:**

* **clang-ASAN** — the r-hub `clang-asan` container builds against libc++ while
  system GDAL is libstdc++, so `terra` compiles and then fails to load. Not a
  package defect. CRAN runs its own clang flavour.
* **macOS x R-devel** — both mac-builder jobs came back from the *release*
  builder even though `devtools::check_mac_devel()` correctly sends
  `rflavor = "r-devel"`. Server-side; the builder's repo is
  [R-macos/recipes](https://github.com/R-macos/recipes). There is no devtools
  issue to file (r-lib/devtools#2612 was closed as server-side).

## Open

* **#121** — `adj(returnDT = TRUE)` changed its return type (matrix ->
  data.table) below `cutoff.for.data.table = 2e3`, when `x` is a `SpatRaster`.
  Undocumented and untested. A decision about which contract is correct, not a
  defect. `test-downstream` passes, so SpaDES.core does not hit it on any
  exercised path — but an unexercised call site elsewhere would be silently
  wrong. Whichever way it goes, add `returnDT` and `include` x `pairs = FALSE`
  to the equivalence grid in `test-adj.R`; their absence is why it got through.
* **#123** — merge `PredictiveEcology/actions#24`, then re-pin the four
  reusable-workflow callers off `c2b2459`. That is what restores the
  `nosuggests` job labels (the checks list currently shows two jobs both called
  `ubuntu-latest (release)`). **Do not pin to `v0.5`** — it predates
  `e74180e`, the no-suggests fix. **Use the full 40-character SHA** — a short
  one fails workflow validation instantly with no jobs and no explanation.
* Post-CRAN, all upstream in `actions`: scope `CODECOV_TOKEN` to its step;
  split pkgdown into `contents: read` build + `contents: write` deploy jobs;
  move `test-coverage`, `test-downstream` and `pkgdown` onto `setup-r-deps`.
* Repo settings: turn off "Allow GitHub Actions to create and approve pull
  requests" — standing capability nothing uses, unreachable by any
  `permissions:` block.

## Gotchas on this machine

* `~/.Renviron` sets `NOT_CRAN=true`, so `skip_on_cran()` never fires in a local
  check. Use `R_ENVIRON_USER=/dev/null` to test CRAN conditions.
* `gh` is a snap: it cannot read files under `/tmp` or hidden dirs in `$HOME`,
  and **cannot fetch Actions job logs at all** (the API returns empty with no
  redirect). Job metadata works.
* `usethis::use_release_issue()` reads `release_bullets()` from the **installed**
  namespace. Install the current source first or the custom checklist is
  silently dropped.
* `DESCRIPTION` pins `Collate:` — a new `R/*.R` file must be added there
  (`devtools::document()` does it) or installation fails.
