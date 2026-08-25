# SpaDES.tools 2.1.3 — CRAN submission status

Handoff note. Written 2026-08-25. Named `TODO-*` so `.Rbuildignore`'s
`^TODO.*\.md$` keeps it out of the tarball.

Follow the `/cran-submission` skill (`~/.claude/skills/cran-submission/`);
`checklist.md` there is authoritative. This records where that checklist stands.

## ⚠️ Read this first: you are not the maintainer

`cre` is **Alex M Chubaty** (`achubaty@for-cast.ca`), reasserted deliberately at
`7f06f55` ("Revert maintainer change: keep Alex M Chubaty as cre"). CRAN only
accepts a submission from the maintainer's address, so **the final submit is
Alex's to make, not ours.** Everything below is prep done so that his part is
one click.

## Context

SpaDES.tools was archived from CRAN on **2026-07-13** solely because
`reproducible` was — CRAN's own comment, visible in the incoming-feasibility
NOTE, reads *"Archived on 2026-07-13 as requires archived package
'reproducible'"*. Nothing was wrong with SpaDES.tools itself.

`reproducible` 3.2.0 was accepted on **2026-08-25** and is back on CRAN, so the
blocker is gone.

Dependency order for the family is
**reproducible → SpaDES.tools → SpaDES.core → SpaDES**. SpaDES.core 3.2.0 is
ready and deliberately **waiting on this package**: until SpaDES.tools is back,
SpaDES.core's check carries a `Suggests or Enhances not in mainstream
repositories: SpaDES.tools` NOTE. See `TODO-cran-submission-status.md` in the
SpaDES.core repo.

## State

Branch **`release/2.1.3`**, commit **`1fb61ff`**, cut from `origin/development`
at `430d295`. **Not pushed.** Working tree clean.

### Why 2.1.3 and not 2.1.2

`CRAN-SUBMISSION` in this repo records **2.1.1** (2026-01-09) as the last
version actually submitted. So 2.1.2 was prepared (`b5f7abb`, 2026-05-16) but
**never submitted**, and the last CRAN version is 2.1.1. That leaves 2.1.2
technically free to reuse — but `origin/development` is already public at
**`2.1.2.9000`**, and `2.1.2.9000 > 2.1.2`. Releasing as 2.1.2 would leave
everyone tracking `@development` (most of the SpaDES ecosystem) refusing to
upgrade. 2.1.3 sorts cleanly above it.

Consequence: 2.1.3 carries **both** the 2.1.2 changes and those made since, so
NEWS keeps both headings and `cran-comments.md` says so explicitly.

### What `1fb61ff` changes

| file | change |
|---|---|
| `DESCRIPTION` | `2.1.2.9000` → `2.1.3`; Date → 2026-08-25 |
| `DESCRIPTION` | **removed** `Remotes: PredictiveEcology/reproducible@development` |
| `NEWS.md` | `# SpaDES.tools (development version)` → `# SpaDES.tools 2.1.3`, plus an `## Enhancements` subhead to match the 2.1.2 section |
| `cran-comments.md` | rewritten for the post-archival resubmission |

The `Remotes` entry was added at `1beaaa9` (2026-08-21) *only* because
`reproducible` had been archived. It is now both unnecessary and a CRAN NOTE
(`Unknown, possibly misspelled, fields in DESCRIPTION: 'Remotes'`), so it goes.
Note this package's `Remotes` is unlike SpaDES.core's: there is exactly one
entry and it points at a package that is now on CRAN, so removing it on the
release branch is safe and does **not** need to wait for the merge to `main`.

## Local `R CMD check --as-cran`: 2 NOTEs, both benign

Run 2026-08-25 on the built `SpaDES.tools_2.1.3.tar.gz`, R 4.5.3, Ubuntu 24.04.
Tests OK, vignettes rebuild OK, C++ compiles clean (g++ 13.3.0).

* `New submission` / `Package was archived on CRAN` — expected for a
  resubmission; CRAN's own comment confirms the cascade.
* `checking HTML version of manual ... Skipping checking math rendering:
  package 'V8' unavailable` — local environment artifact only; will not occur
  on CRAN's machines.

Gone, and worth confirming stay gone: the `Remotes` NOTE, and any
"not in mainstream repositories" NOTE — this package's own dependencies are all
on CRAN.

## What's left

1. **Push `release/2.1.3`** and open a PR into `development`. Not done: this is
   Alex's repo and nothing has been pushed without his say-so.
2. **CI green** on the branch. Note `origin/main` is **43 commits behind**
   `origin/development` — the 2.1.2 release never merged — so the eventual
   merge to `main` is large. `main` is protected; merge, never force-push.
3. **win-builder ×3** (oldrelease, release, devel). FTP is broken; use the HTTP
   form, and note the slot mapping is *not* in visible order:
   `Button1` = release, `Button2` = devel, `Button3` = oldrelease.
   The mac builder is **dead** — `submit.html` serves 200 but
   `/macbuilder/v1/submit` returns 502 on every attempt.
4. **R-hub sanitizers + valgrind** (clang-asan, clang-ubsan, gcc-asan,
   valgrind, rchk). **Do not skip these.** R-hub was skipped for `reproducible`
   on the grounds that it is pure R and the GHA matrix covers more — that
   reasoning does **not** carry over here: this release's headline change is new
   **Rcpp/C++** code in `spread()`/`spread2()`. There is no `RHUB_TOKEN`
   configured on this machine.
5. **revdepcheck** — effectively a no-op against CRAN: the only two CRAN
   reverse dependencies (SpaDES.core, SpaDES) were archived alongside this
   package. Last real run was 2026-05-16 over the 10 ecosystem packages
   (SpaDES.core, SpaDES, LandR, NetLogoR, map, scfmutils, SpaDES.experiment,
   fireSenseUtils, LandWebUtils, usefulFuns) with 0 new problems. Say this
   rather than skipping the step silently.
6. Fill in the `<!-- TODO -->` block in `cran-comments.md` with the actual
   win-builder / GHA / R-hub rows once run. The file currently lists only the
   local Ubuntu check, which is honest but thin.
7. **Alex submits** via `devtools::submit_cran()` (`devtools::release()` is
   deprecated), then **clicks CRAN's confirmation email** — the submission is
   not queued until he does.
8. On acceptance: `CRAN-SUBMISSION` commit, tag `v2.1.3` (note there are no
   `v2.1.x` tags yet; the newest is `v2.0.9`), GitHub release, then "Begin
   2.1.3.9000 development cycle" on `development`.
9. Tell whoever is driving SpaDES.core — it is queued behind this.

## Gotchas

* **`gh` is a snap here.** It cannot read files under `/tmp` *or* under hidden
  directories in `$HOME`. Use `--body "$(cat …)"` or stage in a non-hidden dir.
  It also returns empty output for `gh run view --log`; fetch job logs via
  `gh api repos/OWNER/REPO/actions/jobs/<id>/logs` instead.
* **A `getFromNamespace()` re-export can import a package you never declared.**
  This bit SpaDES.core during this same release round: the re-export binds at
  build time, so the *other* package's function body lands in your namespace and
  `R CMD check` attributes its `::` calls to you — a **WARNING**, so
  CRAN-blocking. It reproduces only against the *installed* package
  (`tools:::.check_packages_used(package = "pkg")`), never the source tree.
  SpaDES.tools has no such re-exports today; keep it that way.
