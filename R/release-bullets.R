# Extra checklist items for `usethis::use_release_issue()`.
#
# usethis finds this by name in the package namespace and appends one
# checkbox per element, after its own "Prepare for release" bullets and before
# "Submit to CRAN". Its defaults already cover the generic steps -- NEWS,
# urlchecker, build_readme, check(remote = TRUE, manual = TRUE),
# check_win_devel, revdepcheck, submit_cran, and post-acceptance
# use_github_release() / use_dev_version(). Everything below is what THIS
# package does that usethis cannot know about, reconstructed from the 2.0.8,
# 2.0.9 and 2.1.1 release cycles.
#
# Not exported, and deliberately undocumented: it exists for the maintainer's
# tooling, not for users.
release_bullets <- function() {
  c(
    # -- house steps usethis does not know about ----------------------------
    "Spell check: `spelling::spell_check_package()`, then `spelling::update_wordlist()`",
    "Re-document: `devtools::document()` (check `man/` and `NAMESPACE` are clean in `git status`)",
    "Check `DESCRIPTION` has no `Remotes:` or `Additional_repositories:` left over -- both are CRAN NOTEs",
    "Bump `Date:` in `DESCRIPTION` to the submission date",
    "Confirm the no-suggests condition: `_R_CHECK_DEPENDS_ONLY_=true R CMD check`",

    # -- the check matrix this package reports in cran-comments -------------
    "win-builder x3: `devtools::check_win_release()`, `check_win_oldrelease()`, `check_win_devel()`",
    "mac-builder x2: `devtools::check_mac_release()` and `devtools::check_mac_devel()`. These have failed with HTTP 502 in the past; that is a mac.r-project.org outage, not a devtools bug (r-lib/devtools#2612, closed same-day as server-side), so there is no open issue to watch -- just retry. If it is genuinely down, say so in cran-comments rather than omitting the rows",
    "Update `cran-comments.md` Test environments: one line per platform, listing the R versions checked, plus a line naming which legs ran without the suggested packages. This is the simplified form deliberately adopted for 2.1.3 -- the older grouped Previous/Current/Development table meant hand-extracting exact R-devel build strings (e.g. `R-devel (2026-06-21 r90185)`) out of the CI logs for every leg, which is not worth the effort. Give the exact devel string where you already have it and leave it plain `R-devel` where you do not",
    "Record the reverse-dependency result in `cran-comments.md`, even when it is a no-op -- say so rather than omitting the section",

    # -- branch mechanics ---------------------------------------------------
    "Merge `development` -> `main` (a merge commit, never a force-push; `main` is protected), and re-run checks on `main`",

    # -- post-acceptance, beyond usethis' own bullets ------------------------
    "After acceptance: tag from `main` and confirm `usethis::use_github_release()` used the current `NEWS.md` section verbatim as the release body -- that is this repo's convention",
    "After acceptance: confirm the `Update CITATION.cff` workflow ran on `main` and committed the refreshed `CITATION.cff`",
    "After acceptance: tell whoever is driving SpaDES.core -- it is queued behind this package in the dependency order (reproducible -> SpaDES.tools -> SpaDES.core -> SpaDES)"
  )
}
