#devtools::install_github("r-lib/revdepcheck")
library("revdepcheck")

options(
  repos = c(
    PE = "https://predictiveecology.r-universe.dev",
    CRAN =  paste0("https://", "cloud.", "r-project.", "org")
  )
)

revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = getOption("Ncpus", 8L), timeout = 30*60) ## 30 mins
revdepcheck::revdep_report_cran() ## update cran-comments with this output

### email maintainers of revdep packages (need to edit: `revdep/email.yml`)
#revdep_email(type = "broken") ## will send via gmail
#revdep_email(type = "failed") ## will send via gmail
