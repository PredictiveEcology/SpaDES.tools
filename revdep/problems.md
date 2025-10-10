# SpaDES.experiment

<details>

* Version: 0.0.2.9005
* GitHub: https://github.com/PredictiveEcology/SpaDES.experiment
* Source code: https://github.com/cran/SpaDES.experiment
* Date/Publication: 2024-11-25 23:07:54 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::revdep_details(, "SpaDES.experiment")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘v-experiments.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘v-experiments.Rmd’
      ...
    Using a GITHUB_PAT environment variable will continue to work, but see: https://usethis.r-lib.org/articles/git-credentials.html
    cannot open URL 'https://raw.githubusercontent.com/ropensci/NLMR/Error in strsplit(gitRefs, "},")[[1]] : subscript out of bounds
    /DESCRIPTION'
    Elapsed time for simInit: 2.46686 secs
    
      When sourcing ‘v-experiments.R’:
    Error: The following repository does not seem to exist: 
    ropensci/NLMR (>= 1.1.1)
    Did you spell the GitHub.com repository, package and or branch/gitRefs correctly?
    Execution halted
    ```

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       22. │                       └─terra (local) .local(x, ...)
       23. │                         └─x@pntr$filenames()
       24. ├─base::stop(`<Rcpp::xc>`)
       25. └─SpaDES.core (local) `<fn>`(`<Rcpp::xc>`)
      ── Failure ('test-experiment2.R:301:3'): simLists tests 1 ──────────────────────
      Expected `identical("hello", setdiff(lsOrig, lsClear))` to be TRUE.
      Differences:
      `actual`:   FALSE
      `expected`: TRUE 
      
      
      [ FAIL 10 | WARN 0 | SKIP 4 | PASS 36 ]
      Error:
      ! Test failures.
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    as.data.table:
      function(x, keep.rownames, ...)
    as.data.table.simLists:
      function(x, vals, objectsFromSim, objectsFromOutputs, ...)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

