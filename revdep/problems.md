# SpaDES

Version: 2.0.2

## Newly broken

*   checking whether package ‘SpaDES’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.tools/revdep/checks/SpaDES/new/SpaDES.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SpaDES’ ...
** package ‘SpaDES’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘makeMemoiseable’ is not exported by 'namespace:reproducible'
ERROR: lazy loading failed for package ‘SpaDES’
* removing ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.tools/revdep/checks/SpaDES/new/SpaDES.Rcheck/SpaDES’

```
### CRAN

```
* installing *source* package ‘SpaDES’ ...
** package ‘SpaDES’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
* DONE (SpaDES)

```
# SpaDES.core

Version: 0.2.3

## Newly broken

*   checking whether package ‘SpaDES.core’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.tools/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:RandomFieldsUtils':
    
        RFoptions
    
    The following objects are masked from 'package:base':
    
        abs, acosh, asin, asinh, atan, atan2, atanh, cos, cosh, exp,
        expm1, floor, gamma, lgamma, log, log1p, log2, logb, max, min,
        round, sin, sinh, sqrt, tan, tanh, trunc
    
    
    Attaching package: 'data.table'
    
    The following object is masked from 'package:raster':
    
        shift
    
    Quitting from lines 637-677 (ii-modules.Rmd) 
    Error: processing vignette 'ii-modules.Rmd' failed with diagnostics:
    Column 6 of by= (5) is type 'list', not yet supported
    Execution halted
    ```

## Installation

### Devel

```
* installing *source* package ‘SpaDES.core’ ...
** package ‘SpaDES.core’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘makeMemoiseable’ is not exported by 'namespace:reproducible'
ERROR: lazy loading failed for package ‘SpaDES.core’
* removing ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.tools/revdep/checks/SpaDES.core/new/SpaDES.core.Rcheck/SpaDES.core’

```
### CRAN

```
* installing *source* package ‘SpaDES.core’ ...
** package ‘SpaDES.core’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
Creating a new generic function for ‘citation’ in package ‘SpaDES.core’
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
* DONE (SpaDES.core)

```
