# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |en_CA:en                     |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-06-08                   |

## Packages

|package      |*  |version    |date       |source                                          |
|:------------|:--|:----------|:----------|:-----------------------------------------------|
|quickPlot    |   |0.1.3.9002 |2018-06-08 |Github (PredictiveEcology/quickPlot@d26bb6e)    |
|reproducible |   |0.2.0      |2018-06-08 |Github (PredictiveEcology/reproducible@417a811) |
|SpaDES.tools |   |0.1.1.9010 |2018-06-08 |Github (PredictiveEcology/SpaDES.tools@e06ab12) |

# Check results

2 packages with problems

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|SpaDES.core |0.1.1   |      1|        0|     0|
|SpaDES      |2.0.1   |      0|        3|     4|

## SpaDES.core (0.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘SpaDES.core’ can be installed ... ERROR
Installation failed.
See ‘/home/achubaty/Documents/GitHub/SpaDES/SpaDES.tools/revdep/checks/SpaDES.core.Rcheck/00install.out’ for details.
```

## SpaDES (2.0.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 3 warnings | 4 notes

```
checking S3 generic/method consistency ... WARNING
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.

checking replacement functions ... WARNING
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
The argument of a replacement function which corresponds to the right
hand side must be named ‘value’.

checking for missing documentation entries ... WARNING
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
All user-level objects in a package should have documentation entries.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.

checking dependencies in R code ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key

checking foreign function calls ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
See chapter ‘System and foreign language interfaces’ in the ‘Writing R
Extensions’ manual.

checking R code for possible problems ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key

checking Rd \usage sections ... NOTE
Invalid MIT-MAGIC-COOKIE-1 keyInvalid MIT-MAGIC-COOKIE-1 key
The \usage entries for S3 methods should use the \method markup and not
their full name.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
```

