
[![Build Status](https://travis-ci.org/PredictiveEcology/SpaDES.tools.svg?branch=master)](https://travis-ci.org/PredictiveEcology/SpaDES.tools)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/spades-tools/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/SpaDES.tools/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/SpaDES.tools?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SpaDES.tools)](https://cran.r-project.org/package=SpaDES.tools)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SpaDES.tools)](https://cran.r-project.org/package=SpaDES.tools)


<img align="right" width="80" vspace="10" hspace="10" src="https://github.com/PredictiveEcology/SpaDES/raw/master/docs/images/SpaDES.png">

# SpaDES.tools

Additional modelling tools for Spatial Discrete Event Simulation (`SpaDES`) module development.

Provides GIS/map utilites and additional modeling tools for developing cellular automata and agent based models in `SpaDES`.

**Website:** [http://SpaDES.PredictiveEcology.org](http://SpaDES.PredictiveEcology.org)

**Wiki:** [https://github.com/PredictiveEcology/SpaDES/wiki](https://github.com/PredictiveEcology/SpaDES/wiki)

## Installation

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)).

### Current stable release

**Install from CRAN:**

```r
install.packages("SpaDES.tools")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.tools", dependencies = TRUE) # stable
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/SpaDES.tools", ref = "development", dependencies = TRUE) # unstable
```
