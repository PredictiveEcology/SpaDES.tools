<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SpaDES.tools)](https://cran.r-project.org/package=SpaDES.tools)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SpaDES.tools)](https://cran.r-project.org/package=SpaDES.tools)
[![R build status](https://github.com/PredictiveEcology/SpaDES.tools/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/SpaDES.tools/actions)
<!-- badges: end -->

<img align="right" width="80" pad="20" src="https://github.com/PredictiveEcology/SpaDES/raw/master/man/figures/SpaDES.png">

# SpaDES.tools

Spatial building blocks for landscape simulation models.

`SpaDES.tools` provides the spatial operations that landscape and agent-based
models need repeatedly and that no general-purpose GIS package supplies:
contagious spread across a raster, neighbourhoods and distance surfaces,
correlated random walks, and random landscape generation. Most functions work
directly on `terra` objects and are written to be called thousands of times
inside a simulation loop, so they favour cell indices and `data.table` output
over repeated raster allocation.

It is one of the [`SpaDES`](https://github.com/PredictiveEcology/SpaDES)
packages, but does not depend on the rest of them — you can use it on its own,
without `SpaDES.core` or a discrete event simulation.

**Website:** <https://SpaDES-tools.PredictiveEcology.org>

## What it is for

### Contagious spread

Fire, disease, dispersal, disturbance — anything that propagates from cell to
neighbouring cell. `spread2()` is the workhorse; `spread3()` handles spread from
multiple sources with distinct kernels.

```r
library(SpaDES.tools)
library(terra)

landscape <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
landscape[] <- 1

## one fire, spreading until it goes out on its own
set.seed(2)
fires <- spread2(landscape, start = 5050, spreadProb = 0.24, asRaster = TRUE)
plot(fires)
```

`spreadProb` can be a single number or a raster of per-cell probabilities, which
is how landscape heterogeneity enters the model.

Spread on a lattice is a percolation process, so this one number matters more
than its size suggests. Roughly, for 8-neighbour spread:

- **below about 0.2**, events die within a handful of cells;
- **between about 0.2 and 0.28**, events are self-stopping but with a real
  chance of getting well past a few cells -- at `0.24` on the 100 x 100 grid
  above, the median event burns a few hundred cells and the largest run to
  several thousand;
- **above about 0.3**, events almost always percolate and fill the grid.

That self-stopping band is usually where you want to be, and it is narrow.
`maxSize`, `exactSize` and `iterations` are there for when you need to pin the
size distribution down rather than let it emerge.

### Neighbourhoods, rings and distances

```r
## the 8 neighbours of a cell, as cell indices
adj(landscape, cells = 5050, directions = 8)

## every cell between 5 and 10 cells away -- a donut around a focal cell
donut <- rings(landscape, loci = 5050, minRadius = 5, maxRadius = 10,
               returnIndices = TRUE)
head(donut)
#>       id initialLocus indices active dists
#> 1:     1         5050    4647  FALSE     5
#> 2:     1         5050    4653  FALSE     5
#> 3:     1         5050    5453  FALSE     5
```

`cir()` draws circles and `spokes()` draws rays from focal points;
`distanceFromEachPoint()` and `directionFromEachPoint()` build distance and
direction surfaces from one set of points to another.

### Agents

```r
## ten agents taking 20 steps of a correlated random walk
set.seed(2)
agents <- vect(cbind(x = runif(10, 0, 100), y = runif(10, 0, 100)))
for (i in 1:20) {
  agents <- crw(agents, stepLength = 2, stddev = 15, lonlat = FALSE)
}
```

`heading()` gives bearings between points, `wrapTorus()` wraps agents that walk
off one edge back onto the other, and `specificNumPerPatch()` seeds a set number
of agents into each patch of a map.

### Random landscapes

Useful for building and testing a model before the real data arrive.

```r
set.seed(1)
habitat <- neutralLandscapeMap(landscape, roughness = 0.6, rand_dev = 10)
patches <- randomPolygons(numTypes = 5, nrow = 50, ncol = 50)
studyArea <- randomStudyArea(size = 1e7)
```

### Raster utilities

`splitRaster()` and `mergeRaster()` tile a raster for parallel processing and
put it back together; `rasterizeReduced()` expands a compact
one-row-per-class table back into a full raster.

For the full categorized list, see `?SpaDES.tools` or the
[reference index](https://SpaDES-tools.PredictiveEcology.org/reference/).

## Installation

`SpaDES.tools` needs **R 4.3 or later**.

Installing from CRAN on Windows or macOS gives you a pre-built binary and needs
nothing else. The notes below apply when you install from source — always the
case on Linux, and on any platform when installing the development version from
GitHub.

**A C++ toolchain**, because part of the package is compiled:

- Windows: [Rtools](https://cran.r-project.org/bin/windows/Rtools/), matching your R version
- macOS: Xcode command line tools (`xcode-select --install`)
- Linux: your distribution's build tools (e.g. `build-essential` on Debian/Ubuntu)

**GDAL, GEOS and PROJ**, because `SpaDES.tools` depends on
[`terra`](https://cran.r-project.org/package=terra). The Windows and macOS
`terra` binaries bundle these; on Linux install them first. On Debian/Ubuntu
that is:

```bash
sudo apt-get install libgdal-dev libgeos-dev libproj-dev libudunits2-dev libsqlite3-dev
```

Everything else is an R package and will be pulled in automatically.

### Current stable release

[![R build status](https://github.com/PredictiveEcology/SpaDES.tools/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/PredictiveEcology/SpaDES.tools/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.tools/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.tools?branch=main)

**From CRAN:**

```r
install.packages("SpaDES.tools")
```

**From GitHub:**

```r
# install.packages("remotes")
remotes::install_github("PredictiveEcology/SpaDES.tools", ref = "main", dependencies = TRUE)
```

### Development version

[![R build status](https://github.com/PredictiveEcology/SpaDES.tools/workflows/R-CMD-check/badge.svg?branch=development)](https://github.com/PredictiveEcology/SpaDES.tools/actions)
[![Codecov test coverage](https://codecov.io/gh/PredictiveEcology/SpaDES.tools/branch/development/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/SpaDES.tools?branch=development)

**From R-universe** — pre-built binaries for Windows and macOS, so no compiler
or system libraries are needed:

```r
install.packages("SpaDES.tools",
                 repos = c("https://predictiveecology.r-universe.dev",
                           "https://cloud.r-project.org"))
```

**From GitHub** (builds from source):

```r
# install.packages("remotes")
remotes::install_github("PredictiveEcology/SpaDES.tools", ref = "development", dependencies = TRUE)
```

## Getting help

- [Reference index](https://SpaDES-tools.PredictiveEcology.org/reference/) — every exported function
- [`?SpaDES.tools`](https://SpaDES-tools.PredictiveEcology.org/reference/SpaDES.tools-package.html) — categorized overview
- [Issue tracker](https://github.com/PredictiveEcology/SpaDES.tools/issues) — bug reports and feature requests
- [SpaDES project site](https://SpaDES.PredictiveEcology.org) — the wider package family

## Contributions

Please see `CONTRIBUTING.md` for information on how to contribute to this project.
