##  SpaDES.tools/R/spades-tools-package.R by Alex M Chubaty and Eliot J B McIntire
##  Copyright (C) 2015-2022 Her Majesty the Queen in Right of Canada,
##   as represented by the Minister of Natural Resources Canada
##

#' Categorized overview of the `SpaDES.tools` package
#'
#' @description
#'
#' \if{html}{\figure{SpaDES.png}{options: width=100 alt="SpaDES logo" style="float: right;"}}
#' \if{latex}{\figure{SpaDES.png}{options: width=0.5in}}
#'
#' @section 1 Spatial spreading/distances methods:
#'
#' Spatial contagion is a key phenomenon for spatially explicit simulation models.
#' Contagion can be modelled using discrete approaches or continuous approaches.
#' Several functions assist with these:
#'
#' \tabular{ll}{
#'   [adj()] \tab Fast neighbour finding, delegating to
#'                          [terra::adjacent()], with a faster torus option\cr
#'   [cir()] \tab Identify pixels in a circle around a
#'                          `SpatVector` object\cr
#'   [directionFromEachPoint()] \tab Fast calculation of direction and
#'                                             distance surfaces\cr
#'   [distanceFromEachPoint()] \tab Fast calculation of distance surfaces\cr
#'   [rings()] \tab Identify rings around focal cells (e.g., buffers and donuts)\cr
#'   [spokes()] \tab Identify pixels along rays radiating out from focal cells\cr
#'   [spread()] \tab Contagious cellular automata\cr
#'   [wrapTorus()] \tab Create a torus from a grid\cr
#' }
#'
#' @section 2 Spatial agent methods:
#'
#' Agents have several methods and functions specific to them:
#'
#' \tabular{ll}{
#'   [crw()] \tab Simple correlated random walk function\cr
#'   [heading()] \tab Determines the heading between points\cr
#'   `quickPlot::makeLines()` \tab Makes `SpatialLines` object for, e.g., drawing arrows\cr
#'   [move()] \tab A meta function that can currently only take "crw"\cr
#'   [specificNumPerPatch()] \tab Initiate a specific number of agents per patch\cr
#' }
#'
#' @section 3 GIS operations:
#'
#' In addition to the GIS operations available in R (chiefly from `terra` and
#' `sf`), we provide the following GIS-related functions:
#' \tabular{ll}{
#'   `quickPlot::equalExtent()` \tab Assess whether a list of extents are all equal\cr
#' }
#'
#' @section 4 Map-reduce - type operations:
#'
#' These functions convert between reduced and mapped representations of the same data.
#' This allows compact representation of, e.g., rasters that have many individual pixels
#' that share identical information.

#' \tabular{ll}{
#'   [rasterizeReduced()] \tab Convert reduced representation to full raster\cr
#' }
#'
#' @section 5 Random Map Generation:
#'
#' It is often useful to build dummy maps with which to build simulation models
#' before all data are available.
#' These dummy maps can later be replaced with actual data maps.
#'
#' \tabular{ll}{
#'   [randomPolygons()] \tab Creates a random polygon with specified number of classes.\cr
#' }
#'
#' See [neutralLandscapeMap()] for tools to generate random landscapes (rasters).
#'
#' @section 6 SELES-type approach to simulation:
#'
#' These functions are essentially skeletons and are not fully implemented.
#' They are intended to make translations from [SELES](http://www.gowlland.ca/).
#' You must know how to use SELES for these to be useful:
#' \tabular{ll}{
#'   [agentLocation()] \tab Agent location\cr
#'   [initiateAgents()] \tab Initiate agents into a `SpatVector`\cr
#'   [numAgents()] \tab Number of agents\cr
#'   [probInit()] \tab Probability of initiating an agent or event\cr
#' }
#'
#' @section 7 Package options:
#'
#' `SpaDES` packages use the following [options()] to configure behaviour:
#'
#' - `spades.lowMemory`: If true, some functions will use more memory
#'   efficient (but slower) algorithms. Default `FALSE`.
#'
"_PACKAGE"

################################################################################
# package imports
# See \url{http://r-pkgs.had.co.nz/namespace.html#imports}

#' @import methods
#' @import utils
#' @importFrom Rcpp evalCpp
#' @useDynLib SpaDES.tools, .registration = TRUE
NULL
