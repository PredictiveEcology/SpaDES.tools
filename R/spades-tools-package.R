#
#  SpaDES.tools/R/spades-tools-package.R by Alex M Chubaty and Eliot J B McIntire
#  Copyright (C) 2015-2017 Her Majesty the Queen in Right of Canada,
#   as represented by the Minister of Natural Resources Canada
#

#' Categorized overview of the \code{SpaDES.tools} package
#'
#' @description
#'
#' \if{html}{\figure{inst/figures/SpaDES.png}{options: width=100 alt="SpaDES logo" align="right"}}
#' \if{latex}{\figure{inst/figures/SpaDES.png}{options: width=0.5in}}
#'
#' # TODO: DESCRIPTION NEEDED
#'
#' Bug reports: \url{https://github.com/PredictiveEcology/SpaDES.tools/issues}
#'
#' @name SpaDES_tools-package
#' @aliases SpaDES.tools SpaDES.tools-package spades.tools-package
#' @docType package
#' @author Alex M. Chubaty \email{alexander.chubaty@@canada.ca}
#' @author Eliot J. B. McIntire \email{eliot.mcintire@@canada.ca}
#' @keywords package
#'
#' @section 1 Spatial spreading/distances methods:
#'
#' Spatial contagion is a key phenomenon for spatially explicit simulation models.
#' Contagion can be modelled using discrete approaches or continuous approaches.
#' Several functions assist with these:
#'
#' \tabular{ll}{
#'   \code{\link{adj}} \tab An optimized (i.e., faster) version of \code{\link[raster]{adjacent}}\cr
#'   \code{\link{cir}} \tab Identify pixels in a circle around a \code{\link[sp:SpatialPoints-class]{SpatialPoints*}} object\cr
#'   \code{\link{directionFromEachPoint}} \tab Fast calculation of direction and distance surfaces\cr
#'   \code{\link{distanceFromEachPoint}} \tab Fast calculation of distance surfaces\cr
#'   \code{\link{rings}} \tab Identify rings around focal cells (e.g., buffers and donuts)\cr
#'   \code{\link{spokes}} \tab TO DO: need description\cr
#'   \code{\link{spread}} \tab Contagious cellular automata\cr
#'   \code{\link{wrap}} \tab Create a torus from a grid\cr
#' }
#'
#' @section 2 Spatial agent methods:
#'
#' Agents have several methods and functions specific to them:
#'
#' \tabular{ll}{
#'   \code{\link{crw}} \tab Simple correlated random walk function\cr
#'   \code{\link{heading}} \tab Determines the heading between SpatialPoints*\cr
#'   \code{\link[quickPlot]{makeLines}} \tab Makes \code{SpatialLines} object for, e.g., drawing arrows\cr
#'   \code{\link{move}} \tab A meta function that can currently only take "crw"\cr
#'   \code{\link{specificNumPerPatch}} \tab Initiate a specific number of agents per patch\cr
#' }
#'
#' @section 3 GIS operations:
#'
#' In addition to the vast amount of GIS operations available in R (mostly from
#' contributed packages such as \code{sp}, \code{raster}, \code{maps}, \code{maptools}
#' and many others), we provide the following GIS-related functions:
#' \tabular{ll}{
#'   \code{\link{equalExtent}} \tab Assess whether a list of extents are all equal\cr
#' }
#'
#' @section 4 Map-reduce - type operations:
#'
#' These functions convert between reduced and mapped representations of the same data.
#' This allows compact representation of, e.g., rasters that have many individual pixels
#' that share identical information.

#' \tabular{ll}{
#'   \code{\link{rasterizeReduced}} \tab Convert reduced representation to full raster\cr
#' }
#'
#' @section 5 Random Map Generation:
#'
#' It is often useful to build dummy maps with which to build simulation models before all data are available.
#' These dummy maps can later be replaced with actual data maps.
#'
#' \tabular{ll}{
#'   \code{\link{gaussMap}} \tab Creates a random map using Gaussian random fields\cr
#'   \code{\link{randomPolygons}} \tab Creates a random polygon with specified number of classes\cr
#' }
#'
#' @section 6 SELES-type approach to simulation:
#'
#' These functions are essentially skeletons and are not fully implemented.
#' They are intended to make translations from \href{http://www.gowlland.ca/}{SELES}.
#' You must know how to use SELES for these to be useful:
#' \tabular{ll}{
#'   \code{\link{agentLocation}} \tab Agent location\cr
#'   \code{\link{initiateAgents}} \tab Initiate agents into a \code{SpatialPointsDataFrame}\cr
#'   \code{\link{numAgents}} \tab Number of agents\cr
#'   \code{\link{probInit}} \tab Probability of intiating an agent or event\cr
#'   \code{\link{transitions}} \tab Transition probability\cr
#' }
#'
NULL

################################################################################
# package imports
# See \url{http://r-pkgs.had.co.nz/namespace.html#imports}

#' @import methods
NULL

#' @import utils
NULL

#'@importFrom Rcpp evalCpp
#'@useDynLib SpaDES.tools, .registration = TRUE
NULL
