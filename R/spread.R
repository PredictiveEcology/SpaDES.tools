if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", ".I", "dists", "dup", "id", "indices", "initialLocus"))
}


###############################################################################
#' Simulate a spread process on a landscape.
#'
#' This can be used to simulate fires, seed dispersal, calculation of iterative,
#' concentric landscape values (symmetric or asymmetric) and many other things.
#' Essentially, it starts from a collection of cells (\code{loci}) and spreads
#' to neighbours, according to the \code{directions} and \code{spreadProb} arguments.
#' This can become quite general, if \code{spreadProb} is 1 as it will expand
#' from every loci until all cells in the landscape have been covered.
#' With \code{id} set to \code{TRUE}, the resulting map will be classified
#' by the index of the cell where that event propagated from.
#' This can be used to examine things like fire size distributions.
#' \bold{NOTE:} See also \code{\link{spread2}}, which is more robust and can be
#' used to build custom functions.
#' However, under some conditions, this \code{spread} function is faster.
#' The two functions can accomplish many of the same things, and key differences
#' are internal.
#'
#' For large rasters, a combination of \code{lowMemory = TRUE} and
#' \code{returnIndices = TRUE} or \code{returnIndices = 2}
#' will be fastest and use the least amount of memory.
#'
#' This function can be interrupted before all active cells are exhausted if
#' the \code{iterations} value is reached before there are no more active
#' cells to spread into. If this is desired, \code{returnIndices} should be
#' \code{TRUE} and the output of this call can be passed subsequently as an input
#' to this same function. This is intended to be used for situations where external
#' events happen during a spread event, or where one or more arguments to the spread
#' function change before a spread event is completed. For example, if it is
#' desired that the \code{spreadProb} change before a spread event is completed because,
#' for example, a fire is spreading, and a new set of conditions arise due to
#' a change in weather.
#'
#' \code{asymmetry} is currently used to modify the \code{spreadProb} in the following way.
#' First for each active cell, spreadProb is converted into a length 2 numeric of Low and High
#' spread probabilities for that cell:
#' \code{spreadProbsLH <- (spreadProb*2) // (asymmetry+1)*c(1,asymmetry)},
#' whose ratio is equal to
#' \code{asymmetry}.
#' Then, using \code{asymmetryAngle}, the angle between the
#' initial starting point of the event and all potential
#' cells is found. These are converted into a proportion of the angle from
#' \code{-asymmetryAngle}
#' to
#' \code{asymmetryAngle}
#' using:
#' \code{angleQuality <- (cos(angles - rad(asymmetryAngle))+1)/2}
#'
#' These are then converted to multiple spreadProbs by
#' \code{spreadProbs <- lowSpreadProb+(angleQuality * diff(spreadProbsLH))}
#' To maintain an expected \code{spreadProb} that is the same as the asymmetric
#' \code{spreadProbs}, these are then rescaled so that the mean of the
#' asymmetric spreadProbs is always equal to spreadProb at every iteration:
#' \code{spreadProbs <- spreadProbs - diff(c(spreadProb,mean(spreadProbs)))}
#'
#' @section Breaking out of spread events:
#'
#' There are 4 ways for the spread to "stop" spreading. Here, each "event" is defined as
#' all cells that are spawned from a single starting loci. So, one spread call can have
#' multiple spreading "events". The ways outlines below are all acting at all times,
#' i.e., they are not mutually exclusive. Therefore, it is the user's
#' responsibility to make sure the different rules are interacting with
#' each other correctly. Using \code{spreadProb} or \code{maxSize} are computationally
#' fastest, sometimes dramatically so.
#'
#' \tabular{ll}{
#'   \code{spreadProb} \tab Probabilistically, if spreadProb is low enough,
#'                          active spreading events will stop. In practice,
#'                          active spreading events will stop. In practice,
#'                          this number generally should be below 0.3 to actually
#'                          see an event stop\cr
#'   \code{maxSize} \tab This is the number of cells that are "successfully" turned
#'                       on during a spreading event. This can be vectorized, one value
#'                       for each event   \cr
#'   \code{circleMaxRadius} \tab If \code{circle} is TRUE, then this will be the maximum
#'                       radius reached, and then the event will stop. This is
#'                       vectorized, and if length is >1, it will be matched
#'                       in the order of \code{loci}\cr
#'   \code{stopRule} \tab This is a function that can use "landscape", "id", "cells",
#'                       or any named vector passed into \code{spread} in the \code{...}.
#'                       This can take on relatively complex functions.
#'                       Passing in, say, a \code{RasterLayer} to \code{spread}
#'                       can access the individual values on that arbitrary
#'                       \code{RasterLayer} using "cells".
#'                       These will be calculated within all the cells of the individual
#'                       event (equivalent to a "group_by(event)" in \code{dplyr}.
#'                       So, \code{sum(arbitraryRaster[cells])} would sum up all
#'                       the raster values on the \code{arbitraryRaster} raster
#'                       that are overlaid by the individual event.
#'                       This can then be used in a logical statement. See examples.
#'                       To confirm the cause of stopping, the user can assess the values
#'                       after the function has finished.\cr
#' }
#'
#' The spread function does not return the result of this stopRule. If,
#' say, an event has both \code{circleMaxRadius} and \code{stopRule},
#' and it is
#' the \code{circleMaxRadius} that caused the event spreading to stop,
#' there will be no indicator returned from this function that indicates
#' which rule caused the stop.
#'
#' \code{stopRule} has many use cases. One common use case is evaluating
#' a neighbourhood around a focal set of points. This provides,
#' therefore, an alternative to the \code{\link[raster]{buffer}} function or
#' \code{\link[raster]{focal}} function.
#' In both of those cases, the window/buffer size must be an input to the function. Here,
#' the resulting size can be emergent based on the incremental growing and calculating
#' of the \code{landscape} values underlying the spreading event.
#'
#' @section \code{stopRuleBehavior}:
#' This determines how the \code{stopRule} should be implemented. Because
#' spreading occurs outwards in concentric circles or shapes, one cell width at a time, there
#' are 4 possible ways to interpret the logical inequality defined in \code{stopRule}.
#' In order of number of cells included in resulting events, from most cells to fewest cells:
#'
#' \tabular{ll}{
#'   \code{"includeRing"} \tab Will include the entire ring of cells that, as a group,
#'                             caused \code{stopRule} to be \code{TRUE}.\cr
#'   \code{"includePixel"} \tab Working backwards from the entire ring that caused the
#'                              \code{stopRule} to be \code{TRUE}, this will iteratively
#'                              random cells in the final ring
#'                              until the \code{stopRule} is \code{FALSE}. This will add back
#'                              the last removed cell and include it in the return result
#'                              for that event.\cr
#'   \code{"excludePixel"} \tab Like \code{"includePixel"}, but it will not add back the cell
#'                        that causes \code{stopRule} to be \code{TRUE}\cr
#'   \code{"excludeRing"} \tab Analogous to \code{"excludePixel"}, but for the entire final
#'                             ring of cells added. This will exclude the entire ring of cells
#'                             that caused the \code{stopRule} to be \code{TRUE}\cr
#' }
#'
#'
#' @param landscape     A \code{RasterLayer} object. This defines the possible
#'                      locations for spreading events to start and spread into.
#'                      This can also be used as part of \code{stopRule}.
#'
#' @param loci          A vector of locations in \code{landscape}.
#'                      These should be cell indices.
#'                      If user has x and y coordinates, these can be converted
#'                      with \code{\link[raster:cellFrom]{cellFromXY}}.
#'
#' @param spreadProb    Numeric, or \code{RasterLayer}.
#'                      If numeric of length 1, then this is the global probability
#'                      of spreading into each cell from a neighbour.
#'                      If a raster (or a vector of length \code{ncell(landscape)},
#'                      resolution and extent of \code{landscape}), then this will
#'                      be the cell-specific probability. Default is \code{0.23}.
#'                      If a \code{spreadProbLater} is provided, then this is
#'                      only used for the first iteration. Also called "escape
#'                      probability". See section on "Breaking out of spread events".
#'
#' @param persistence   A length 1 probability that an active cell will continue
#'                      to burn, per time step.
#'
#' @param mask          non-\code{NULL}, a \code{RasterLayer} object congruent with
#'                      \code{landscape} whose elements are \code{0,1}, where
#'                      \code{1} indicates "cannot spread to".
#'                      Currently not implemented, but identical behaviour can be
#'                      achieved if \code{spreadProb} has zeros in all unspreadable
#'                      locations.
#'
#' @param maxSize       Numeric. Maximum number of cells for a single or
#'                      all events to be spread. Recycled to match \code{loci} length,
#'                      if it is not as long as \code{loci}.
#'                      See section on \code{Breaking out of spread events}.
#'
#' @param directions    The number of adjacent cells in which to look;
#'                      default is 8 (Queen case). Can only be 4 or 8.
#'
#' @param iterations    Number of iterations to spread.
#'                      Leaving this \code{NULL} allows the spread to continue
#'                      until stops spreading itself (i.e., exhausts itself).
#'
#' @param lowMemory     Deprecated.
#'
#' @param returnIndices Logical or numeric. If \code{1} or \code{TRUE}, will
#'                      return a \code{data.table} with indices and values of
#'                      successful spread events.
#'                      If \code{2}, it will simply return a vector of pixel indices of
#'                      all cells that were touched. This will be the fastest option. If
#'                      \code{FALSE}, then it will return a raster with
#'                      values. See Details.
#'
#' @param returnDistances Logical. Should the function include a column with the
#'                      individual cell distances from the locus where that event
#'                      started. Default is \code{FALSE}. See Details.
#'
#' @param spreadProbLater Numeric, or \code{RasterLayer}. If provided, then this
#'                      will become the spreadProb after the first iteration.
#'                      See Details.
#'
#' @param spreadState   \code{data.table}. This should be the output of a previous call
#'                      to \code{spread}, where \code{returnIndices} was \code{TRUE}.
#'                      Default \code{NA}, meaning the spread is starting from \code{loci}.
#'                      See Details.
#'
#' @param circle        Logical. If \code{TRUE}, then outward spread will be by
#'                      equidistant rings, rather than solely by adjacent cells
#'                      (via \code{directions} arg.). Default is \code{FALSE}.
#'                      Using \code{circle = TRUE} can be dramatically slower for
#'                      large problems.
#'                      Note, this should usually be used with \code{spreadProb = 1}.
#'
#' @param circleMaxRadius Numeric. A further way to stop the outward spread of events.
#'                      If \code{circle} is \code{TRUE}, then it will grow to this maximum radius.
#'                      See section on \code{Breaking out of spread events}.
#'                      Default is \code{NA}.
#'
#' @param stopRule      A function which will be used to assess whether each
#'                      individual cluster should stop growing.
#'                      This function can be an argument of \code{"landscape"},
#'                      \code{"id"}, \code{"cells"}, and any other named vectors,
#'                      a named list of named vectors, or a named \code{data.frame}
#'                      with column names passed to \code{spread} in the \code{...}.
#'                      Default \code{NA}, meaning that spreading will not stop
#'                      as a function of the landscape.
#'                      See section on "Breaking out of spread events" and examples.
#'
#' @param stopRuleBehavior Character. Can be one of \code{"includePixel"},
#'                      \code{"excludePixel"}, \code{"includeRing"}, or
#'                      \code{"excludeRing"}.
#'                      If \code{stopRule} contains a function, this argument is
#'                      used determine what to do with the cell(s) that caused
#'                      the rule to be \code{TRUE}. See details.
#'                      Default is \code{"includeRing"} which means to accept the
#'                      entire ring of cells that caused the rule to be \code{TRUE}.
#'
#' @param allowOverlap  Logical. If \code{TRUE}, then individual events can overlap
#'                      with one another, i.e., they do not interact (this is slower
#'                      than if \code{allowOverlap = FALSE}).
#'                      Default is \code{FALSE}.
#'
#' @param asymmetry     A numeric indicating the ratio of the asymmetry to be used.
#'                      Default is \code{NA}, indicating no asymmetry.
#'                      See details. This is still experimental.
#'                      \bold{Use with caution.}
#'
#' @param asymmetryAngle A numeric indicating the angle in degrees (0 is "up",
#'                      as in North on a map), that describes which way the
#'                      \code{asymmetry} is.
#'
#' @param quick  Logical. If \code{TRUE}, then several potentially time consuming
#'               checking (such as \code{inRange}) will be skipped.
#'               This should only be used if there is no concern about checking
#'               to ensure that inputs are legal.
#'
#' @param neighProbs A numeric vector, whose sum is 1.
#'                   It indicates the probabilities an individual spread iteration
#'                   spreading to \code{1:length(neighProbs)} neighbours.
#'
#' @param exactSizes Logical. If \code{TRUE}, then the \code{maxSize} will be
#'                   treated as exact sizes, i.e., the spread events will continue
#'                   until they are \code{floor(maxSize)}.
#'                   This is overridden by \code{iterations}, but if \code{iterations}
#'                   is run, and individual events haven't reached \code{maxSize},
#'                   then the returned \code{data.table} will still have at least
#'                   one active cell per event that did not achieve \code{maxSize},
#'                   so that the events can continue if passed into \code{spread}
#'                   with \code{spreadState}.
#'
#' @param relativeSpreadProb Logical. If \code{TRUE}, then \code{spreadProb} will
#'                      be rescaled *within* the \code{directions} neighbours, such that
#'                      the sum of the probabilities of all neighbours will be 1. Default
#'                      \code{FALSE}, unless \code{spreadProb} values are not contained
#'                      between 0 and 1, which will force \code{relativeSpreadProb}
#'                      to be \code{TRUE}.
#'
#' @param ...           Additional named vectors or named list of named vectors
#'                      required for \code{stopRule}. These
#'                      vectors should be as long as required e.g., length
#'                      \code{loci} if there is one value per event.
#'
#' @return Either a \code{RasterLayer} indicating the spread of the process in
#' the landscape or a \code{data.table} if \code{returnIndices} is \code{TRUE}.
#' If a \code{RasterLayer}, then it represents
#' every cell in which a successful spread event occurred. For the case of, say, a fire
#' this would represent every cell that burned. If \code{allowOverlap} is \code{TRUE},
#' This \code{RasterLayer} will represent the sum of the individual event ids
#' (which are numerics \code{seq_along(loci)}.
#' This will generally be of minimal use because it won't be possible to distinguish
#' if event 2 overlapped with event 5 or if it was just event 7.
#'
#' If \code{returnIndices} is \code{TRUE},
#' then this function returns a \code{data.table} with columns:
#'
#' \tabular{ll}{
#'   \code{id} \tab an arbitrary ID \code{1:length(loci)} identifying
#'                      unique clusters of spread events, i.e., all cells
#'                      that have been spread into that have a
#'                      common initial cell.\cr
#'   \code{initialLocus} \tab the initial cell number of that particular
#'                            spread event.\cr
#'   \code{indices} \tab The cell indices of cells that have
#'                        been touched by the spread algorithm.\cr
#'   \code{active} \tab a logical indicating whether the cell is active (i.e.,
#'                        could still be a source for spreading) or not (no
#'                        spreading will occur from these cells).\cr
#' }
#'
#' This will generally be more useful when \code{allowOverlap} is \code{TRUE}.
#'
#' @author Eliot McIntire and Steve Cumming
#' @export
#' @importFrom data.table := data.table setcolorder set
#' @importFrom fastmatch %fin%
#' @importFrom fpCompare %<=%
#' @importFrom magrittr %>%
#' @importFrom quickPlot clearPlot Plot
#' @importFrom raster extent maxValue minValue ncell ncol nrow raster res setValues
#' @importFrom stats runif
#' @importFrom utils assignInMyNamespace
#' @rdname spread
#'
#' @seealso \code{\link{spread2}} for a different implementation of the same algorithm.
#' It is more robust, meaning, there will be fewer unexplainable errors, and the behaviour
#' has been better tested, so it is more likely to be exactly as described under all
#' argument combinations.
#' Also, \code{\link{rings}} which uses \code{spread} but with specific argument
#' values selected for a specific purpose.
#' \code{\link[raster]{distanceFromPoints}}.
#' \code{\link{cir}} to create "circles"; it is fast for many small problems.
#'
setGeneric(
  "spread",
  function(landscape, loci = NA_real_, spreadProb = 0.23, persistence = 0,
           mask = NA, maxSize = 1e8L, directions = 8L, iterations = 1e6L,
           lowMemory = NULL, # getOption("spades.lowMemory"),
           returnIndices = FALSE,
           returnDistances = FALSE, mapID = NULL, id = FALSE, plot.it = FALSE,
           spreadProbLater = NA_real_, spreadState = NA,
           circle = FALSE, circleMaxRadius = NA_real_,
           stopRule = NA, stopRuleBehavior = "includeRing", allowOverlap = FALSE,
           asymmetry = NA_real_, asymmetryAngle = NA_real_, quick = FALSE,
           neighProbs = NULL, exactSizes = FALSE, relativeSpreadProb = FALSE, ...) {
    standardGeneric("spread")
})

#' @param plot.it  If \code{TRUE}, then plot the raster at every iteration,
#'                 so one can watch the spread event grow.
#'
#' @param mapID    Deprecated. Use \code{id}.
#'
#' @param id    Logical. If \code{TRUE}, returns a raster of events ids.
#'              If \code{FALSE}, returns a raster of iteration numbers,
#'              i.e., the spread history of one or more events.
#'              NOTE: this is overridden if \code{returnIndices} is \code{TRUE}
#'              or \code{1} or \code{2}.
#'
#' @rdname spread
#'
#' @example inst/examples/example_spread.R
#'
setMethod(
  "spread",
  signature(landscape = "RasterLayer"),
  definition = function(landscape, loci, spreadProb, persistence, mask, maxSize,
                        directions, iterations, lowMemory, returnIndices,
                        returnDistances, mapID, id, plot.it, spreadProbLater,
                        spreadState, circle, circleMaxRadius, stopRule,
                        stopRuleBehavior, allowOverlap, asymmetry, asymmetryAngle,
                        quick, neighProbs, exactSizes, relativeSpreadProb, ...) {
    if (!is.null(neighProbs)) {
      if (isTRUE(allowOverlap))
        stop("Can't use neighProbs and allowOverlap = TRUE together")
    }
    samInt <- if (requireNamespace("dqrng", quietly = TRUE))
      dqrng::dqsample.int
    else
      sample.int


    if (!is.null(mapID)) {
      warning("mapID is deprecated, use id")
      id <- mapID
    }
    if (!quick) {
      allowedRules <- c("includePixel", "excludePixel", "includeRing", "excludeRing")
      if (!any(stopRuleBehavior %fin% allowedRules))
        stop("stopRuleBehaviour must be one of \"",
             paste(allowedRules, collapse = "\", \""), "\".")
    }
    if (isTRUE(lowMemory)) {
      requireNamespace("ff", quietly = TRUE)
      requireNamespace("ffbase", quietly = TRUE)
    }

    spreadStateExists <- is(spreadState, "data.table")
    spreadProbLaterExists <- TRUE

    if (!is(spreadProbLater, "Raster")) {
      if (anyNA(spreadProbLater)) {
        spreadProbLaterExists <- FALSE
        spreadProbLater <- spreadProb
      }
    }

    ### should sanity check map extents
    if (any(is.na(loci)))  {
      # start it in the centre cell, if there is no spreadState
      if (!spreadStateExists)
        loci <- middlePixel(landscape) #(nrow(landscape) / 2L + 0.5) * ncol(landscape)
    }
    if (!quick) {
      dupLoci <- duplicated(loci)
      if (any(duplicated(loci))) {
        message("duplicate initial loci are provided")
        # loci <- loci[dupLoci]
      }
    }

    if (length(loci) == 0) stop("No loci. Nothing to do")

    if (any(!is.na(maxSize))) {
      msEqZero <- maxSize < 1
      if (any(msEqZero)) {
        loci <- loci[!msEqZero]
        maxSize <- maxSize[!msEqZero]
      }
    }

    if (spreadStateExists) {
      keepers <- spreadState$active == TRUE
      loci <- initialActiveCells <- spreadState[keepers, indices]
      initialLoci <- unique(spreadState$initialLocus)
    } else {
      initialLoci <- loci
    }
    lenInitialLoci <- length(initialLoci)
    sequenceInitialLoci <- seq(lenInitialLoci)

    # Check for probabilities
    if (!quick) {
      if (is(spreadProbLater, "RasterLayer") | is(spreadProb, "Rasterlayer")) {
        if ((minValue(spreadProb) > 1L) || (maxValue(spreadProb) < 0L) ||
            (maxValue(spreadProb) > 1L) || (minValue(spreadProb) < 0L)) {
          relativeSpreadProb <- TRUE
        }
        if (spreadProbLaterExists)
          if (((minValue(spreadProbLater) > 1L) || (maxValue(spreadProbLater) < 0L) ||
              (maxValue(spreadProbLater) > 1L) || (minValue(spreadProbLater) < 0L))) {
            relativeSpreadProb <- TRUE
          }
      } else {
        if (!all(inRange(na.omit(spreadProb)))) {
          relativeSpreadProb <- TRUE
          stop("spreadProb is not a probability")
        }
        if (spreadProbLaterExists) {
          relativeSpreadProb <- TRUE
          if (!all(inRange(na.omit(spreadProbLater)))) stop("spreadProbLater is not a probability")
        }
      }
    }

    ncells <- as.integer(ncell(landscape))

    if (allowOverlap | returnDistances | spreadStateExists) {
      if (spreadStateExists) {
        spreads <- as.matrix(spreadState[, list(initialLocus, indices, id, active)])
      } else {
        spreads <- cbind(initialLocus = initialLoci, indices = initialLoci,
                         id = 1:length(loci), active = 1)
      }
    } else {
      if (!is.null(lowMemory)) {
        message("lowMemory argument is now deprecated; using standard spread")
        # create vector of 0s called spreads, which corresponds to the indices
        # of the landscape raster
        #spreads <- ff(vmode = "short", 0, length = ncells)
      } #else {

      # The experimental new spread function has some changes for speed. 1) The
      # bottleneck amazingly, was the creation of a new empty vector of length
      # ncell(landscape) ... it took >50% of the time of the spread function
      # when called 100,000s of times on a variety of spreadProb situations. 2) I
      # found that the only way to stop instantiating this was to have a
      # data.table object that uses reference semantics. 3) Put a simple, 1 column
      # data.table object into the SpaDES.tools namespace. It will contain the
      # former spreads object which was 0 everywhere the events hadn't spread
      # to, and a non-zero integer otherwise. 4) The function has to make sure that
      # it is "correct" on leaving the function. Two different cases: A) it
      # exits improperly --> action is delete this object; B) it exits correctly
      # --> action is to change all the values that were non-zero back to zero,
      # rather than delete the object. The whole point is to keep the object
      # intact after it has exited spread, so that it is available again
      # immediately for reuse.

      needEmptySpreads <- TRUE
      stNamespace <- asNamespace("SpaDES.tools")
      if (exists("spreadsDT", envir = stNamespace)) {
        spreadsDT <- get("spreadsDT", envir = stNamespace)
        # set(spreadsDT, NULL, "spreads", 0L)
        # spreads <- spreadsDT$spreads
        if (identical(NROW(spreadsDT), ncells)) {
          needEmptySpreads <- FALSE
        }
      }
      if (needEmptySpreads) {
        spreads <- vector("integer", ncells)
        spreadsDT <- data.table(spreads = spreads)
        set(spreadsDT, NULL, "spreads", 0L)
        # put the empty data.table into the SpaDES.tools namespace
        assignInMyNamespace("spreadsDT", spreadsDT)
        on.exit({assignInMyNamespace("spreadsDT", integer())})
      }
    }
    n <- 1L

    # circle needs directions to be 8
    if (circle | !is.na(asymmetry)) {
      if (circle) directions <- 8L # only required for circle
      initialLociXY <- cbind(id = seq_along(initialLoci), xyFromCell(landscape, initialLoci))
      id <- TRUE
      if (allowOverlap | returnDistances) {
        spreads <- cbind(spreads, dists = 0)
      }
    }

    # determine ... variables
    otherVars <- list(...)
    anyList <- unlist(lapply(otherVars, is.list))

    if (any(anyList)) {
      otherVarsLists <- unlist(unname(otherVars), recursive = FALSE)
      otherVars[anyList] <- NULL
      otherVars <- append(otherVars, otherVarsLists)
    }

    # check validity of stopRule
    if (is.function(stopRule)) {
      id <- TRUE
      stopRuleObjs <- names(formals(stopRule))
      if (!quick) {
        if (any(is.na(match(stopRuleObjs,
                            c("id", "landscape", "cells", names(otherVars)))))) {
          stop("Arguments in stopRule not valid.\n",
               "The function definition must be a function of built-in options,",
               " (id, landscape, or cells) or user supplied variables.",
               " If user supplied, the variables",
               " must be passed as named vectors, or lists or data.frames.",
               " See examples.")
        }
      }
      landRasNeeded <- any(stopRuleObjs == "landscape")
      colNamesPotentials <- c("id", "landscape"[landRasNeeded], "cells", "prev")
      argNames <- c(colNamesPotentials, names(otherVars))
      whArgs <- match(names(formals(stopRule)), argNames)

      # Raster indexing is slow. If there is are Rasters submitted with the stopRule
      #  then this will convert them to vectors first. Clearly, this will have
      #  memory consequences if the Rasters are on disk, but spread is optimized for speed
      rasters <- unlist(lapply(otherVars[names(otherVars)], function(x) is(x, "Raster")))
      if (any(rasters)) {
        for (i in 1:which(rasters)) {
          otherVars[[names(rasters[i])]] <- otherVars[[names(rasters[i])]][]
        }
      }
      landRas <- landscape[] # For speed
    }

    if (!allowOverlap & !returnDistances) {
      if (id | returnIndices > 0 | relativeSpreadProb) {
        if (!spreadStateExists) {
          set(spreadsDT, loci, "spreads", seq(loci))
          ##DT spreads[loci] <- 1L:length(loci)
          # give values to spreads vector at initialLoci
        }
      } else {
        spreads[loci] <- n
      }
      spreadsIndices <- unname(loci)
      length(spreadsIndices) <- length(loci) * 100
      prevSpreadIndicesActiveLen <- length(loci)
      prevSpreadIndicesFullLen <- length(spreadsIndices)
    }

    # Convert mask and NAs to 0 on the spreadProb Raster
    if (is(spreadProb, "Raster")) {
      # convert NA to 0s
      #isNASpreadProb <- is.na(spreadProb[])
      # if (anyNA(spreadProb[])) {
      #   isNASpreadProb <- is.na(spreadProb[])
      #   spreadProb[isNASpreadProb] <- 0L
      # }
    } else if (is.numeric(spreadProb)) {
      # Translate numeric spreadProb into a Raster, if there is a mask
      if (is(mask, "Raster")) {
        spreadProb <- raster(extent(landscape), res = res(landscape), vals = spreadProb)
      }
    }

    # Convert mask and NAs to 0 on the spreadProbLater Raster
    if (is(spreadProbLater, "Raster")) {
    } else if (is.numeric(spreadProbLater)) {
      # Translate numeric spreadProbLater into a Raster, if there is a mask
      if (is(mask, "Raster")) {
        spreadProbLater <- raster(extent(landscape), res = res(landscape), vals = spreadProbLater)
      }
    }

    # Mask spreadProbLater and spreadProb
    if (is(mask, "Raster")) {
      spreadProbLater[mask[] == 1L] <- 0L
      spreadProb[mask[] == 1L] <- 0L
    }

    if (spreadStateExists) {
      if (allowOverlap | returnDistances) {
        stop("Using spreadState with either allowOverlap = TRUE",
             " or returnDistances = TRUE is not implemented")
      } else {
        if (sum(colnames(spreadState) %fin% c("indices", "id", "active", "initialLocus")) != 4) {
          stop("spreadState must have at least columns: ",
               "indices, id, active, and initialLocus.")
        }
      }
    }


    if (!quick)
      if (any(loci > ncells)) stop("loci indices are not on landscape")

    # Recycling maxSize as needed
    if (any(!is.na(maxSize))) {
      if (!is.integer(maxSize)) maxSize <- floor(maxSize)
      if (spreadStateExists) {
        sizeAll <- spreadState[, list(len = .N), by = id]
        size <- c(sizeAll[, len])
      } else {
        maxSize <- rep_len(maxSize, length(loci))
        size <- rep_len(1L, length(loci))
      }
    } else {
      maxSize <- ncells
      size <- length(loci)
    }

    noMaxSize <- all(maxSize >= ncells) # will be used to omit testing for maxSize
    if (is.null(neighProbs)) {
      numNeighs <- NULL
    }

    if (!exists("numRetries", envir = .pkgEnv))
      assign("numRetries", rep(0, lenInitialLoci), envir = .pkgEnv)

    toColumn <- c("to", "indices")

    # while there are active cells
    while (length(loci) & (n <= iterations)) {
      if (!is.null(neighProbs)) {
        numNeighs <- if (is.list(neighProbs)) {
          unlist(lapply(neighProbs, function(x) {
            samInt(length(x), size = 1, replace = TRUE, prob = x)
          }))
        } else {
          samInt(length(neighProbs), size = length(loci), replace = TRUE,
                     prob = neighProbs)
        }
      }

      # identify neighbours
      if (allowOverlap | returnDistances | spreadStateExists) {
        whActive <- spreads[, "active"] == 1 # spreads carries over
        potentials <- adj(landscape, loci, directions, pairs = TRUE,
                          id = spreads[whActive, "id"])#, numNeighs = numNeighs)
        spreads[whActive, "active"] <- 0
        potentials <- cbind(potentials, active = 1)
      } else {
        if (id | returnIndices > 0 | circle | relativeSpreadProb | !is.null(neighProbs)) {
          potentials <- adj(landscape, loci, directions, pairs = TRUE)
        } else {
          # must pad the first column of potentials
          newAdj <- adj(landscape, loci, directions, pairs = FALSE)
          potentials <- cbind(NA, newAdj)
        }
      }

      if (circle)
        potentials <- cbind(potentials, dists = 0)

      # keep only neighbours that have not been spread to yet
      if (allowOverlap | returnDistances | spreadStateExists) {
        # data.table version is faster for potentials > 2000 or so
        if (NROW(potentials) > 2000) {
          spreadsDT <- as.data.table(spreads)
          potentialsDT <- as.data.table(potentials)
          potentialsDT[, initialLocus := initialLoci[potentialsDT$id]]
          colnamesPot <- colnames(potentialsDT)
          whIL <- which(colnamesPot == "initialLocus")
          whFrom <- which(colnamesPot == "from")
          setcolorder(potentialsDT,
                      c(colnamesPot[whIL], colnamesPot[-c(whIL, whFrom)], colnamesPot[whFrom]))
          setnames(potentialsDT, old = "to", new = "indices")
          newPot <- potentialsDT[!spreadsDT, on = c("id", "indices")]
          potentials <- as.matrix(newPot)
        } else {
          potentials <- cbind(initialLocus = initialLoci[potentials[, "id"]], potentials)
          colnames(potentials)[which(colnames(potentials) == "to")] <- "indices"
          colnamesPot <- colnames(potentials)
          whIL <- which(colnamesPot == "initialLocus")
          whFrom <- which(colnamesPot == "from")
          potentials <- potentials[,c(colnamesPot[whIL],
                                      colnamesPot[-c(whIL, whFrom)],
                                      colnamesPot[whFrom])]

          # These next lines including the lapply are the rate limiting
          #   step and it has been heavily worked to speed it up March 31, 2020
          seq2 <- sequenceInitialLoci[sequenceInitialLoci %in% potentials[,"id"]]
          out <- lapply(seq2, function(ind) {
            hasID <- potentials[, "id"] == ind
            po <- potentials[hasID, ]
            hasID2 <- spreads[, "id"] == ind
            inds <- spreads[hasID2, "indices"]
            vals <- po[, 2L] %in% inds
            po[!vals,]
          })
          potentials <- do.call(rbind, out)
        }
      } else {
        # Keep only the ones where it hasn't been spread to yet
        ##DT
        keep <- spreadsDT$spreads[potentials[, 2L]] == 0L
        # keep <- spreads[potentials[, 2L]] == 0L
       # if (any(!keep))
          potentials <- potentials[keep, , drop = FALSE]
      }

      if (n == 2) {
        spreadProb <- spreadProbLater
      }

      # extract spreadProb values from spreadProb argument
      if (is.numeric(spreadProb)) {
        if (!(length(spreadProb) == 1 || length(spreadProb) == ncell(landscape)))
          stop("spreadProb must be length 1 or length ncell(landscape), or a raster")
        if (n == 1 & spreadProbLaterExists) {
          # need cell specific values
          spreadProbs <- rep(spreadProb, NROW(potentials))
          spreadProb <- spreadProbLater
        } else {
          if (length(spreadProb) > 1) {
            spreadProbs <- spreadProb[potentials[, 2L]]
          } else {
            spreadProbs <- rep(spreadProb, NROW(potentials))
          }
        }
      } else {
        # here for raster spreadProb
        if (n == 1 & spreadProbLaterExists) {
          # need cell specific values
          spreadProbs <- spreadProb[][potentials[, 2L]]
          spreadProb <- spreadProbLater
        } else {
          spreadProbs <- spreadProb[][potentials[, 2L]]
        }
      }

      if (anyNA(spreadProbs)) spreadProbs[is.na(spreadProbs)] <- 0

      if (!is.na(asymmetry)) {
        if (allowOverlap | returnDistances) {
          a <- cbind(id = potentials[, 3L], to = potentials[, 2L],
                     xyFromCell(landscape, potentials[, 2L]))
        } else {
          a <- cbind(id = spreads[potentials[, 1L]], to = potentials[, 2L],
                     xyFromCell(landscape, potentials[, 2L]))
        }
        d <- directionFromEachPoint(from = initialLociXY, to = a)
        newSpreadProbExtremes <- (spreadProb[] * 2) / (asymmetry + 1) * c(1, asymmetry)
        angleQuality <- (cos(d[, "angles"] - rad(asymmetryAngle)) + 1) / 2
        spreadProbs <- newSpreadProbExtremes[1] + (angleQuality * diff(newSpreadProbExtremes))
        spreadProbs <- spreadProbs - diff(c(spreadProb[], mean(spreadProbs)))
      }

      if (!is.null(neighProbs) | relativeSpreadProb) {
        aaa <- split(seq_along(potentials[, toColumn[spreadStateExists + 1]]),
                     potentials[, "from"]);
        if (length(aaa) != length(numNeighs)) {
          activeCellContinue <- loci %in% unique(potentials[, "from"])
          numNeighs <- numNeighs[activeCellContinue]
        }

        tmpA <- unlist(lapply(aaa, length))
        tmpB <- which(tmpA < numNeighs)
        if (length(tmpB) > 0)
          numNeighs[tmpB] <- unname(tmpA[tmpB])

        if (relativeSpreadProb) {
          rescaledProbs <- tapply(spreadProbs, potentials[, "from"], function(x) {
            x / sum(x, na.rm = TRUE)
          }, simplify = FALSE)
          neighIndexToKeep <- unlist(lapply(seq_along(aaa), function(x)
            resample(aaa[[x]], size = numNeighs[x], prob = rescaledProbs[[x]])))
        } else {
          neighIndexToKeep <- unlist(lapply(seq_along(aaa), function(x)
            resample(aaa[[x]], size = numNeighs[x])))
        }
        potentials <- potentials[neighIndexToKeep, , drop = FALSE]
        spreadProbs <- spreadProbs[neighIndexToKeep]
        spreadProbs[spreadProbs > 0] <- 1
      }

      randomSuccesses <- runifC(NROW(potentials)) <= spreadProbs
      potentials <- potentials[randomSuccesses, , drop = FALSE]

      # random ordering so not always same:
      lenPot <- NROW(potentials)
      if (lenPot) {
        reorderVals <- samInt(lenPot)
        potentials <- potentials[reorderVals, , drop = FALSE]
      }
      if (!allowOverlap) {
        # here is where allowOverlap and returnDistances are different ##### NOW OBSOLETE, I BELIEVE ELIOT March 2020
        potentials <- potentials[!duplicated(potentials[, 2L]), , drop = FALSE]
      } else {
        pots <- potentials[, c("id", "indices"), drop = FALSE]
        potentials <- potentials[!duplicated(pots), , drop = FALSE]
      }

      # increment iteration
      n <- n + 1L

      # potentials can become zero because all active cells are edge cells
      if (length(potentials) > 0) {
        # implement circle
        if (!missing(circle)) {
          if (circle) {
            if (allowOverlap | returnDistances) {
              a <- cbind(potentials, xyFromCell(landscape, potentials[, 2L]))
            } else {
              a <- cbind(potentials, id = spreads[potentials[, "from"]],
                         xyFromCell(landscape, potentials[, "to"]))
            }
            # need to remove dists column because distanceFromEachPoint, adds one back
            a <- a[, !(colnames(a) %fin% c("dists")), drop = FALSE]
            # need 3 columns, id, x, y in both initialLociXY and a
            d <- distanceFromEachPoint(initialLociXY, a, angles = asymmetry) # d is sorted
            cMR <- (n - 1) * res(landscape)[1]
            if (!any(is.na(circleMaxRadius))) {
              # don't bother proceeding if circleMaxRadius is larger than current iteration
              if (any(circleMaxRadius <= ((n - 1) * res(landscape)[1]))) {
                if (length(circleMaxRadius) > 1) {
                  # if it is a vector of values
                  cMR <- circleMaxRadius[d[, "id"]]
                } else {
                  cMR <- circleMaxRadius
                }
              }
            }
            potentials <- d[, !(colnames(d) %fin% c("x", "y")), drop = FALSE]
            potentials <- potentials[(d[, "dists"] %<=% cMR), , drop = FALSE]
          }
        }

        events <- potentials[, 2L]

        if (!noMaxSize) {
          if (allowOverlap | returnDistances | spreadStateExists) {
            len <- tabulate(potentials[, 3L], length(maxSize))
          } else {
            # actually interested in potential[,2L], but they don't have values yet..
            #  can use their source
            len <- tabulate(spreadsDT$spreads[potentials[, 1L]], length(maxSize))
          }
          if (any((size + len) > maxSize & size <= maxSize)) {
            whichID <- which(size + len > maxSize)

            # remove some active cells, if more than maxSize
            toRm <- (size + len)[whichID] - maxSize[whichID]

            for (i in 1:length(whichID)) {
              if (allowOverlap | returnDistances | spreadStateExists) {
                thisID <- which(potentials[, 3L] == whichID[i])
              } else {
                thisID <- which(spreadsDT$spreads[potentials[, 1L]] == whichID[i])
              }

              # some unusual cases where there are none on the spreads. Unsure how this occurs
              if (length(thisID))
                potentials <- potentials[-resample(thisID, toRm[i]), , drop = FALSE]
            }
            events <- potentials[, 2L]
          }
          size <- pmin(size + len, maxSize) ## Quick? and dirty. fast but loose (too flexible)
        }

        # Implement stopRule section
        if (is.function(stopRule) & length(events) > 0) {
          if (allowOverlap | returnDistances) {
            prevCells <- cbind(
              id = spreads[, "id"],
              landscape = if (landRasNeeded) landRas[spreads[, "indices"]] else NULL,
              cells = spreads[, "indices"], prev = 1)
            eventCells <- cbind(id = potentials[, "id"],
                                landscape = if (landRasNeeded) landRas[events] else NULL,
                                cells = events, prev = 0)
          } else {
            whgtZero <- spreadsIndices
            prevCells <- cbind(id = spreads[whgtZero],
                               landscape = if (landRasNeeded) landRas[whgtZero] else NULL,
                               cells = whgtZero, prev = 1)
            eventCells <- cbind(id = spreads[potentials[, 1L]],
                                landscape = if (landRasNeeded) landRas[potentials[, 2L]] else NULL,
                                cells = potentials[, 2L], prev = 0)
          }
          if (circle) {
            prevCells <- cbind(prevCells, dist = NA)
            eventCells <- cbind(eventCells, dist = potentials[, "dists"])
          }
          # don't need to continue doing ids that are not active
          tmp <- rbind(prevCells[prevCells[, "id"] %fin% unique(eventCells[, "id"]), ], eventCells)

          ids <- unique(tmp[, "id"])

          shouldStopList <- lapply(ids, function(id) {
            shortTmp <- tmp[tmp[, "id"] == id, ]
            args <- append(list(id = id),
                           lapply(colNamesPotentials[-1], function(j) shortTmp[, j]))
            names(args) <- colNamesPotentials
            args <- append(args, otherVars)
            do.call(stopRule, args[whArgs])
          })
          if (any(lapply(shouldStopList, length) > 1))
            stop("stopRule does not return a length-one logical.",
                 " Perhaps stopRule need indexing by cells or id?")

          shouldStop <- unlist(shouldStopList)

          names(shouldStop) <- ids

          if (any(shouldStop)) {
            if (stopRuleBehavior != "includeRing") {
              if (stopRuleBehavior != "excludeRing") {
                whStop <- as.numeric(names(shouldStop)[shouldStop])
                whStopAll <- tmp[, "id"] %fin% whStop
                tmp2 <- tmp[whStopAll, ]

                whStopEvents <- eventCells[, "id"] %fin% whStop

                # If an event needs to stop, then must identify which cells are included
                out <- lapply(whStop, function(id) {
                  tmp3 <- tmp2[tmp2[, "id"] == id, ]
                  newOnes <- tmp3[, "prev"] == 0
                  ord <- seq_along(newOnes)

                  # because of undesired behaviour of sample when length(x) == 1
                  if (sum(newOnes) > 1) {
                    ord[newOnes] <- sample(ord[newOnes])
                    if (circle) ord[newOnes] <- ord[newOnes][order(tmp3[ord[newOnes], "dist"])]
                    tmp3 <- tmp3[ord, ]
                  }
                  startLen <- sum(!newOnes)
                  addIncr <- 1
                  done <- FALSE
                  args <- append(list(id = id),
                                 lapply(colNamesPotentials[-1], function(j) {
                                   tmp3[1:startLen, j]
                                 })) # instead of as.data.frame
                  names(args) <- colNamesPotentials
                  args <- append(args, otherVars)
                  argsSeq <- seq_along(colNamesPotentials[-1]) + 1

                  while (!done) {
                    args[argsSeq] <- lapply(colNamesPotentials[-1], function(j) {
                      unname(c(args[[j]], tmp3[(startLen + addIncr), j]))
                    }) # instead of as.data.frame
                    done <- do.call(stopRule, args[whArgs])
                    addIncr <- addIncr + 1
                  }
                  if (stopRuleBehavior == "excludePixel") addIncr <- addIncr - 1
                  firstInd <- startLen + addIncr
                  lastInd <- NROW(tmp3)
                  sequ <- if (firstInd > lastInd) 0 else firstInd:lastInd
                  tmp3[sequ, , drop = FALSE]
                })
                eventRm <- do.call(rbind, out)[, "cells"]
                cellsKeep <- !(potentials[, 2L] %fin% eventRm)
              } else {
                cellsKeep <- rep(FALSE, NROW(potentials))
              }
              potentials <- potentials[cellsKeep, , drop = FALSE]
              events <- potentials[, 2L]
              eventCells <- eventCells[cellsKeep, , drop = FALSE]
            }
            toKeepSR <- !(eventCells[, "id"] %fin% as.numeric(names(which((shouldStop)))))
          }
        }

        if (length(events) > 0) {
          # place new value at new cells that became active
          if (allowOverlap | returnDistances | spreadStateExists) {
            fromCol <- colnames(potentials) == "from"

            spreads <- rbind(spreads, potentials[, !fromCol])
            if ((returnDistances | spreadStateExists) & !allowOverlap) {
              # 2nd place where allowOverlap and returnDistances differ
              notDups <- !duplicated(spreads[, "indices"])
              nrSpreads <- NROW(spreads)
              nrPotentials <- NROW(potentials)
              notDupsEvents <- notDups[-(1:(nrSpreads - nrPotentials))]
              spreads <- spreads[notDups, , drop = FALSE]
              events <- events[notDupsEvents]
            }
          } else {
            if (id | returnIndices > 0 | relativeSpreadProb) {
              # give new cells, the id of the source cell
              ##DT
              set(spreadsDT, events, "spreads", spreadsDT$spreads[potentials[, 1L]])
              # spreads[events] <- spreads[potentials[, 1L]]
            } else {
              spreads[events] <- n
            }
            curEventsLen <- length(events)
            addedIndices <- prevSpreadIndicesActiveLen + 1:curEventsLen

            if (sum(curEventsLen, prevSpreadIndicesActiveLen) > prevSpreadIndicesFullLen) {
              length(spreadsIndices) <- (prevSpreadIndicesActiveLen + curEventsLen) * 2
              prevSpreadIndicesFullLen <- length(spreadsIndices)
            }
            spreadsIndices[addedIndices] <- events
            prevSpreadIndicesActiveLen <- prevSpreadIndicesActiveLen + curEventsLen
            # spreadsIndices <- c(spreadsIndices, events)
          }
        }

        # remove the cells from "events" that push it over maxSize
        #  There are some edge cases that aren't captured above ... identify them...
        if (length(maxSize) > 1L) {
          if (exists("whichID", inherits = FALSE)) {
            # must update toKeepSR in case that is a second reason to stop event
            if (exists("toKeepSR", inherits = FALSE)) {
              if (allowOverlap | returnDistances) {
                maxSizeKeep <- !(spreads[spreads[, "active"] == 1, "id"] %fin% whichID)
                spreads <- spreads[c(rep(TRUE, sum(spreads[, "active"] == 0)), maxSizeKeep), ]
              } else {
                maxSizeKeep <- !spreads[events] %fin% whichID
              }
              events <- events[maxSizeKeep]
              toKeepSR <- toKeepSR[maxSizeKeep]
            }
            rm(whichID)
          }
        } else {
          if (all(size >= maxSize)) {
            potentials <- potentials[0L,] # remove any potential cells, as size is met
            events <- NULL
          }
        }

        # Remove cells that were stopped by stopRule
        if (is.function(stopRule)) {
          if (exists("toKeepSR", inherits = FALSE)) {
            events <- events[toKeepSR]
            if (allowOverlap | returnDistances) {
              spreads[c(rep(TRUE, sum(spreads[, "active"] == 0)), !toKeepSR), "active"] <- 0
            }
            rm(toKeepSR)
          }
        }
      } else {
        # there are no potentials -- possibly from failed runif, or spreadProbs all 0
        events <- NULL
      }

      if (exactSizes) {
        if (all(get("numRetries", inherits = FALSE, envir = .pkgEnv) < 10)) {
          if (spreadStateExists) {
            tooSmall <- tabulate(spreads[, "id"], length(maxSize)) < maxSize
            inactive <- tabulate(spreads[spreads[, "active"] == 1, "id"], length(maxSize)) == 0
          } else {
            tooSmall <- tabulate(spreads, length(maxSize)) < maxSize
            inactive <- tabulate(spreads[events], length(maxSize)) == 0
          }

          # these are ones that are stuck ... i.e., too small, and inactive
          needPersist <- tooSmall & inactive
          needPersistJump <- TRUE
          if (any(needPersist)) {
            assign("numRetries", envir = .pkgEnv,
                   get("numRetries", inherits = FALSE, envir = .pkgEnv) + needPersist)

            if (spreadStateExists) {
              whSmallInactive <- which(tooSmall & inactive)
              spreadsSmallInactive <- spreads[spreads[, "id"] %in% whSmallInactive, , drop = FALSE]
              if (needPersistJump) {
                message("Jumping to new active location, up to 1000 m away")
                mmm <- rings(landscape, loci = spreadsSmallInactive[, "indices"],
                             maxRadius = 1000, minRadius = 1, returnIndices = TRUE)
                wh <- mmm[, list(whKeepLoci = resample(.I, 1)), by = id]$whKeepLoci
              } else {
                for (whSI in whSmallInactive) {
                  wh <- which(spreads[, "id"] == whSI)
                  wh <- tail(wh, 2) # pick last two ones from all inactive cells
                  keepLoci <- spreads[wh, "indices"]
                  events <- c(keepLoci, events)
                  spreads[wh, "active"] <- 1
                }
              }
            } else {
              keepLoci <- spreads[loci] %fin% which(tooSmall & inactive)
              events <- c(loci[keepLoci], events)
            }

          }
        }
      }

      # drop or keep loci
      if (is.na(persistence) | persistence == 0L) {
        loci <- NULL
      } else {
        if (inRange(persistence)) {
          loci <- loci[runif(length(loci)) <= persistence]
        } else {
          # here is were we would handle methods for raster* or functions
          stop("Unsupported type: persistence")
        }
      }

      if (plot.it) {
        if (n == 2 & !spreadStateExists) clearPlot()
        if (allowOverlap | returnDistances) {
          spreadsDT <- data.table(spreads);
          hab2 <- landscape;
          hab2[] <- 0;
          pixVal <- spreadsDT[, sum(id), by = indices]
          hab2[pixVal$indices] <- pixVal$V1;
          Plot(hab2, legendRange = c(0, sum(seq_along(initialLoci))))
        } else {
          plotCur <- raster(landscape)
          plotCur <- setValues(plotCur, spreads)
          Plot(plotCur)
        }
      }

      # new loci list for next while loop, concat of persistent and new events
      loci <- c(loci, events)
    } # end of while loop

    if (!allowOverlap & !returnDistances)
      spreadsIndices <- spreadsIndices[1:prevSpreadIndicesActiveLen]


    # Convert the data back to raster
    if (!allowOverlap & !returnDistances & !spreadStateExists) {
      # if (lowMemory) {
      #   wh <- ffwhich(spreads, spreads > 0) %>% as.ram()
      #   if (returnIndices > 0) {
      #     wh <- wh[!(wh %in% potentials[,2L])]
      #     completed <- data.table(indices = wh, id = spreads[wh], active = FALSE)
      #     if (NROW(potentials) > 0) {
      #       active <- data.table(indices = potentials[, 2L],
      #                            id = spreads[potentials[, 1L]],
      #                            active = TRUE)
      #     } else {
      #       active <- data.table(indices = numeric(0), id = numeric(0),
      #                            active = logical(0))
      #     }
      #   }
      # } else {
        wh <- if (spreadStateExists) {
          c(spreadState[!keepers]$indices, spreadsIndices)
        } else {
          spreadsIndices
        }
        if (returnIndices > 0) {
          # wh already contains the potentials for next iteration -- these should be not duplicated
          #   inside "completed"
          wh <- wh[!(wh %in% potentials[,2L])]
          completed <- wh %>% data.table(indices = ., id = spreadsDT$spreads[.], active = FALSE)
          if (NROW(potentials) > 0) {
            active <- data.table(indices = potentials[, 2L],
                                 id = spreads[potentials[, 1L]],
                                 active = TRUE)
          } else {
            active <- data.table(indices = integer(0), id = integer(0),
                                 active = logical(0))
          }
        }
      }
    #}

    if (returnIndices == 1) {
      if (allowOverlap | returnDistances | spreadStateExists) {
        keepCols <- c(3, 1, 2, 4)
        if (circle) keepCols <- c(keepCols, 5)

        # change column order to match non allowOverlap
        allCells <- as.data.table(spreads[, keepCols, drop = FALSE])
        set(allCells, , j = "active", as.logical(allCells$active))
        # setkeyv(allCells, "id")
      } else {
        allCells <- rbindlist(list(completed, active)) # active first; next line will keep active
        if (spreadStateExists) {
          initEventID <- unique(spreadState$id)
        } else {
          initEventID <- allCells[indices %fin% initialLoci, id]
        }
        if (!all(is.na(initialLoci))) {
          attr(initialLoci, ".match.hash") <- NULL # something in data.table put this
          dtToJoin <- data.table(id = sort(initEventID), initialLocus = initialLoci)
        } else {
          dtToJoin <- data.table(id = numeric(0), initialLocus = numeric(0))
        }
        setkeyv(dtToJoin, "id")
        setkeyv(allCells, "id")

        # tack on initialLoci
        allCells <- dtToJoin[allCells]
      }
      allCells[]
      if (exactSizes)
        if (exists("numRetries", envir = .pkgEnv)) {
          if (sum(allCells$active) == 0) rm("numRetries", envir = .pkgEnv)
        }
      if (!allowOverlap & !returnDistances) {
        set(spreadsDT, allCells$indices, "spreads", 0L)
        # remove the previous on.exit which had the effect of deleting the contents
        #   completely on a failed `spread`. Here, we want to delete the previous
        #   on.exit --> allowing the object to stay intact, but with only zeros.
        on.exit()
        # spreadsIndices <- spreadsIndices[1:prevSpreadIndicesActiveLen]
      }

      return(allCells)
    }
    if (returnIndices == 2) {
      return(wh)
    }

    landscape[] <- 0
    landscape@legend@colortable <- logical(0) # remove colour table
    if (allowOverlap | returnDistances) {
      if (returnDistances & !allowOverlap) {
        landscape[spreads[, "indices"]] <- spreads[, "dists"]
      } else {
        spreadsDT <- data.table(spreads);
        if (returnDistances & allowOverlap) {
          pixVal <- spreadsDT[, min(dists), by = indices]
          message("returnDistances is TRUE, allowOverlap is TRUE, but returnIndices is FALSE; ",
                  "returning minimum distance raster.")
        } else {
          pixVal <- spreadsDT[, sum(id), by = indices]
        }
        landscape[pixVal$indices] <- pixVal$V1;
      }
    } else {
      landscape[wh] <- spreads[wh]
      if (exists("potentials"))
        if (NROW(potentials) > 0)
          landscape[potentials[, 1L]] <- spreads[potentials[, 2L]]
    }
    return(landscape)
})

spreadsDT <- integer()
