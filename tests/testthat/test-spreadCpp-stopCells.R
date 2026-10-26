## The optional stop rule of spreadCpp(): an event stops once it has burned `stopAt`
## of its own stop cells.

stopRas <- function(side) terra::rast(terra::ext(0, side, 0, side), resolution = 1, vals = 0)

## cells of the square ring at Chebyshev distance `r` from cell `centre`, on a side x side grid
ringCells <- function(side, centre, r) {
  rc <- (centre - 1L) %/% side; cc <- (centre - 1L) %% side
  g <- expand.grid(row = 0:(side - 1L), col = 0:(side - 1L))
  d <- pmax(abs(g$row - rc), abs(g$col - cc))
  which(d == r)
}

test_that("with stopCells, stopEvent and stopAt all NULL the output and random stream are those of development", {
  testInit(c("terra", "data.table", "withr"))
  scen <- list(
    a = function() spreadCpp(stopRas(60L), loci = c(1000L, 2000L), spreadProb = 0.2),
    b = function() spreadCpp(stopRas(120L), loci = c(2000L, 4000L, 8000L, 12000L),
                             spreadProb = 0.9, maxSize = c(5, 25, 100, 1)),
    c = function() spreadCpp(stopRas(80L), loci = c(500L, 3000L, 5000L), spreadProb = 0.3,
                             minSize = 40, jumpTries = 5L, directions = 4L),
    d = function() spreadCpp(stopRas(80L), loci = c(500L, 3000L, 5000L), spreadProb = 0.25,
                             minSize = 30, minSizeTries = 20L, iterations = 15))
  ## nrow, sum(indices * row), sum(id * row), and the next random number, recorded on development
  expected <- list(
    a = c(39, 1415309, 1482, 0.138094096677378),
    b = c(131, 67935074, 25589, 0.44488204154186),
    c = c(194, 78054595, 48560, 0.998711626976728),
    d = c(309, 168514634, 115104, 0.636633664136752))
  for (n in names(scen)) {
    withr::with_seed(11, {
      o <- scen[[n]]()
      u <- stats::runif(1)
    })
    i <- seq_len(nrow(o))
    expect_equal(c(nrow(o), sum(as.numeric(o$indices) * i), sum(o$id * i), u), expected[[n]],
                 tolerance = 1e-12, info = n)
  }
})

test_that("an event stops with exactly stopAt of its stop cells burned, mid-generation", {
  testInit(c("terra", "data.table", "withr"))
  side <- 101L
  ras <- stopRas(side)
  centre <- 5101L
  ring <- ringCells(side, centre, 10L)
  full <- spreadCpp(ras, loci = centre, spreadProb = 1)
  expect_identical(nrow(full), side * side)
  for (k in c(1L, 3L, 7L)) {
    for (s in 1:5) {
      out <- withr::with_seed(s, spreadCpp(ras, loci = centre, spreadProb = 1,
                                           stopCells = ring, stopEvent = rep(1L, length(ring)), stopAt = k))
      expect_identical(sum(out$indices %in% ring), k)
      expect_lt(nrow(out), nrow(full))
      ## nothing beyond the ring burned except in the generation that reached it
      expect_lte(nrow(out), (2 * 10 + 1)^2)
    }
  }
})

test_that("an event whose stopAt exceeds its stop cells is unaffected", {
  testInit(c("terra", "data.table", "withr"))
  ras <- stopRas(101L)
  ring <- ringCells(101L, 5101L, 10L)
  for (s in 1:3) {
    base <- withr::with_seed(s, spreadCpp(ras, loci = 5101L, spreadProb = 0.5, maxSize = 600))
    out <- withr::with_seed(s, spreadCpp(ras, loci = 5101L, spreadProb = 0.5, maxSize = 600,
                                         stopCells = ring, stopEvent = rep(1L, length(ring)),
                                         stopAt = length(ring) + 1L))
    expect_identical(out, base)
  }
})

test_that("stop cells of one event do not count for another", {
  testInit(c("terra", "data.table", "withr"))
  side <- 101L
  ras <- stopRas(side)
  a <- 2 * side + 2L; b <- 99L * side - 1L          # opposite corners, far apart
  cap <- 300
  ## event 2's ring, handed to event 1: event 2 burns all of it, event 1 never gets there
  ring2 <- ringCells(side, b, 8L)
  ring2 <- ring2[ring2 >= 1L & ring2 <= side * side]
  out <- withr::with_seed(1, spreadCpp(ras, loci = c(a, b), spreadProb = 1, maxSize = cap,
                                       stopCells = ring2, stopEvent = rep(1L, length(ring2)), stopAt = 1L))
  expect_identical(as.vector(table(out$id)), c(300L, 300L))
  expect_gt(sum(out$indices[out$id == 2L] %in% ring2), 1L)
  ## the same ring given to event 2 stops it after exactly 2
  out2 <- withr::with_seed(1, spreadCpp(ras, loci = c(a, b), spreadProb = 1, maxSize = cap,
                                        stopCells = ring2, stopEvent = rep(2L, length(ring2)), stopAt = c(1L, 2L)))
  expect_identical(sum(out2$indices[out2$id == 2L] %in% ring2), 2L)
  expect_identical(sum(out2$id == 1L), 300L)
  ## a cell listed for both events counts for each; repeated pairs count once
  shared <- ringCells(side, a, 3L)
  out3 <- withr::with_seed(1, spreadCpp(ras, loci = c(a, b), spreadProb = 1, maxSize = cap,
                                        stopCells = c(shared, shared, shared), stopEvent = c(rep(1L, 2 * length(shared)), rep(2L, length(shared))),
                                        stopAt = c(2L, 1L)))
  expect_identical(sum(out3$indices[out3$id == 1L] %in% shared), 2L)
})

test_that("stopAt works with minSize, minSizeTries and jumpTries without leaving a stopped event running", {
  testInit(c("terra", "data.table", "withr"))
  side <- 101L
  ras <- stopRas(side)
  ring <- ringCells(side, 5101L, 6L)
  for (args in list(list(minSize = 400), list(minSize = 400, minSizeTries = 0L, jumpTries = 5L),
                    list(minSize = 20, minSizeTries = 10L))) {
    out <- withr::with_seed(2, do.call(spreadCpp, c(list(ras, loci = 5101L, spreadProb = 0.9,
                                                         stopCells = ring, stopEvent = rep(1L, length(ring)),
                                                         stopAt = 2L), args)))
    expect_identical(sum(out$indices %in% ring), 2L)
    expect_lt(nrow(out), 15^2)
  }
})

test_that("stop rule inputs are validated", {
  testInit(c("terra", "data.table", "withr"))
  ras <- stopRas(20L)
  f <- function(...) spreadCpp(ras, loci = c(5L, 300L), spreadProb = 0.5, ...)
  expect_error(f(stopCells = 10L), "needs `stopEvent` and `stopAt`")
  expect_error(f(stopEvent = 1L, stopAt = 1L), "need `stopCells`")
  expect_error(f(stopCells = 10L, stopEvent = c(1L, 2L), stopAt = 1L), "same length")
  expect_error(f(stopCells = 401L, stopEvent = 1L, stopAt = 1L), "within the landscape")
  expect_error(f(stopCells = 0L, stopEvent = 1L, stopAt = 1L), "within the landscape")
  expect_error(f(stopCells = NA, stopEvent = 1L, stopAt = 1L), "within the landscape")
  expect_error(f(stopCells = 10L, stopEvent = 3L, stopAt = 1L), "between 1 and length")
  expect_error(f(stopCells = 10L, stopEvent = 0L, stopAt = 1L), "between 1 and length")
  expect_error(f(stopCells = 10L, stopEvent = 1L, stopAt = c(1L, 1L, 1L)), "length 1 or length")
  expect_error(f(stopCells = 10L, stopEvent = 1L, stopAt = 0L), "1 or more")
  expect_error(f(stopCells = 10L, stopEvent = 1L, stopAt = NA), "1 or more")
  ## a scalar stopAt is recycled over events
  expect_no_error(f(stopCells = 10L, stopEvent = 1L, stopAt = 1L))
})
