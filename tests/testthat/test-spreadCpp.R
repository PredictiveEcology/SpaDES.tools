## spreadCpp() is not bit-compatible with spread() by design, so these tests check
## the RULES it promises, plus the invariants that must hold for every run.

mkRas <- function(side = 100L) {
  terra::rast(terra::ext(0, side, 0, side), resolution = 1, vals = 0)
}

test_that("spreadCpp returns spread()'s returnIndices shape", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(60L)

  withr::local_seed(1)
  out <- spreadCpp(ras, loci = c(1000L, 2000L), spreadProb = 0.2)

  expect_s3_class(out, "data.table")
  expect_identical(names(out), c("id", "initialLocus", "indices", "active"))
  expect_identical(data.table::key(out), "id")
  expect_type(out$id, "integer")
  expect_type(out$initialLocus, "integer")
  expect_type(out$indices, "integer")
  expect_false(any(out$active))
  ## initialLocus is the starting cell of that row's fire
  expect_identical(unique(out$initialLocus[out$id == 1L]), 1000L)
  expect_identical(unique(out$initialLocus[out$id == 2L]), 2000L)
})

test_that("a cell is spread to at most once, and never re-spread", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(80L)
  ## many ignitions and a high probability, so fires collide constantly
  withr::local_seed(2)
  loci <- sample.int(terra::ncell(ras), 60L)
  for (s in 1:10) {
    withr::with_seed(s, {
      out <- spreadCpp(ras, loci = loci, spreadProb = 0.5)
    })
    expect_identical(anyDuplicated(out$indices), 0L)
  }
})

test_that("no fire exceeds its own maxSize, and each is capped independently", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(120L)
  caps <- c(5, 25, 100, 1)
  withr::local_seed(3)
  loci <- c(2000L, 4000L, 8000L, 12000L)
  for (s in 1:10) {
    withr::with_seed(s, {
      out <- spreadCpp(ras, loci = loci, spreadProb = 0.9, maxSize = caps)
    })
    sizes <- out[, .N, by = "id"]
    expect_true(all(sizes$N <= caps[sizes$id]))
    ## spreadProb 0.9 on an open landscape: every fire should reach its cap
    expect_identical(sort(sizes$N), sort(as.integer(caps)))
  }
})

test_that("spreadProb bounds the spread: 0 burns only ignitions, 1 fills the landscape", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(40L)

  withr::local_seed(4)
  none <- spreadCpp(ras, loci = c(100L, 700L), spreadProb = 0)
  expect_identical(nrow(none), 2L)
  expect_setequal(none$indices, c(100L, 700L))

  all <- spreadCpp(ras, loci = 800L, spreadProb = 1)
  expect_identical(nrow(all), as.integer(terra::ncell(ras)))
  expect_identical(anyDuplicated(all$indices), 0L)
})

test_that("NA spreadProb cells are unburnable, so they fence a fire in", {
  testInit(c("terra", "data.table", "withr"))
  side <- 21L
  ras <- mkRas(side)
  n <- terra::ncell(ras)
  P <- rep(1, n)
  ## a ring of NA around the middle 5x5 block boxes the ignition in
  inBlock <- function(r, c) r >= 9 & r <= 13 & c >= 9 & c <= 13
  rows <- rep(1:side, each = side); cols <- rep(1:side, times = side)
  P[!inBlock(rows, cols)] <- NA_real_
  centre <- (11L - 1L) * side + 11L

  withr::local_seed(5)
  out <- spreadCpp(ras, loci = centre, spreadProb = P)
  expect_identical(nrow(out), 25L)            # exactly the 5x5 block
  expect_true(all(!is.na(P[out$indices])))
})

test_that("spread is outward: growth is contiguous from the ignition", {
  testInit(c("terra", "data.table", "withr"))
  side <- 41L
  ras <- mkRas(side)
  centre <- (21L - 1L) * side + 21L

  withr::local_seed(6)
  out <- spreadCpp(ras, loci = centre, spreadProb = 1, maxSize = 200)
  expect_identical(nrow(out), 200L)

  ## With spreadProb 1 and 8 directions, generation g reaches exactly the cells
  ## at Chebyshev distance g, so after g generations the burn is the solid
  ## (2g+1)^2 block. 6 generations give 169 cells and 7 would give 225, so a
  ## maxSize of 200 must contain the whole distance-6 block and nothing beyond
  ## distance 7. That fails if spread jumps a ring, skips a diagonal, or spreads
  ## from cells that are not the newest generation.
  rr <- (out$indices - 1L) %/% side
  cc <- (out$indices - 1L) %% side
  cheb <- pmax(abs(rr - 20L), abs(cc - 20L))
  expect_identical(sum(cheb <= 6L), 169L)     # the distance-6 block, complete
  expect_identical(max(cheb), 7L)             # and nothing further out
  expect_identical(sum(cheb == 7L), 31L)      # the 200 - 169 remainder
})

test_that("directions = 4 spreads only orthogonally", {
  testInit(c("terra", "data.table", "withr"))
  side <- 21L
  ras <- mkRas(side)
  centre <- (11L - 1L) * side + 11L

  withr::local_seed(7)
  out <- spreadCpp(ras, loci = centre, spreadProb = 1, maxSize = 5, directions = 4L)
  rr <- (out$indices - 1L) %/% side; cc <- (out$indices - 1L) %% side
  ## after one generation of 4-direction spread from the centre: the plus shape
  expect_identical(nrow(out), 5L)
  expect_true(all(abs(rr - 10L) + abs(cc - 10L) <= 1L))
})

test_that("spreadCpp does not wrap across the left/right edge", {
  testInit(c("terra", "data.table", "withr"))
  side <- 20L
  ras <- mkRas(side)
  ## ignite in column 1; with spreadProb 1 and one generation it must not reach
  ## column `side` of the row above/below
  leftEdge <- (10L - 1L) * side + 1L

  withr::local_seed(8)
  out <- spreadCpp(ras, loci = leftEdge, spreadProb = 1, maxSize = 6, directions = 8L)
  cc <- (out$indices - 1L) %% side + 1L
  expect_false(any(cc == side))
})

test_that("more ignitions of the same fire and bad input are handled", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(30L)

  ## duplicate loci: the second cannot claim a cell the first already has
  withr::local_seed(9)
  out <- spreadCpp(ras, loci = c(400L, 400L), spreadProb = 0)
  expect_identical(nrow(out), 1L)

  expect_error(spreadCpp(ras, loci = 1L, spreadProb = c(0.1, 0.2)),
               "length 1 or terra::ncell", fixed = TRUE)
  expect_error(spreadCpp(ras, loci = c(1L, 2L), spreadProb = 0.1, maxSize = c(1, 2, 3)),
               "length 1 or length(loci)", fixed = TRUE)
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0.1, directions = 6L),
               "must be 4 or 8")
  expect_error(spreadCpp(ras, loci = NA_integer_, spreadProb = 0.1),
               "must not contain NA")
  expect_error(spreadCpp(ras, loci = 0L, spreadProb = 0.1),
               "outside the landscape")
})

test_that("spread()'s unsupported arguments are rejected, not silently ignored", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(30L)
  ## The documented Limitations section promises an error for each of these. It
  ## comes from spreadCpp() having no `...`, so a typo or a carried-over argument
  ## cannot quietly change the model.
  unsupported <- list(
    allowOverlap = TRUE, returnDistances = TRUE, circle = TRUE,
    circleMaxRadius = 10, asymmetry = 2, asymmetryAngle = 90,
    neighProbs = c(0.7, 0.3), relativeSpreadProb = TRUE,
    stopRule = function(...) TRUE, stopRuleBehavior = "excludePixel",
    exactSizes = TRUE, persistence = 0.5, mask = NULL,
    spreadState = NULL, spreadProbLater = 0.1, torus = TRUE,
    plot.it = TRUE, id = TRUE, returnIndices = 2L
  )
  for (nm in names(unsupported)) {
    args <- c(list(ras, loci = 400L, spreadProb = 0.2), unsupported[nm])
    expect_error(do.call(spreadCpp, args), "unused argument", info = nm)
  }
})

test_that("iterations caps the number of generations", {
  testInit(c("terra", "data.table", "withr"))
  side <- 51L
  ras <- mkRas(side)
  centre <- (26L - 1L) * side + 26L

  withr::local_seed(10)
  out <- spreadCpp(ras, loci = centre, spreadProb = 1, iterations = 3)
  ## 3 generations of 8-direction spread from one cell: a 7x7 block
  expect_identical(nrow(out), 49L)
})

test_that("catch probability is 1-(1-p)^k, checked against theory", {
  testInit(c("terra", "data.table", "withr"))
  skip_on_cran()
  ## The documented rule is one draw per (burning cell, unburned neighbour) pair against the
  ## TARGET cell's probability, so a cell with k burning neighbours catches with probability
  ## 1-(1-p)^k in a generation. Checked against the analytic value rather than against spread(),
  ## because two implementations of the same misunderstanding would agree with each other.
  ## Seeded, so this is deterministic rather than a flaky statistical test.
  side <- 31L
  ras <- mkRas(side)
  cellOf <- function(r, cc) (r - 1L) * side + cc
  NREP <- 1500L

  ## k = 1: the 8 neighbours of a lone ignition, one generation
  ## k = 2: the cell sandwiched between two ignitions two columns apart
  ## k = 3: the cell below three ignitions sitting side by side
  probes <- list(
    list(k = 1L, loci = cellOf(16L, 16L), target = NULL),
    list(k = 2L, loci = c(cellOf(16L, 15L), cellOf(16L, 17L)), target = cellOf(16L, 16L)),
    list(k = 3L, loci = c(cellOf(15L, 15L), cellOf(15L, 16L), cellOf(15L, 17L)),
         target = cellOf(16L, 16L))
  )

  withr::local_seed(20260921)
  for (p in c(0.1, 0.5)) {
    for (pr in probes) {
      hits <- replicate(NREP, {
        o <- spreadCpp(ras, loci = pr$loci, spreadProb = p, iterations = 1)
        if (is.null(pr$target)) nrow(o) - length(pr$loci) else as.integer(pr$target %in% o$indices)
      })
      n <- if (is.null(pr$target)) NREP * 8L else NREP
      obs <- sum(hits) / n
      expect <- 1 - (1 - p)^pr$k
      ## 4 binomial standard errors: wide enough never to fire by chance, tight enough that
      ## drawing once per cell instead of once per pair (which would give p, not 1-(1-p)^k)
      ## is caught at k = 2 and k = 3.
      tol <- 4 * sqrt(expect * (1 - expect) / n)
      expect_lt(abs(obs - expect), tol,
                label = sprintf("p=%.2f k=%d obs=%.4f expected=%.4f", p, pr$k, obs, expect))
    }
  }
})

test_that("spreadCpp agrees with spread() on how much burns", {
  testInit(c("terra", "data.table", "withr"))
  skip_on_cran()
  ## Not bit-identical by design, so compare the distributions. A rule error
  ## (wrong neighbour count, double draws, a missed edge) moves these a long way.
  side <- 300L
  ras <- mkRas(side)
  n <- terra::ncell(ras)
  withr::local_seed(11)
  P <- rep(0.2, n)
  loci <- sample.int(n, 40L)

  burnR <- burnC <- numeric(25)
  for (s in 1:25) {
    withr::with_seed(s, {
      burnR[s] <- nrow(spread(ras, loci = loci, spreadProb = P, returnIndices = TRUE,
                              allowOverlap = FALSE, quick = TRUE))
    })
    withr::with_seed(s + 1000L, {
      burnC[s] <- nrow(spreadCpp(ras, loci = loci, spreadProb = P))
    })
  }
  ## generous, because these are heavy-tailed; a broken rule misses by much more
  expect_gt(stats::wilcox.test(burnR, burnC)$p.value, 0.01)
  expect_lt(abs(log2(median(burnC) / median(burnR))), 0.5)
})

## minSize: while a fire is below minSize, all its cells stay active and draw again
## (persistence), so it reaches minSize in a patch shaped by spreadProb. Used by
## fireSense to start every escaped fire at the escape size.

test_that("minSize = 0 (the default) reproduces the draws exactly as before it existed", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 30, 0, 30), resolution = 1, vals = 0)
  ## recorded with SpaDES.tools 2.1.3.9008 (development before minSize), set.seed(42)
  expected <- list(
    id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L),
    indices = c(200L, 230L, 199L, 259L, 170L, 228L, 229L, 141L, 110L, 140L, 109L, 139L, 169L, 171L, 168L, 172L, 202L, 80L, 108L, 138L, 137L, 167L, 49L, 143L, 19L, 142L, 174L, 136L, 111L, 135L, 165L, 166L, 203L, 205L, 48L, 50L, 104L, 106L, 164L, 20L, 650L))
  for (ms in list(NULL, 0)) {
    withr::local_seed(42)
    out <- if (is.null(ms)) spreadCpp(ras, loci = c(200L, 650L), spreadProb = 0.25, maxSize = 40)
           else spreadCpp(ras, loci = c(200L, 650L), spreadProb = 0.25, maxSize = 40, minSize = ms)
    expect_identical(out$id, expected$id)
    expect_identical(out$indices, expected$indices)
  }
})

test_that("with low spreadProb a fire still reaches minSize where fuel allows", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(50L)
  for (ms in c(9, 30)) {
    withr::with_seed(ms, out <- spreadCpp(ras, loci = 1275L, spreadProb = 0.05, minSize = ms, maxSize = ms))
    expect_identical(nrow(out), as.integer(ms))
  }
  ## without minSize the same fires mostly die small
  sizes <- vapply(1:40, function(s) withr::with_seed(s, nrow(spreadCpp(ras, loci = 1275L, spreadProb = 0.05))), 1L)
  expect_lt(median(sizes), 9)
})

test_that("the minSize patch is contiguous and follows high spreadProb", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(60L)
  cols <- terra::colFromCell(ras, seq_len(terra::ncell(ras)))
  sp <- ifelse(cols <= 30, 0.9, 0.1)             # left half 0.9, right half 0.1
  start <- terra::cellFromRowCol(ras, 30, 30)    # on the boundary, left side
  share <- vapply(1:30, function(s) {
    out <- withr::with_seed(s, spreadCpp(ras, loci = start, spreadProb = sp, minSize = 30, maxSize = 30))
    r <- terra::rast(ras); r[] <- NA; r[out$indices] <- 1
    expect_identical(length(unique(stats::na.omit(terra::values(terra::patches(r, directions = 8))[, 1]))), 1L)
    mean(terra::colFromCell(ras, out$indices) <= 30)
  }, 1)
  expect_gt(mean(share), 0.8)
})

test_that("a fire boxed in by NA or 0 stops below minSize", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(20L)
  patch <- c(210L, 211L, 212L, 231L)             # 4 connected burnable cells
  for (fill in c(NA_real_, 0)) {
    sp <- rep(fill, terra::ncell(ras)); sp[patch] <- 0.3
    out <- withr::with_seed(1, spreadCpp(ras, loci = 210L, spreadProb = sp, minSize = 9))
    expect_setequal(out$indices, patch)          # it burns all it can, then stops
  }
})

test_that("minSize is per fire", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(80L)
  ms <- c(1, 9, 30)
  out <- withr::with_seed(5, spreadCpp(ras, loci = c(820L, 4020L, 5660L), spreadProb = 0.1,
                                       minSize = ms, maxSize = ms))
  expect_identical(as.integer(out[, .N, by = "id"]$N), c(1L, 9L, 30L))
  ## and fires keep spreading normally after reaching it
  out2 <- withr::with_seed(6, spreadCpp(ras, loci = 3240L, spreadProb = 1, minSize = 9, maxSize = 200))
  expect_identical(nrow(out2), 200L)
})

test_that("minSize must not exceed maxSize, and is validated", {
  testInit(c("terra", "data.table", "withr"))
  ras <- mkRas(40L)
  expect_error(spreadCpp(ras, loci = 820L, spreadProb = 0.2, minSize = 10, maxSize = 9), "must not exceed")
  expect_error(spreadCpp(ras, loci = c(1L, 2L), spreadProb = 0.2, minSize = c(1, 2, 3)), "length")
  expect_error(spreadCpp(ras, loci = 820L, spreadProb = 0.2, minSize = -1), "0 or more")
})

## jumping: only for a fire stuck under minSize (nothing burnable next to it)
islandLandscape <- function() {
  ras <- terra::rast(terra::ext(0, 30, 0, 30), resolution = 1, vals = 0)
  rc <- terra::rowColFromCell(ras, seq_len(terra::ncell(ras)))
  island <- rc[, 1] %in% 14:15 & rc[, 2] %in% 5:6          # 4 burnable cells
  big <- rc[, 2] >= 10                                      # 3-column gap (7:9) of unburnable
  sp <- ifelse(island | big, 0.5, NA_real_)
  list(ras = ras, sp = sp, start = terra::cellFromRowCol(ras, 14, 5), island = which(island))
}

test_that("jumpTries = 0 changes nothing", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 30, 0, 30), resolution = 1, vals = 0)
  a <- withr::with_seed(42, spreadCpp(ras, loci = c(200L, 650L), spreadProb = 0.25, maxSize = 40))
  b <- withr::with_seed(42, spreadCpp(ras, loci = c(200L, 650L), spreadProb = 0.25, maxSize = 40,
                                      jumpTries = 0L))
  expect_identical(a, b)
  isl <- islandLandscape()
  c1 <- withr::with_seed(7, spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, minSize = 9))
  c2 <- withr::with_seed(7, spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, minSize = 9,
                                      jumpTries = 0L))
  expect_identical(c1, c2)
})

test_that("a fire stuck on an island stops below minSize, and jumps off it with jumpTries", {
  testInit(c("terra", "data.table", "withr"))
  isl <- islandLandscape()
  no <- withr::with_seed(1, spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, minSize = 9))
  expect_setequal(no$indices, isl$island)                    # the whole island, then stuck
  reached <- vapply(1:30, function(s) {
    out <- withr::with_seed(s, spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp,
                                         minSize = 9, maxSize = 9, jumpTries = 20L, jumpMeanDist = 3))
    expect_false(anyNA(isl$sp[out$indices]))                # never onto unburnable cells
    nrow(out)
  }, 1L)
  ## 20 tries at a 3-cell mean distance get across the 3-column gap in most runs (23 of 30 here);
  ## a run whose tries all miss stops at the island, as documented
  expect_gte(mean(reached == 9L), 0.6)
  expect_true(all(reached %in% c(4L, 9L)))
  ## reproducible for a fixed seed
  a <- withr::with_seed(3, spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, minSize = 9,
                                     jumpTries = 20L))
  b <- withr::with_seed(3, spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, minSize = 9,
                                     jumpTries = 20L))
  expect_identical(a, b)
  expect_error(spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, jumpTries = -1L), "0 or more")
  expect_error(spreadCpp(isl$ras, loci = isl$start, spreadProb = isl$sp, jumpTries = 2L, jumpMeanDist = 0),
               "positive")
})
