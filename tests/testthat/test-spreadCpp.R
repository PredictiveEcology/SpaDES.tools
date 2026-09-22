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
