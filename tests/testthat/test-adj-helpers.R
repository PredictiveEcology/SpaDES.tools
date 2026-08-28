## Direct unit tests for the C++ neighbour-pair helpers introduced in
## adj_spread.cpp:
##   B. wrap prevention on left/right/top/bottom edges (incl. corner
##      diagonals for directions = 8)
##   E. RNG-state regression: confirm the helpers consume RNG identically
##      to the pre-Rcpp path by snapshotting .Random.seed after one
##      seeded spread() / spread2() iteration on a tiny raster
##   F. id-mixing guard: when adjPairsWithId is called with cells that
##      share neighbours but carry distinct ids, every emitted row must
##      carry its source cell's id (no crossing)
##
## The snapshot tests in test-spread-snapshots.R exercise the full call
## graph; these tests fail faster and localize the problem to the helper.

# B. wrap prevention -----------------------------------------------------

test_that("adjPairsMatrix does not wrap across left/right edges", {
  ## 10-col raster: cell 10 is rightmost of row 1; its naive +1 neighbour
  ## is cell 11, which is col 1 of row 2 — a horizontal wrap that must be
  ## dropped. Likewise cell 11 (leftmost of row 2) must not "see" cell 10.
  numCol <- 10L
  numCell <- 100L

  for (dirs in c(4L, 8L)) {
    rightEdge <- adjPairsMatrix(cells = 10L, numCol = numCol,
                                numCell = numCell, directions = dirs)
    expect_false(11L %in% rightEdge[, "to"],
                 info = sprintf("dirs=%d: cell 10's neighbour list must not include cell 11 (wrap)", dirs))
    if (dirs == 8L) {
      ## diagonals from cell 10 (row 1, col 10): topl/topr would be
      ## above the raster anyway, but botl=19 and botr=21 — cell 21 is
      ## col 1 of row 3, a wrap. cell 20 (= row 2, col 10) is the legit
      ## "bot" neighbour.
      expect_false(21L %in% rightEdge[, "to"],
                   info = "dirs=8: cell 10's botr would wrap to col 1 row 3 — must drop")
      expect_true(20L %in% rightEdge[, "to"],
                  info = "dirs=8: cell 10's bot neighbour (cell 20) must be present")
    }

    leftEdge <- adjPairsMatrix(cells = 11L, numCol = numCol,
                               numCell = numCell, directions = dirs)
    expect_false(10L %in% leftEdge[, "to"],
                 info = sprintf("dirs=%d: cell 11's neighbour list must not include cell 10 (wrap)", dirs))
    if (dirs == 8L) {
      ## cell 11 (row 2, col 1): topl=0 (off raster, also wrap), botl=20
      ## — cell 20 is row 2 col 10, a wrap.
      expect_false(20L %in% leftEdge[, "to"],
                   info = "dirs=8: cell 11's botl would wrap to col 10 row 2 — must drop")
    }
  }
})

test_that("adjPairsMatrix does not emit neighbours off the top/bottom", {
  numCol <- 10L
  numCell <- 100L

  for (dirs in c(4L, 8L)) {
    top <- adjPairsMatrix(cells = 5L, numCol = numCol,
                          numCell = numCell, directions = dirs)
    expect_true(all(top[, "to"] >= 1L),
                info = sprintf("dirs=%d: top-row cell must not produce to < 1", dirs))

    bot <- adjPairsMatrix(cells = 95L, numCol = numCol,
                          numCell = numCell, directions = dirs)
    expect_true(all(bot[, "to"] <= numCell),
                info = sprintf("dirs=%d: bottom-row cell must not produce to > numCell", dirs))
  }
})

test_that("adjPairsMatrix corners produce only the valid 3 (dir=8) or 2 (dir=4) neighbours", {
  ## 10x10 raster, top-left corner = cell 1.
  numCol <- 10L; numCell <- 100L

  tl4 <- adjPairsMatrix(cells = 1L, numCol = numCol, numCell = numCell, directions = 4L)
  expect_setequal(tl4[, "to"], c(2L, 11L))            # rig + bot only

  tl8 <- adjPairsMatrix(cells = 1L, numCol = numCol, numCell = numCell, directions = 8L)
  expect_setequal(tl8[, "to"], c(2L, 11L, 12L))       # rig, bot, botr

  ## bottom-right = cell 100
  br4 <- adjPairsMatrix(cells = 100L, numCol = numCol, numCell = numCell, directions = 4L)
  expect_setequal(br4[, "to"], c(99L, 90L))           # lef + top

  br8 <- adjPairsMatrix(cells = 100L, numCol = numCol, numCell = numCell, directions = 8L)
  expect_setequal(br8[, "to"], c(99L, 90L, 89L))      # lef, top, topl
})

test_that("adjPairsMatrix matches adj() exactly on edge / corner cells", {
  ## Belt-and-suspenders: the helper must agree with adj() row-for-row
  ## (including order) on every cell type that's prone to wrap bugs.
  numCol <- 10L; numCell <- 100L
  landscape <- terra::rast(terra::ext(0, numCol, 0, numCol), resolution = 1, vals = 0)

  ## one cell from each kind of position: 4 corners, 4 edge midpoints, interior
  cells <- c(
    1L, 10L, 91L, 100L,    # corners: tl, tr, bl, br
    5L, 50L, 51L, 95L,     # edges: top, right(col10 row5), left(col1 row6), bottom
    44L                    # interior
  )

  for (dirs in c(4L, 8L)) {
    for (i in seq_along(cells)) {
      expected <- adj(landscape, cells[i], directions = dirs, pairs = TRUE)
      got      <- adjPairsMatrix(cells = cells[i], numCol = numCol,
                                 numCell = numCell, directions = dirs)
      expect_identical(got, expected,
                       info = sprintf("cell=%d dirs=%d", cells[i], dirs))
    }
  }
})

# F. id-mixing guard -----------------------------------------------------

test_that("adjPairsWithId never crosses ids when source cells share neighbours", {
  ## cells 50 and 51 are adjacent in a 10-col raster, so cell 50's
  ## "right" neighbour is 51 and cell 51's "left" neighbour is 50 — they
  ## cross-reference. With distinct ids per cell, every emitted row must
  ## carry its source cell's id, never the other cell's id.
  numCol <- 10L; numCell <- 100L
  cells <- c(50L, 51L)
  ids   <- c(101L, 202L)

  for (dirs in c(4L, 8L)) {
    out <- adjPairsWithId(cells = cells, id = ids,
                          numCol = numCol, numCell = numCell,
                          directions = dirs)

    ## list shape and column lengths
    expect_named(out, c("from", "to", "id"))
    expect_true(length(out$from) == length(out$to))
    expect_true(length(out$from) == length(out$id))

    ## every emitted from=50 row must carry id 101; from=51 row must carry 202
    expect_true(all(out$id[out$from == 50L] == 101L),
                info = sprintf("dirs=%d: id crossed for from=50", dirs))
    expect_true(all(out$id[out$from == 51L] == 202L),
                info = sprintf("dirs=%d: id crossed for from=51", dirs))

    ## sanity: both ids actually appear (the cells are interior, must
    ## emit at least one neighbour each)
    expect_true(101L %in% out$id)
    expect_true(202L %in% out$id)
  }
})

test_that("adjPairsWithId matches adj(..., pairs=TRUE, id=...) row-for-row", {
  ## Direct comparison against the reference implementation.
  numCol <- 10L; numCell <- 100L
  landscape <- terra::rast(terra::ext(0, numCol, 0, numCol), resolution = 1, vals = 0)

  cells <- c(50L, 51L, 1L, 100L)
  ids   <- c(7L, 13L, 99L, 42L)

  for (dirs in c(4L, 8L)) {
    expected <- adj(landscape, cells, directions = dirs, pairs = TRUE, id = ids)
    got      <- adjPairsWithId(cells = cells, id = ids,
                               numCol = numCol, numCell = numCell,
                               directions = dirs)

    ## adj() returns a 3-col matrix; helper returns a list — compare per column
    expect_identical(got$from, as.integer(expected[, "from"]),
                     info = sprintf("dirs=%d: from column", dirs))
    expect_identical(got$to,   as.integer(expected[, "to"]),
                     info = sprintf("dirs=%d: to column", dirs))
    expect_identical(got$id,   as.integer(expected[, "id"]),
                     info = sprintf("dirs=%d: id column", dirs))
  }
})

test_that("adjPairsWithId handles empty cells gracefully", {
  out <- adjPairsWithId(cells = integer(0), id = integer(0),
                        numCol = 10L, numCell = 100L, directions = 8L)
  expect_named(out, c("from", "to", "id"))
  expect_length(out$from, 0L)
  expect_length(out$to,   0L)
  expect_length(out$id,   0L)
})

# E. RNG-state regression ------------------------------------------------

## After one seeded iteration, .Random.seed must match the value we
## recorded from the pre-Rcpp baseline. If a future change reorders
## row emission, this fails here even on a tiny single-cell raster —
## faster than waiting for the snapshot battery.
##
## Compare [-1] only: .Random.seed[1] packs the RNG kinds, and R-devel
## added a further field above sample.kind (10403 -> 110403 for the
## defaults), so the header word differs from snapshots taken under
## R <= 4.5 while the generator state is unchanged. It is the state
## that this test is about.

test_that("spread() consumes RNG identically to baseline (one iteration)", {
  ## testInit() chdir's into a tempdir, so capture the path first.
  snapDir <- normalizePath(testthat::test_path("_spread_snapshots"), mustWork = FALSE)
  testInit(c("terra", "withr"))

  ## Force base R sample.int (dqrng has its own state which we can't
  ## intercept via .Random.seed)
  if (requireNamespace("dqrng", quietly = TRUE)) {
    rN_orig <- base::requireNamespace
    rN_mock <- function(package, ...) {
      if (identical(package, "dqrng")) return(FALSE)
      rN_orig(package, ...)
    }
    base_env <- asNamespace("base")
    unlockBinding("requireNamespace", base_env)
    assign("requireNamespace", rN_mock, envir = base_env)
    lockBinding("requireNamespace", base_env)
    withr::defer({
      unlockBinding("requireNamespace", base_env)
      assign("requireNamespace", rN_orig, envir = base_env)
      lockBinding("requireNamespace", base_env)
    })
  }

  rngFile <- file.path(snapDir, "rng_state__spread.rds")
  skip_if_not(file.exists(rngFile), "RNG-state snapshot missing")

  ras <- terra::rast(terra::ext(0, 10, 0, 10), resolution = 1, vals = 1)
  set.seed(7L)
  invisible(spread(ras, loci = 55L, spreadProb = 0.30,
                   iterations = 1L, returnIndices = 1L))
  expect_identical(.Random.seed[-1], readRDS(rngFile)[-1])
})

test_that("spread2() consumes RNG identically to baseline (one iteration)", {
  snapDir <- normalizePath(testthat::test_path("_spread_snapshots"), mustWork = FALSE)
  testInit(c("terra", "withr"))

  rngFile <- file.path(snapDir, "rng_state__spread2.rds")
  skip_if_not(file.exists(rngFile), "RNG-state snapshot missing")

  ras <- terra::rast(terra::ext(0, 10, 0, 10), resolution = 1, vals = 1)
  set.seed(7L)
  invisible(spread2(ras, start = 55L, spreadProb = 0.30,
                    iterations = 1L, asRaster = FALSE))
  expect_identical(.Random.seed[-1], readRDS(rngFile)[-1])
})

# Input validation ------------------------------------------------------

test_that("the Rcpp helpers reject a malformed grid instead of crashing", {
  ## numCol = 0 made `c %% numCol` an integer division by zero, which took the
  ## whole R session down with SIGFPE rather than raising a condition.
  expect_error(adjPairsMatrix(cells = 1:3, numCol = 0L, numCell = 100L, directions = 8L),
               "numCol")
  expect_error(adjPairsWithId(cells = 1:3, id = 1:3, numCol = 0L, numCell = 100L,
                              directions = 8L), "numCol")

  expect_error(adjPairsMatrix(cells = 1:3, numCol = -1L, numCell = 100L, directions = 8L),
               "numCol")
  expect_error(adjPairsMatrix(cells = 1:3, numCol = 10L, numCell = 0L, directions = 8L),
               "numCell")
  expect_error(adjPairsMatrix(cells = 1:3, numCol = 200L, numCell = 100L, directions = 8L),
               "cannot exceed")
  expect_error(adjPairsMatrix(cells = 1:3, numCol = NA_integer_, numCell = 100L,
                              directions = 8L), "numCol")
})

test_that("the Rcpp helpers reject a directions other than 4 or 8", {
  ## This used to return an empty result, so spread() produced a degenerate
  ## answer with no error -- despite ?spread documenting "Can only be 4 or 8".
  for (dirs in list(16L, 0L, 5L, NA_integer_)) {
    expect_error(adjPairsMatrix(cells = 1:3, numCol = 10L, numCell = 100L,
                                directions = dirs), "directions")
    expect_error(adjPairsWithId(cells = 1:3, id = 1:3, numCol = 10L, numCell = 100L,
                                directions = dirs), "directions")
  }
})

test_that("adjPairsWithId requires id to match cells in length", {
  ## A short `id` was indexed past its end, so emitted rows carried whatever
  ## happened to sit in memory as their event id.
  expect_error(adjPairsWithId(cells = 40:49, id = c(1L, 2L), numCol = 10L,
                              numCell = 100L, directions = 8L),
               "same length")
  expect_error(adjPairsWithId(cells = 1:2, id = 1:5, numCol = 10L, numCell = 100L,
                              directions = 8L), "same length")

  ## the matched-length case, including empty, still works. Assert the
  ## contract rather than a row count: cells 40:49 straddle a row boundary, so
  ## the wrap filter legitimately drops some of the 10 x 8 candidate pairs.
  ok <- adjPairsWithId(cells = 40:49, id = 1:10, numCol = 10L,
                       numCell = 100L, directions = 8L)
  expect_length(ok$to, length(ok$from))
  expect_length(ok$id, length(ok$from))
  expect_gt(length(ok$id), 0L)
  expect_true(all(ok$id %in% 1:10))

  expect_length(adjPairsWithId(cells = integer(0), id = integer(0), numCol = 10L,
                               numCell = 100L, directions = 8L)$id, 0L)
})
