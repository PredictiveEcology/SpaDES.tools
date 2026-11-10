## spreadCpp(link =): spreadProb is a linear predictor and the probability of a cell is computed
## only when a fire reaches it. It must give exactly the cells of the same spread with the full
## probability vector, for the same seed.

powerLawRef <- function(x, pc, gamma, floor, shape) {
  pmax(pc - (pc - floor) * exp(-sign(x) * abs(x / gamma)^shape), 0)
}

test_that("link = 'powerLaw' burns exactly the cells of the full spreadProb vector", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 150, 0, 150), resolution = 1, vals = 0)
  n <- terra::ncell(ras)
  withr::local_seed(11)
  lp <- stats::rnorm(n, 0.5, 1.2)
  lp[sample.int(n, n %/% 5)] <- NA          # unburnable cells
  loci <- sample.int(n, 40L)
  lp[loci[1:3]] <- NA                       # ignitions on unburnable cells still start
  for (shape in c(0.5, 1, 2)) {
    pars <- c(pc = 0.26, gamma = 1.9, floor = 0.12, shape = shape)
    for (s in 1:4) {
      eps <- withr::with_seed(100 + s, stats::rnorm(1))
      full <- powerLawRef(lp + eps, pars[["pc"]], pars[["gamma"]], pars[["floor"]], shape)
      full[loci] <- 1
      ## minSize, minSizeTries and jumpTries read spreadProb in all three places it is read
      args <- list(ras, loci = loci, maxSize = Inf, minSize = 9, minSizeTries = 5L, jumpTries = 20L,
                   jumpMeanDist = 3)
      a <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = full))))
      b <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = lp, link = "powerLaw",
                                                                linkPars = pars, linkOffset = eps))))
      expect_identical(b, a)
      expect_identical(attr(b, "minSizeTries"), attr(a, "minSizeTries"))
      expect_gt(nrow(a), length(loci))
    }
  }
})

test_that("link = NULL (the default) gives the results of a call without the link arguments", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 80, 0, 80), resolution = 1, vals = 0)
  sp <- withr::with_seed(3, stats::runif(terra::ncell(ras), 0, 0.3))
  a <- withr::with_seed(4, spreadCpp(ras, loci = c(100L, 3000L), spreadProb = sp, minSize = 5))
  b <- withr::with_seed(4, spreadCpp(ras, loci = c(100L, 3000L), spreadProb = sp, minSize = 5,
                                     link = NULL, linkPars = NULL, linkOffset = 0))
  expect_identical(b, a)
})

test_that("spreadCpp(link =) checks its arguments", {
  testInit(c("terra", "data.table"))
  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1, vals = 0)
  pars <- c(pc = 0.26, gamma = 1.9, floor = 0.12, shape = 1)
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "probit", linkPars = pars), "link")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw"), "linkPars")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw", linkPars = pars[-2]), "gamma")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw", linkPars = pars,
                         linkOffset = c(1, 2)), "linkOffset")
  expect_error(spreadCpp(ras, loci = 1:3, spreadProb = 0, link = "powerLaw", linkPars = pars,
                         linkOffset = c(1, 2)), "linkOffset")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw", linkPars = pars,
                         linkOffset = NA_real_), "linkOffset")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw",
                         linkPars = matrix(pars, 3, 4, byrow = TRUE, dimnames = list(NULL, names(pars)))), "one row per cell")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLaw",
                         linkPars = matrix(pars, terra::ncell(ras), 4, byrow = TRUE)), "pc")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0.1, linkPars = pars), "link")
})

test_that("a vector of equal linkOffsets is the scalar, for the same seed", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 100, 0, 100), resolution = 1, vals = 0)
  n <- terra::ncell(ras)
  lp <- withr::with_seed(5, stats::rnorm(n, 0.5, 1))
  loci <- withr::with_seed(6, sample.int(n, 20L))
  pars <- c(pc = 0.26, gamma = 1.9, floor = 0.12, shape = 1)
  args <- list(ras, loci = loci, spreadProb = lp, link = "powerLaw", linkPars = pars, minSize = 5,
               minSizeTries = 3L, jumpTries = 10L, jumpMeanDist = 3)
  a <- withr::with_seed(1, do.call(spreadCpp, c(args, list(linkOffset = 0.7))))
  b <- withr::with_seed(1, do.call(spreadCpp, c(args, list(linkOffset = rep(0.7, length(loci))))))
  expect_identical(b, a)
  expect_gt(nrow(a), length(loci))
})

test_that("each fire uses its own linkOffset", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 200, 0, 100), resolution = 1, vals = 0)
  pars <- c(pc = 0.5, gamma = 1, floor = 0, shape = 1)
  loci <- terra::cellFromRowCol(ras, 50, c(50, 150))
  sizes <- vapply(1:20, function(s) {
    out <- withr::with_seed(s, spreadCpp(ras, loci = loci, spreadProb = 0, link = "powerLaw",
                                         linkPars = pars, linkOffset = c(-3, 3)))
    as.numeric(table(factor(out$id, 1:2)))
  }, numeric(2))
  expect_true(all(sizes[1, ] < sizes[2, ]))
  expect_lt(median(sizes[1, ]), 50)
  expect_gt(median(sizes[2, ]), 500)
})

test_that("a linkPars matrix with identical rows is the vector, and rows differ by cell", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 100, 0, 100), resolution = 1, vals = 0)
  n <- terra::ncell(ras)
  lp <- withr::with_seed(5, stats::rnorm(n, 0.5, 1))
  loci <- withr::with_seed(6, sample.int(n, 20L))
  pars <- c(pc = 0.26, gamma = 1.9, floor = 0.12, shape = 1)
  m <- matrix(rep(pars[c("shape", "floor", "gamma", "pc")], each = n), n, 4,
              dimnames = list(NULL, c("shape", "floor", "gamma", "pc")))   # column order does not matter
  args <- list(ras, loci = loci, spreadProb = lp, link = "powerLaw", minSize = 5, minSizeTries = 3L,
               jumpTries = 10L, jumpMeanDist = 3, linkOffset = rnorm(length(loci)))
  a <- withr::with_seed(1, do.call(spreadCpp, c(args, list(linkPars = pars))))
  b <- withr::with_seed(1, do.call(spreadCpp, c(args, list(linkPars = m))))
  expect_identical(b, a)

  ## pc = floor = 0 is unburnable: the left half burns nothing but its starting cells
  left <- terra::colFromCell(ras, seq_len(n)) <= 50
  m0 <- m
  m0[left, c("pc", "floor")] <- 0
  loci2 <- terra::cellFromRowCol(ras, 50, c(10, 60))
  out <- withr::with_seed(2, spreadCpp(ras, loci = loci2, spreadProb = 5, link = "powerLaw", linkPars = m0))
  expect_identical(sum(out$id == 1), 1L)
  expect_gt(sum(out$id == 2), 100)
  expect_false(any(out$id == 2 & left[out$indices]))
})
