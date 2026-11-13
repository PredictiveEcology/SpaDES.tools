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

## The R forms of the links 2 and 3 (fireSenseUtils::powerLawSmoothLink() and richardsLink()), which the C++ must match
powerLawSmoothRef <- function(x, pc, gamma, floor, shape, lower) {
  p <- powerLawRef(pmax(x, 0), pc, gamma, floor, shape)
  neg <- which(x < 0)
  p[neg] <- if (lower == floor) floor else
    lower + (floor - lower) * exp(-((pc - floor) / (floor - lower)) * abs(x[neg] / gamma)^shape)
  p
}

richardsRef <- function(x, pc, lower, nu, B) {
  e <- exp(-B * x)
  lower + (pc - lower) * if (nu == 0) exp(-e) else exp(-log1p(nu * e) / nu)
}

## every link's pars on one grid, as named vectors for spreadCpp() and as the engine's order
linkCases <- function() {
  x <- c(-1e3, -50, -20, -5, -1, -1e-8, 0, 1e-8, 0.3, 1, 5, 20, 1e3)
  cases <- list()
  for (lower in c(0, 0.05, 0.13)) {
    for (shape in c(0.4, 1, 2)) {
      pars <- c(pc = 0.26, gamma = 1.9, floor = 0.13, shape = shape, lower = lower)
      cases[[length(cases) + 1L]] <- list(link = "powerLawSmooth", id = 2L, pars = pars, x = x,
                                         ref = powerLawSmoothRef(x, 0.26, 1.9, 0.13, shape, lower))
    }
    for (nu in c(0, 0.5, 1, 3)) {
      pars <- c(pc = 0.26, lower = lower, nu = nu, B = 1 / 1.9)
      cases[[length(cases) + 1L]] <- list(link = "richards", id = 3L, pars = pars, x = x,
                                         ref = richardsRef(x, 0.26, lower, nu, 1 / 1.9))
    }
  }
  cases
}

test_that("links 2 and 3 are the R forms, double for double, on a grid of x", {
  for (cs in linkCases()) {
    expect_identical(linkProbCpp(cs$id, cs$x, cs$pars), cs$ref)
  }
  ## link 1 is unchanged
  x <- c(-50, -1, 0, 0.5, 3)
  expect_identical(linkProbCpp(1L, x, c(0.26, 1.9, 0.13, 1)), powerLawRef(x, 0.26, 1.9, 0.13, 1))
})

test_that("links 2 and 3 have a lower asymptote; link 1 falls to 0", {
  x <- c(-50, -20)
  expect_identical(linkProbCpp(1L, x, c(0.26, 1.9, 0.13, 1)), c(0, 0))
  for (id in 2:3) {
    pars <- list(c(0.26, 1.9, 0.13, 1, 0.13), c(0.26, 0.13, 0.5, 1 / 1.9))[[id - 1L]]
    expect_equal(linkProbCpp(id, x, pars), c(0.13, 0.13))
    pars0 <- list(c(0.26, 1.9, 0.13, 1, 0), c(0.26, 0, 0.5, 1 / 1.9))[[id - 1L]]
    expect_true(all(linkProbCpp(id, x, pars0) > 0))
    expect_true(all(linkProbCpp(id, c(-1e3, 1e3), pars0) >= 0))
  }
  ## continuous at 0: link 2 is the power law's floor there
  expect_equal(linkProbCpp(2L, c(-1e-12, 0, 1e-12), c(0.26, 1.9, 0.13, 1, 0)), rep(0.13, 3), tolerance = 1e-9)
})

test_that("links 2 and 3 burn exactly the cells of the full spreadProb vector, with a scalar or per-fire linkOffset", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 120, 0, 120), resolution = 1, vals = 0)
  n <- terra::ncell(ras)
  withr::local_seed(12)
  lp <- stats::rnorm(n, 0.3, 1.5)
  lp[sample.int(n, n %/% 6)] <- NA
  loci <- sample.int(n, 25L)
  args <- list(ras, loci = loci, maxSize = Inf, minSize = 9, minSizeTries = 5L, jumpTries = 20L, jumpMeanDist = 3)
  for (cs in linkCases()[c(1, 5, 8, 12, 18, 20)]) {
    pr <- function(x) if (cs$id == 2L) powerLawSmoothRef(x, cs$pars[["pc"]], cs$pars[["gamma"]], cs$pars[["floor"]],
                                                          cs$pars[["shape"]], cs$pars[["lower"]]) else
      richardsRef(x, cs$pars[["pc"]], cs$pars[["lower"]], cs$pars[["nu"]], cs$pars[["B"]])
    for (s in 1:3) {
      eps <- withr::with_seed(100 + s, stats::rnorm(1))
      full <- pr(lp + eps)
      full[loci] <- 1
      a <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = full))))
      b <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = lp, link = cs$link, linkPars = cs$pars,
                                                                linkOffset = eps))))
      expect_identical(b, a)
      ## a per-fire offset, all equal, is the scalar
      d <- withr::with_seed(s, do.call(spreadCpp, c(args, list(spreadProb = lp, link = cs$link, linkPars = cs$pars,
                                                                linkOffset = rep(eps, length(loci))))))
      expect_identical(d, a)
      expect_gt(nrow(a), length(loci))
    }
  }
})

test_that("each fire uses its own linkOffset with links 2 and 3", {
  testInit(c("terra", "data.table", "withr"))
  ras <- terra::rast(terra::ext(0, 200, 0, 100), resolution = 1, vals = 0)
  loci <- terra::cellFromRowCol(ras, 50, c(50, 150))
  for (pars in list(list("powerLawSmooth", c(pc = 0.5, gamma = 1, floor = 0.13, shape = 1, lower = 0)),
                    list("richards", c(pc = 0.5, lower = 0, nu = 0.5, B = 1)))) {
    sizes <- vapply(1:10, function(s) {
      out <- withr::with_seed(s, spreadCpp(ras, loci = loci, spreadProb = 0, link = pars[[1]], linkPars = pars[[2]],
                                           linkOffset = c(-6, 6)))
      as.numeric(table(factor(out$id, 1:2)))
    }, numeric(2))
    expect_true(all(sizes[1, ] < sizes[2, ]))
  }
})

test_that("a fire on a landscape of x = -20 spreads with a lower asymptote of 0.13 on links 2 and 3, and not on link 1", {
  testInit(c("terra", "withr"))
  ras <- terra::rast(terra::ext(0, 100, 0, 100), resolution = 1, vals = 0)
  loci <- terra::cellFromRowCol(ras, 50, 50)
  sz <- function(link, pars) mean(vapply(1:20, function(s)
    nrow(withr::with_seed(s, spreadCpp(ras, loci = loci, spreadProb = -20, link = link, linkPars = pars))), numeric(1)))
  expect_identical(sz("powerLaw", c(pc = 0.26, gamma = 1.9, floor = 0.13, shape = 1)), 1)
  expect_gt(sz("powerLawSmooth", c(pc = 0.26, gamma = 1.9, floor = 0.13, shape = 1, lower = 0.13)), 2)
  expect_gt(sz("richards", c(pc = 0.26, lower = 0.13, nu = 0.5, B = 1 / 1.9)), 2)
})

test_that("link 1 is unchanged: a fixed-seed snapshot", {
  testInit(c("terra", "withr"))
  ras <- terra::rast(terra::ext(0, 120, 0, 120), resolution = 1, vals = 0)
  n <- terra::ncell(ras)
  set.seed(21)
  lp <- stats::rnorm(n, 0.3, 1.5)
  lp[sample.int(n, n %/% 6)] <- NA
  loci <- sample.int(n, 30L)
  out <- withr::with_seed(7, spreadCpp(ras, loci = loci, spreadProb = lp, link = "powerLaw",
                                       linkPars = c(pc = 0.26, gamma = 1.9, floor = 0.13, shape = 0.5),
                                       linkOffset = stats::rnorm(30, 0, 0.3), minSize = 9, minSizeTries = 5L,
                                       jumpTries = 20L, jumpMeanDist = 3))
  expect_identical(nrow(out), 428L)
  expect_identical(sum(as.numeric(out$indices)), 2828714)
  expect_identical(sum(as.numeric(out$id) * seq_len(nrow(out))), 1952142)
})

test_that("spreadCpp(link =) checks the parameters of links 2 and 3", {
  testInit(c("terra", "data.table"))
  ras <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1, vals = 0)
  sm <- c(pc = 0.26, gamma = 1.9, floor = 0.13, shape = 1, lower = 0.05)
  rc <- c(pc = 0.26, lower = 0.05, nu = 0.5, B = 0.5)
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLawSmooth", linkPars = sm[-5]), "lower")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "richards", linkPars = rc[-3]), "nu")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLawSmooth", linkPars = replace(sm, "lower", 0.2)), "lower")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "powerLawSmooth", linkPars = replace(sm, "lower", -0.1)), "lower")
  expect_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "richards", linkPars = replace(rc, "nu", -1)), "nu")
  expect_no_error(spreadCpp(ras, loci = 1L, spreadProb = 0, link = "richards", linkPars = rc))
  ## a per-cell matrix works and equals the vector
  m <- matrix(rc, terra::ncell(ras), 4, byrow = TRUE, dimnames = list(NULL, names(rc)))
  a <- withr::with_seed(1, spreadCpp(ras, loci = 1:3, spreadProb = 0, link = "richards", linkPars = rc))
  b <- withr::with_seed(1, spreadCpp(ras, loci = 1:3, spreadProb = 0, link = "richards", linkPars = m))
  expect_identical(b, a)
  sm5 <- matrix(sm, terra::ncell(ras), 5, byrow = TRUE, dimnames = list(NULL, names(sm)))
  expect_identical(withr::with_seed(1, spreadCpp(ras, loci = 1:3, spreadProb = -3, link = "powerLawSmooth", linkPars = sm5)),
                   withr::with_seed(1, spreadCpp(ras, loci = 1:3, spreadProb = -3, link = "powerLawSmooth", linkPars = sm)))
})
