#' Resample
#'
#' A version of sample that doesn't have awkward behaviour when \code{length(x) == 1}.
#' Vectorized version of \code{sample} using \code{Vectorize}
#'
#' Intended for internal use only. \code{size} is vectorized.
#' @return A random permutation, as in \code{sample}, but with \code{size} vectorized.
#' @keywords internal
#' @inheritParams base::sample
#'
sampleV <- Vectorize("sample", "size", SIMPLIFY = FALSE)

#' Adapted directly from the \code{\link[base]{sample}} help file.
#'
#' @inheritParams base::sample
#'
#' @param ... Passed to \code{\link[base]{sample}}
#'
resample <- function(x, ...) x[sample.int(length(x), ...)]

#' \code{resampleZeroProof} is a version that works even if sum of all probabilities passed to
#' \code{sample.int} is zero. This causes an error in \code{sample.int}. This function is
#' intended for internal use only.
#' @rdname resample
#' @param spreadProbHas0 logical. Does \code{spreadProb} have any zeros on it.
#' @inheritParams base::sample
resampleZeroProof <- function(spreadProbHas0, x, n, prob) {
  if (spreadProbHas0) {
    sm <- sum(prob, na.rm = TRUE)
    if (sum(prob > 0) <= n) {
      integer()
    } else {
      resample(x, n, prob = prob / sm)
    }
  } else resample(x, n, prob = prob / sum(prob, na.rm = TRUE))
}
