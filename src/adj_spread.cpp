#include <Rcpp.h>
#include <limits>
using namespace Rcpp;

// Argument checks shared by both helpers below.
//
// These are cheap, constant-time checks on the grid description, not on the
// cells themselves. That is deliberate: both functions run inside the
// spread()/spread2() iteration loop, so an O(n) sweep over `cells` would cost
// real time on the hot path. Out-of-range cell numbers are harmless here --
// the `t < 1 || t > numCell` filter in the emit loops drops whatever they
// produce -- whereas the checks below guard against undefined behaviour:
//
//   * numCol < 1        -> `c % numCol` is integer division by zero (SIGFPE)
//   * numCell < 1       -> no cell can ever pass the bounds filter
//   * numCol > numCell  -> not a grid
//   * numCell too large -> `c + rowOffset * numCol + colOffset` can overflow
//   * directions not 4/8 -> silently emitted nothing, so spread() returned a
//                           degenerate result rather than erroring, despite
//                           ?spread documenting "Can only be 4 or 8"
//
// NA_INTEGER arrives as INT_MIN and is caught by the `< 1` comparisons.
static inline void checkGrid(int numCol, int numCell, int directions) {
  if (directions != 4 && directions != 8)
    stop("`directions` must be 4 or 8, not %d.", directions);
  if (numCol < 1)
    stop("`numCol` must be a positive integer, not %d.", numCol);
  if (numCell < 1)
    stop("`numCell` must be a positive integer, not %d.", numCell);
  if (numCol > numCell)
    stop("`numCol` (%d) cannot exceed `numCell` (%d).", numCol, numCell);
  if (numCell > std::numeric_limits<int>::max() - numCol - 1)
    stop("`numCell` (%d) is too large; neighbour arithmetic would overflow.", numCell);
}

//' Adjacent-pairs with id, used by spread2 hot loop
//'
//' Computes 4- or 8-direction neighbour pairs (from, to, id) for every cell
//' and filters out neighbours that fall off the raster top/bottom or wrap
//' across left/right edges. Rows are emitted in the same direction-major
//' order that adj() produces (topl, top, topr, lef, rig, botl, bot, botr for
//' directions == 8) so that downstream seeded operations (e.g. R-side
//' sample.int shuffle) consume RNG state and produce identical output to
//' the original spread2 implementation. This replaces only the adj()
//' call inside the spread2 main loop; the row-randomization step still
//' happens in R afterwards.
//'
//' @param cells integer vector of source cell indices (1-based)
//' @param id integer vector of the same length as cells; the event id for each cell
//' @param numCol integer; number of raster columns
//' @param numCell integer; total number of raster cells
//' @param directions integer; either 4 or 8
//'
//' @return A list with three integer vectors `from`, `to`, `id`.
//' @keywords internal
//' @rdname adjPairsWithId
// [[Rcpp::export]]
List adjPairsWithId(IntegerVector cells, IntegerVector id,
                    int numCol, int numCell, int directions) {
  checkGrid(numCol, numCell, directions);

  const R_xlen_t n = cells.size();
  // `id` is indexed in lockstep with `cells`; a shorter `id` read past its end
  // and returned uninitialised memory as event ids, silently.
  if (id.size() != n)
    stop("`id` must be the same length as `cells` (%d vs %d).",
         (int) id.size(), (int) n);

  if (n == 0) {
    IntegerVector empty(0);
    return List::create(Named("from") = empty,
                        Named("to")   = empty,
                        Named("id")   = empty);
  }

  std::vector<int> outFrom, outTo, outId;
  outFrom.reserve(n * directions);
  outTo.reserve(n * directions);
  outId.reserve(n * directions);

  auto emitPass = [&](int colOffset, int rowOffset) {
    for (R_xlen_t i = 0; i < n; ++i) {
      const int c = cells[i];
      const int colMod = c % numCol;
      if (colOffset == -1 && colMod == 1) continue;
      if (colOffset ==  1 && colMod == 0) continue;
      const int t = c + rowOffset * numCol + colOffset;
      if (t < 1 || t > numCell) continue;
      outFrom.push_back(c);
      outTo.push_back(t);
      outId.push_back(id[i]);
    }
  };

  if (directions == 8) {
    emitPass(-1, -1); // topl
    emitPass( 0, -1); // top
    emitPass( 1, -1); // topr
    emitPass(-1,  0); // lef
    emitPass( 1,  0); // rig
    emitPass(-1,  1); // botl
    emitPass( 0,  1); // bot
    emitPass( 1,  1); // botr
  } else {
    emitPass( 0, -1); // top
    emitPass(-1,  0); // lef
    emitPass( 1,  0); // rig
    emitPass( 0,  1); // bot
  }

  IntegerVector vFrom(outFrom.begin(), outFrom.end());
  IntegerVector vTo(outTo.begin(),     outTo.end());
  IntegerVector vId(outId.begin(),     outId.end());

  return List::create(Named("from") = vFrom,
                      Named("to")   = vTo,
                      Named("id")   = vId);
}

//' Adjacent-pairs matrix used by spread() hot loop
//'
//' Computes 4- or 8-direction neighbour pairs (from, to) for every input cell
//' and filters out neighbours that fall off the raster top/bottom or wrap
//' across left/right edges. Returns a 2-column integer matrix with column
//' names "from" and "to". Equivalent to adj(..., pairs = TRUE) but without
//' the data.table allocation/coercion overhead.
//'
//' @inheritParams adjPairsWithId
//'
//' @return A 2-column integer matrix with columns named `from` and `to`.
//' @keywords internal
//' @rdname adjPairsMatrix
// [[Rcpp::export]]
IntegerMatrix adjPairsMatrix(IntegerVector cells, int numCol, int numCell, int directions) {
  checkGrid(numCol, numCell, directions);

  const R_xlen_t n = cells.size();
  if (n == 0) {
    IntegerMatrix empty(0, 2);
    colnames(empty) = CharacterVector::create("from", "to");
    return empty;
  }

  // Emit in the same direction-major order as adj() with match.adjacent=FALSE,
  // include=FALSE, pairs=TRUE: for directions==8, the order is
  // (topl, top, topr, lef, rig, botl, bot, botr); for directions==4 it is
  // (top, lef, rig, bot). This preserves reproducibility for seeded code that
  // relies on `!duplicated(to)` keeping the first occurrence.
  std::vector<int> outFrom, outTo;
  outFrom.reserve(n * directions);
  outTo.reserve(n * directions);

  auto emitPass = [&](int colOffset, int rowOffset) {
    for (R_xlen_t i = 0; i < n; ++i) {
      const int c = cells[i];
      const int colMod = c % numCol;
      if (colOffset == -1 && colMod == 1) continue;     // would wrap left
      if (colOffset ==  1 && colMod == 0) continue;     // would wrap right
      const int t = c + rowOffset * numCol + colOffset;
      if (t < 1 || t > numCell) continue;               // off top/bottom
      outFrom.push_back(c);
      outTo.push_back(t);
    }
  };

  if (directions == 8) {
    emitPass(-1, -1); // topl
    emitPass( 0, -1); // top
    emitPass( 1, -1); // topr
    emitPass(-1,  0); // lef
    emitPass( 1,  0); // rig
    emitPass(-1,  1); // botl
    emitPass( 0,  1); // bot
    emitPass( 1,  1); // botr
  } else {
    emitPass( 0, -1); // top
    emitPass(-1,  0); // lef
    emitPass( 1,  0); // rig
    emitPass( 0,  1); // bot
  }

  const R_xlen_t m = static_cast<R_xlen_t>(outFrom.size());
  IntegerMatrix out(m, 2);
  std::copy(outFrom.begin(), outFrom.end(), out.column(0).begin());
  std::copy(outTo.begin(),   outTo.end(),   out.column(1).begin());
  colnames(out) = CharacterVector::create("from", "to");
  return out;
}
