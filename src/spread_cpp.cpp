#include <Rcpp.h>
#include <limits>
#include <vector>
using namespace Rcpp;

// spreadCpp(): a stochastic outward spread, done entirely in C++.
//
// This is NOT a bit-for-bit reimplementation of spread(). It reproduces the
// RULES -- each burning cell offers its unburned neighbours a chance to catch,
// with the probability of the cell being spread TO, generation by generation, so
// fires grow outwards -- but it makes its own random draws and its own
// tie-breaks, so results differ from spread() for the same seed.
//
// The rules, stated so they can be checked:
//
//  1. Generations. Every cell that caught in generation g tries to spread in
//     generation g+1, and only then. That is what makes growth outward and keeps
//     a fire's shape tied to how many steps it has had.
//  2. One draw per (burning cell, unburned neighbour) pair. A cell with k
//     burning neighbours therefore gets k chances in a generation, so
//     P(catch) = 1 - (1 - p)^k, exactly as in spread(). The draw is compared
//     against the probability of the TARGET cell.
//  3. No overlap. A cell belongs to one fire. Within a generation the burning
//     cells are visited in a random order and the first success claims the cell,
//     so when two fires reach the same cell neither is systematically favoured.
//     A failed draw does not claim or block anything, so the remaining
//     neighbours still get their chances -- which is why rule 2 holds exactly.
//  4. maxSize. A fire stops claiming once it reaches its own maxSize, so a fire
//     never exceeds it. (spread() adds the whole generation and then randomly
//     removes the excess; this simply stops, which lands on maxSize exactly.)
//  5. A fire with no new cells in a generation is finished. When no fire claims
//     anything, the spread is over.
//
// NA spreadProb is treated as 0, i.e. unburnable, as spread() does.

static inline void checkGridCpp(int numCol, int numCell, int directions) {
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

//' Stochastic outward spread, in C++
//'
//' Internal engine for [spreadCpp()]. See the comments in `src/spread_cpp.cpp`
//' for the rules it implements and how they differ from [spread()].
//'
//' @param numCol Integer; number of raster columns.
//' @param numCell Integer; total number of raster cells.
//' @param directions Integer; 4 or 8.
//' @param loci Integer vector of starting cells, one per fire.
//' @param spreadProb Numeric, length 1 or `numCell`; probability of being spread to.
//' @param maxSize Numeric, length 1 or `length(loci)`; maximum cells per fire.
//' @param iterations Integer; maximum number of generations.
//'
//' @return A list of three integer vectors: `id`, `initialLocus`, `indices`.
//' @keywords internal
//' @rdname spreadCppEngine
// [[Rcpp::export]]
List spreadCppEngine(int numCol, int numCell, int directions,
                     IntegerVector loci, NumericVector spreadProb,
                     NumericVector maxSize, double iterations) {
  checkGridCpp(numCol, numCell, directions);

  const int nFire = (int) loci.size();
  if (nFire == 0) {
    IntegerVector e(0);
    return List::create(Named("id") = e, Named("initialLocus") = e, Named("indices") = e);
  }

  const R_xlen_t np = spreadProb.size();
  if (np != 1 && np != numCell)
    stop("`spreadProb` must be length 1 or one element per cell.");
  const R_xlen_t nm = maxSize.size();
  if (nm != 1 && nm != nFire)
    stop("`maxSize` must be length 1 or one element per starting cell.");

  const double *pp = REAL(spreadProb);
  const bool oneProb = (np == 1);
  const double p0 = oneProb ? pp[0] : 0.0;

  // per-fire cap, as a cell count; a non-finite or missing maxSize means no cap
  std::vector<double> cap((size_t) nFire);
  for (int f = 0; f < nFire; ++f) {
    const double v = (nm == 1) ? maxSize[0] : maxSize[f];
    cap[(size_t) f] = (ISNAN(v) || !R_FINITE(v)) ? R_PosInf : v;
  }

  // state[c] is 0 when cell c+1 is unburned, otherwise the 1-based fire id
  std::vector<int> state((size_t) numCell, 0);
  std::vector<double> size((size_t) nFire, 0.0);

  std::vector<int> outId, outCell;
  outId.reserve((size_t) nFire * 16);
  outCell.reserve((size_t) nFire * 16);

  // current generation: the cells that caught last time, and whose fire they are
  std::vector<int> actCell, actFire, nextCell, nextFire;
  actCell.reserve((size_t) nFire);
  actFire.reserve((size_t) nFire);

  // the starting cells themselves burn, subject to maxSize and to not being
  // claimed twice when two fires are given the same locus
  for (int f = 0; f < nFire; ++f) {
    const int c = loci[f];
    if (c < 1 || c > numCell) stop("`loci` has a cell outside the landscape.");
    if (state[(size_t) c - 1] != 0) continue;   // another fire already starts here
    if (cap[(size_t) f] < 1.0) continue;        // maxSize 0: this fire never burns
    state[(size_t) c - 1] = f + 1;
    size[(size_t) f] = 1.0;
    outId.push_back(f + 1);
    outCell.push_back(c);
    actCell.push_back(c);
    actFire.push_back(f);
  }

  // neighbour offsets, as (column shift, row shift)
  const int nDir = directions;
  const int dCol[8] = {-1,  0,  1, -1,  1, -1,  0,  1};
  const int dRow[8] = {-1, -1, -1,  0,  0,  1,  1,  1};
  const int dCol4[4] = { 0, -1,  1,  0};
  const int dRow4[4] = {-1,  0,  0,  1};
  const int *dc = (nDir == 8) ? dCol : dCol4;
  const int *dr = (nDir == 8) ? dRow : dRow4;

  std::vector<int> order;
  double it = 0.0;
  while (!actCell.empty() && it < iterations) {
    ++it;
    nextCell.clear();
    nextFire.clear();

    // random visiting order, so that when two fires reach the same cell in the
    // same generation neither is systematically favoured (rule 3)
    const size_t na = actCell.size();
    order.resize(na);
    for (size_t i = 0; i < na; ++i) order[i] = (int) i;
    for (size_t i = na; i > 1; --i) {
      size_t j = (size_t) (unif_rand() * (double) i);   // 0 .. i-1
      if (j >= i) j = i - 1;                            // guard the u == 1 corner
      const int tmp = order[i - 1];
      order[i - 1] = order[j];
      order[j] = tmp;
    }

    for (size_t k = 0; k < na; ++k) {
      const size_t idx = (size_t) order[k];
      const int c = actCell[idx];
      const int f = actFire[idx];
      if (size[(size_t) f] >= cap[(size_t) f]) continue;   // this fire is full

      const int colMod = c % numCol;
      for (int d = 0; d < nDir; ++d) {
        if (dc[d] == -1 && colMod == 1) continue;          // would wrap left
        if (dc[d] ==  1 && colMod == 0) continue;          // would wrap right
        const int t = c + dr[d] * numCol + dc[d];
        if (t < 1 || t > numCell) continue;                // off top/bottom
        if (state[(size_t) t - 1] != 0) continue;          // already burned

        double p = oneProb ? p0 : pp[t - 1];
        if (ISNAN(p)) p = 0.0;
        if (unif_rand() > p) continue;                     // did not catch

        state[(size_t) t - 1] = f + 1;
        size[(size_t) f] += 1.0;
        outId.push_back(f + 1);
        outCell.push_back(t);
        nextCell.push_back(t);
        nextFire.push_back(f);
        if (size[(size_t) f] >= cap[(size_t) f]) break;    // full: stop this cell
      }
    }
    actCell.swap(nextCell);
    actFire.swap(nextFire);
  }

  const R_xlen_t n = (R_xlen_t) outId.size();
  IntegerVector id(n), initialLocus(n), indices(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    id[i] = outId[(size_t) i];
    initialLocus[i] = loci[outId[(size_t) i] - 1];
    indices[i] = outCell[(size_t) i];
  }
  return List::create(Named("id") = id,
                      Named("initialLocus") = initialLocus,
                      Named("indices") = indices);
}
