#include <R.h>
#include <Rinternals.h>

#include "macros.h"
#include "hv.h"

/* Interface to hypervolume algorithm by Fonseca et al.
 *
 * @param r_points [matrix]
 *   Matrix of points (column-wise).
 * @param ref.point [numeric}]
 *   Reference point.
 * @return [numeric(1)] Dominated hypervolume.
**/
SEXP computeHVC(SEXP r_points, SEXP r_ref_point) {
  // unpack R data
  EXTRACT_NUMERIC_MATRIX(r_points, c_points, dim, n_points);
  EXTRACT_NUMERIC_VECTOR(r_ref_point, c_ref_point, len_ref);

  // allocate memory for Hypervolume value
  SEXP r_hv = ALLOC_REAL_VECTOR(1);
  double *c_hv = REAL(r_hv);

  // call Fonseca et al. algorithm
  c_hv[0] = fpli_hv(c_points, dim, n_points, c_ref_point);

  // free memory and return
  UNPROTECT(1);
  return r_hv;
}

/* Interface to the exclusive hypervolume contribution of a set of points.
 *
 * @param r_point [matrix]
 *   Matrix of points (each column contains one point).
 * @param r_ref_point [numeric]
 *   Reference point, e.g., the nadir point.
 * @return [numeric] Vector of hypervolume contributions.
**/
SEXP computeHVContributionC(SEXP r_points, SEXP r_ref_point) {
  // unpack imcoming R data
  // JB: compatibility of dimensions is checked in R
  EXTRACT_NUMERIC_MATRIX(r_points, c_points, dim, n_points);
  EXTRACT_NUMERIC_VECTOR(r_ref_point, c_ref_point, n_ref_point);

  SEXP r_hvc = ALLOC_REAL_VECTOR(n_points);
  double *c_hvc = REAL(r_hvc);

  // Here we compute the hypervolume contribution with a naive approach.
  // Let Y be the set of points. We iteratively compute the dominated hyper-
  // volume for Y\{y} for all y in Y. After each iteration swap the first and the
  // current column. This way we can always compute the hypervolumne of the points
  // at locations 1, ..., n-1 and omit the first one at position 0.

  // compute total hypervolume contribution
  const double total_hv = fpli_hv(c_points, dim, n_points, c_ref_point);
  // and now compute individual hv contribution
  for (int i = 0; i < n_points; ++i) {
    const double current_hv = fpli_hv(c_points + dim, dim, n_points - 1, c_ref_point);
    c_hvc[i] = total_hv - current_hv;
    // do the swaping (see above)
    if (i != (n_points - 1)) {
      for (int j = 0; j < dim; ++j) {
        double tmp = c_points[dim * (i + 1) + j];
        c_points[dim * (i + 1) + j] = c_points[j];
        c_points[j] = tmp;
      }
    }
  }

  UNPROTECT(1);
  return r_hvc;
}
