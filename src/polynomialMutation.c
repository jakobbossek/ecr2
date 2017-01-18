#include <R.h>
#include <Rinternals.h>

#include "macros.h"
#include "helpers.h"

/* Polynomial mutation operator.
 *
 * Performs polynomial mutation of an individual with probability p for each gene.
 *
 * @param x [numeric]
 *   Individual.
 * @param lower [numeric]
 *   Lower box constraints.
 * @param upper [numeric]
 *   Upper box constraints.
 * @param p [numeric(1)]
 *   Probability for mutation of each gene.
 * @param eta [numeric(1)]
 *   Parameter eta.
 * @return [numeric] Mutated individual.
 */
SEXP polynomialMutationC(SEXP x, SEXP lower, SEXP upper, SEXP p, SEXP eta) {
  SEXP res;

  // unpack imcoming R data
  EXTRACT_NUMERIC_VECTOR(x, c_x, d);
  EXTRACT_NUMERIC_VECTOR(lower, c_lower, c_lower_length);
  EXTRACT_NUMERIC_VECTOR(upper, c_upper, c_upper_length);
  EXTRACT_REAL(p, c_p);
  EXTRACT_REAL(eta, c_eta);

  const double mpow = 1.0 / (c_eta + 1.0);

  PROTECT(res = allocVector(REALSXP, d));
  double *c_res = REAL(res);

  GetRNGstate();
  double rnd, deltaq;
  for (int i = 0; i < d; ++i) {
    // do all the complicated stuff here
    if (unif_rand() < c_p) {
      const double delta = c_upper[i] - c_lower[i];
      rnd = unif_rand();
      if (rnd <= 0.5) {
        const double xy = 1.0 - (c_x[i] - c_lower[i]) / delta;
        deltaq = pow(2.0 * rnd + (1.0 - 2.0 * rnd) * pow(xy, c_eta + 1.0), mpow) - 1.0;
      } else {
        const double xy = 1.0 - (c_upper[i] - c_x[i]) / delta;
        deltaq = 1.0 - pow(2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * pow(xy, c_eta + 1.0), mpow);
      }
      c_res[i] = forceToBounds(c_x[i] + deltaq * delta, c_lower[i], c_upper[i]);
    } else {
      c_res[i] = c_x[i];
    }
  }

  PutRNGstate();
  UNPROTECT(1); /* res */
  return(res);
}
