#include <R.h>
#include <Rinternals.h>

#include "macros.h"
#include "helpers.h"

/* Calculate betaq parameter based on beta and eta.
 *
 * @param beta [numeric(1)]
 *   Beta parameter.
 * @param eta [numeric(1)]
 *   Eta parameter.
 * @return betaq
 */
static R_INLINE double calculateBetaQ(const double beta, const double eta) {
  const double rand = unif_rand();
  const double alpha = 2.0 - pow(beta, -(eta + 1.0));
  if (rand <= (1.0 / alpha)) {
    return(pow(rand * alpha, 1.0 / (eta + 1.0)));
  }
  return(pow(1.0 / (2.0 - rand * alpha), 1.0 / (eta + 1.0)));
}

/* Simulated Binary Crossover (SBX).
 *
 * Given two individuals a simulated binary crossover is performed.
 * The operator returns two new individuals.
 *
 * @param parents [matrix]
 *   Parent genomes as a matrix.
 * @param lower [numeric]
 *   Vector of lower bounds.
 * @param upper [numeric]
 *   Vector of upper bounds.
 * @param p [numeric(1)]
 *   Probability in [0,1].
 * @param eta [numeric(1)]
 *   Paramter eta.
 * @return [matrix]
 */
SEXP simulatedBinaryCrossoverC(SEXP parents, SEXP lower, SEXP upper, SEXP p, SEXP eta) {
  SEXP res;
  double betaq;

  // unpack imcoming R data
  EXTRACT_NUMERIC_MATRIX(parents, c_parents, d, n);
  const double *parent1 = c_parents;
  const double *parent2 = c_parents + d; /* in C we need to compute rows by hand */
  EXTRACT_NUMERIC_VECTOR(lower, c_lower, c_lower_length);
  EXTRACT_NUMERIC_VECTOR(upper, c_upper, c_upper_length);
  EXTRACT_REAL(p, c_p);
  EXTRACT_REAL(eta, c_eta);

  /* Allocate result matrix: */
  PROTECT(res = allocMatrix(REALSXP, d, 2));
  double *child1 = REAL(res);
  double *child2 = REAL(res) + d;

  GetRNGstate();
  for (int i = 0; i < d; ++i) {
  /* Crossover for dim i is performed with probability p, if
   * the two parents differ by at least a very small number.
   */
    if (unif_rand() <= c_p && fabs(parent1[i] - parent2[i]) > 1.0e-14) {
      const double y1 = MIN(parent1[i], parent2[i]);
      const double y2 = MAX(parent1[i], parent2[i]);
      const double yl = c_lower[i];
      const double yu = c_upper[i];

      /* Calculate offsprint: */
      betaq = calculateBetaQ(1.0 + (2.0 * (y1 - yl) / (y2 - y1)), c_eta);
      const double c1 = forceToBounds(0.5 * ((y1 + y2) - betaq * (y2 - y1)), yl, yu);

      betaq = calculateBetaQ(1.0 + (2.0 * (yu - y2) / (y2 - y1)), c_eta);
      const double c2 = forceToBounds(0.5 * ((y1 + y2) + betaq * (y2 - y1)), yl, yu);

      if (unif_rand() > 0.5) {
        child1[i] = c2;
        child2[i] = c1;
      } else {
        child1[i] = c1;
        child2[i] = c2;
      }
    } else {
      child1[i] = parent1[i];
      child2[i] = parent2[i];
    }
  }
  PutRNGstate();
  UNPROTECT(1); /* res */
  return(res);
}
