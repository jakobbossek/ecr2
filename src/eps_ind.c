/*===========================================================================*
 * eps_ind.c: implements the unary epsilon indicator as proposed in
 *            Zitzler, E., Thiele, L., Laumanns, M., Fonseca, C., and
 *            Grunert da Fonseca, V (2003): Performance Assessment of
 *            Multiobjective Optimizers: An Analysis and Review. IEEE
 *            Transactions on Evolutionary Computation, 7(2), 117-132.
 *
 * Compile:
 *   gcc -lm -o eps_ind eps_ind.c
 *
 * IMPORTANT:
 *   To make the epsilon indicator work for mixed optimization problems
 *   where some objectives are to be maximized while others are to be
 *   minimized, in the case of minimization the value -epsilon (for the
 *   additive case) resp. 1/epsilon (for the multiplicative version) is
 *   considered and returned. Example: suppose f1 is to be minimized and
 *   f2 to be maximized, and the multiplicative epsilon value computed by
 *   this program is 3.0; this means that the considered nondominated front
 *   needs to be multiplied by 1/3 for all f1 values and by 3 for all
 *   f2 values. Thus, independently of which type of problem one considers
 *   (minimization, maximization, mixed minimization/maximization), a lower
 *   indicator value corresponds to a better approximation set.
 *
 * Author:
 *   Eckart Zitzler, February 3, 2005 / last update August 9, 2005
 *
 * Author of R interface:
 *   Jakob Bossek <j.bossek@gmail.com>
 *
 */

#include <float.h>
#include <stdlib.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

#include "macros.h"

/*
 * Source code for the computation of the unary epsilon-indicator by Zitzler.
 *
 * @param *a [pointer to matrix]
 *   Reference point set (each column is a point).
 * @param n_a
 *   Number of reference points.
 * @param *b [pointer to matrix]
 *   Approximation set (each column is a point).
 * @param n_b
 *   Number of points.
 * @param dim
 *   Number of objectives.
 * @param method [integer]
 *   Value in {0 = "additive", 1 = "multiplicative"}
 * @return double
 */
//JB: FIXME: rename the poorly named vars
static double emoaIndEps(
  double *a, const size_t n_a,
  double *b, const size_t n_b,
  const size_t dim, unsigned int method) {

  int  i, j, k;
  double  eps, eps_j, eps_k, eps_temp;

  if (method == 0)
    eps = DBL_MIN;
  else
    eps = 0;

  for (i = 0; i < n_a; i++) {
    for (j = 0; j < n_b; j++) {
      for (k = 0; k < dim; k++) {
        switch (method) {
          case 0:
          eps_temp = b[j * dim + k] - a[i * dim + k];
          break;
          case 1:
          eps_temp = b[j * dim + k] / a[i * dim + k];
          break;
        }
        if (k == 0)
          eps_k = eps_temp;
        else if (eps_k < eps_temp)
          eps_k = eps_temp;
      }
      if (j == 0)
        eps_j = eps_k;
      else if (eps_j > eps_k)
        eps_j = eps_k;
    }
    if (i == 0)
      eps = eps_j;
    else if (eps < eps_j)
      eps = eps_j;
  }
  return eps;
}

/*
 * R to C interface for the calculation of the unary epsilon-indicator.
 *
 * @param points [matrix]
 *   R matrix (each column contains one points of the approximation set).
 * @param ref_points [matrix]
 *   R matrix (each column contains one point of the reference set).
 * @return [numeric(1)] Unary epsilon-indicator value.
 */
SEXP emoaIndEpsC(SEXP points, SEXP ref_points) {
  // get that stuff from R
  EXTRACT_NUMERIC_MATRIX(points, c_points, n_objectives, n_points);
  EXTRACT_NUMERIC_MATRIX(ref_points, c_ref_points, n_ref_objectives, n_ref_points);

  double eps_ind = emoaIndEps(
    c_ref_points, n_ref_points,
    c_points, n_points,
    n_objectives,
    0 // here we pass 0 = "additive" in each case since we always minimize all objectives
  );
  return ScalarReal(eps_ind);
}
