#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <math.h>
#include <stdlib.h>

#include "macros.h"

// global variable in order to pass objective value array to compare function
double *cc_cd;

// Comparisson function for numeric arrays
// Mimicks the functionality of order in R.
static int compare(const void *a, const void *b) {
  int aa = *((int *)a), bb = *((int *)b);
  if (cc_cd[aa] < cc_cd[bb]) return(-1);
  if (cc_cd[aa] == cc_cd[bb]) return(0);
  return(1);
}

SEXP computeCrowdingDistanceC(SEXP r_points) {
  // unpack R data
  EXTRACT_NUMERIC_MATRIX(r_points, c_points, dim, n_points);

  // allocate memory for return value
  SEXP r_cd = ALLOC_REAL_VECTOR(n_points);
  double *c_cd = REAL(r_cd);

  // make sure that stuff is initialized properly
  for (int i = 0; i < n_points; ++i) {
    c_cd[i] = 0;
  }

  // actual computation
  for (int i = 0; i < dim; ++i) {
    // allocate memory for vector of i-th objective valies
    double *objvec = malloc(sizeof(double) * n_points);

    // extract i-th row (we need to compute indizes here by hand)
    for (int j = 0; j < n_points; ++j) {
      int idx = (j * dim) + i; // i-the row
      objvec[j] = c_points[idx];
    }

    // we need to sort the indizes and not the values itselt. Thus
    // we define an integer vector with initial permuation 1, ..., n_points
    int *ord = malloc(sizeof(int) * n_points);
    for (int k = 0; k < n_points; ++k) {
      ord[k] = k;
    }

    // sort by order, i.e., sort objvec and apply the same trafo to ord vector
    cc_cd = objvec;
    qsort(ord, n_points, sizeof(int), compare);

    // actually do the crowding sort stuff
    c_cd[ord[0]] = INFINITY;
    c_cd[ord[n_points - 1]] = INFINITY;

    if (n_points > 2) {
      for (int j = 1; j < (n_points - 1); ++j) {
        c_cd[ord[j]] = c_cd[ord[j]] + objvec[ord[j + 1]] - objvec[ord[j - 1]];
      }
    }
  }

  UNPROTECT(1);
  return(r_cd);
}
