#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP computeCrowdingDistanceC(SEXP);
extern SEXP computeDominatedHypervolumeC(SEXP, SEXP);
extern SEXP computeDominatedHypervolumeContributionC(SEXP, SEXP);
extern SEXP computeEpsilonIndicatorC(SEXP, SEXP);
extern SEXP computeRIndicatorC(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dominatedC(SEXP);
extern SEXP doNondominatedSortingC(SEXP);
extern SEXP polynomialMutationC(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP simulatedBinaryCrossoverC(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"computeCrowdingDistanceC",                 (DL_FUNC) &computeCrowdingDistanceC,                 1},
    {"computeDominatedHypervolumeC",             (DL_FUNC) &computeDominatedHypervolumeC,             2},
    {"computeDominatedHypervolumeContributionC", (DL_FUNC) &computeDominatedHypervolumeContributionC, 2},
    {"computeEpsilonIndicatorC",                 (DL_FUNC) &computeEpsilonIndicatorC,                 2},
    {"computeRIndicatorC",                       (DL_FUNC) &computeRIndicatorC,                       5},
    {"dominatedC",                                (DL_FUNC) &dominatedC,                                1},
    {"doNondominatedSortingC",                    (DL_FUNC) &doNondominatedSortingC,                    1},
    {"polynomialMutationC",                      (DL_FUNC) &polynomialMutationC,                      5},
    {"simulatedBinaryCrossoverC",                (DL_FUNC) &simulatedBinaryCrossoverC,                5},
    {NULL, NULL, 0}
};

void R_init_ecr2(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
