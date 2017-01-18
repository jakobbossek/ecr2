#ifndef SEXP_MACROS
#define SEXP_MACROS

// basic math helpers
#define MIN(A, B) ((A < B) ? (A) : (B))
#define MAX(A, B) ((A > B) ? (A) : (B))

// R specific macros
#define EXTRACT_NUMERIC_MATRIX(S_EXP, C_DATA, N_ROW, N_COL) \
    double *C_DATA = REAL(S_EXP); \
    const R_len_t N_ROW = nrows(S_EXP); \
    const R_len_t N_COL = ncols(S_EXP);

#define EXTRACT_NUMERIC_VECTOR(S_EXP, C_DATA, LENGTH) \
    double *C_DATA = REAL(S_EXP); \
    const R_len_t LENGTH = length(S_EXP);

#define EXTRACT_INTEGER(S_EXP, I) \
    int I = INTEGER(S_EXP)[0];

#define EXTRACT_REAL(S_EXP, D) \
    double D = REAL(S_EXP)[0];

#define ALLOC_VECTOR(type, size) (PROTECT(allocVector(type, size)))
#define ALLOC_LOGICAL_VECTOR(size) (ALLOC_VECTOR(LGLSXP, size));
#define ALLOC_REAL_VECTOR(size) (ALLOC_VECTOR(REALSXP, size))
#define ALLOC_INTEGER_VECTOR(size) (ALLOC_VECTOR(INTSXP, size))

#define ALLOC_LIST(size) (ALLOC_VECTOR(VECSXP, size))

#endif
