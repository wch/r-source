/*
  This provides a collection of simple routines which we can call
  with R objects of different types in order to test the automatic
  conversion to the registered type.
*/
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static void
testStrings(char **vals, long *len)
{
  int i;
  for(i = 0; i < *len; i++) {
    fprintf(stderr, "%d) %s\n", i+1, vals[i]);
  }
}

/* 
  This alters its inputs by writing values back
  into the 
 */
static void
setNumeric(double *vals, long *len)
{
  int i;
  for(i = 0; i < *len; i++) {
      vals[i] = 17.0*vals[i];
  }
}

static void
testSingles(float *vals, long *len)
{
  int i;
  for(i = 0; i < *len; i++) {
    fprintf(stderr, "%d) %f\n", i+1, vals[i]);
  }
}

static void
testList(void *vals, long *len)
{
  int i;
  for(i = 0; i < *len; i++) {
     fprintf(stderr, "%d) %d\n", i+1, TYPEOF(((SEXP *) vals)[i]));
  }
}

static R_NativePrimitiveArgType stringArgs_t[2] = {STRSXP, INTSXP};
static R_NativePrimitiveArgType singleArgs_t[2] = {SINGLESXP, INTSXP};
static R_NativePrimitiveArgType listArgs_t[2] = {VECSXP, INTSXP};

static R_NativePrimitiveArgType setNumeric_t[2] = {REALSXP, INTSXP};

static R_NativeArgStyle stringStyles_t[2] = {R_ARG_IN, R_ARG_IN};
static R_NativeArgStyle setNumericStyles_t[2] = {R_ARG_OUT, R_ARG_IN};

void
R_init_Tests(DllInfo *dll)
{
 const R_CMethodDef cmethods[]  = {
	 {"testStrings", (DL_FUNC) &testStrings, 2, stringArgs_t, stringStyles_t},
	 {"testSingles", (DL_FUNC) &testSingles, 2, singleArgs_t},
	 {"testList", (DL_FUNC) &testList, 2, listArgs_t},
	 {"setNumeric", (DL_FUNC) &setNumeric, 2, setNumeric_t, setNumericStyles_t},
	 {NULL, NULL, 0}
 };

 R_registerRoutines(dll, cmethods, NULL, NULL, NULL);
}
