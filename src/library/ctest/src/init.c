#include "ctest.h"
#include "R_ext/Rdynload.h"

R_CMethodDef cmethods[] = {
  {"chisqsim", (DL_FUNC) &chisqsim, 11},  
  {"d2x2xk", (DL_FUNC) &d2x2xk, 5},
  {"fexact",   (DL_FUNC) &fexact, 10},
  {"kendall_tau", (DL_FUNC) &kendall_tau, 4},
  {"pansari",  (DL_FUNC)&pansari, 4},
  {"pkendall", (DL_FUNC)  &pkendall, 3},
  {"pkstwo", (DL_FUNC) &pkstwo, 3},
  {"prho", (DL_FUNC) &prho, 5},
  {"psmirnov2x", (DL_FUNC) &psmirnov2x, 3},
  {"qansari",  (DL_FUNC) &qansari, 4},
  {"swilk", (DL_FUNC) &swilk, 9},
  {NULL, NULL, 0}
};

void R_init_ctest(DllInfo *dll)
{
  R_registerRoutines(dll, cmethods, NULL, NULL);
}
