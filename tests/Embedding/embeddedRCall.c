
#include "R.h"
#include "Rinternals.h"
#include "Rdefines.h"

#include "embeddedRCall.h"
#include "Defn.h"

int
eval_R_command(const char *funcName, int argc, char *argv[])
{
 SEXP e;
 SEXP fun;
 SEXP arg;

 int i;
 int errorOccurred;
 init_R(argc, argv);

    fun = Rf_findFun(Rf_install((char *)funcName),  R_GlobalEnv);
    PROTECT(fun);
    PROTECT(arg = NEW_INTEGER(10));
    for(i = 0; i < GET_LENGTH(arg); i++)
      INTEGER_DATA(arg)[i]  = i + 1;

    e = allocVector(LANGSXP, 2);
    PROTECT(e);
    SETCAR(e, fun);
    SETCAR(CDR(e), arg);

      /* Evaluate the call to the R function.
         Ignore the return value.
       */
    Test_tryEval(e, &errorOccurred);

    UNPROTECT(3);   
  return(0);
}

extern int Rf_initEmbeddedR(int argc, char *argv[]);

void
init_R(int argc, char **argv)
{
  int defaultArgc = 1;
  char *defaultArgv[] = {"Rtest"};

  if(argc == 0 || argv == NULL) {
      argc = defaultArgc;
      argv = defaultArgv;
  }
  Rf_initEmbeddedR(argc, argv);
}



typedef struct {
    SEXP expression;
    SEXP val;
} R_ProtectedEvalData;

void
protectedEval(void *d)
{
    R_ProtectedEvalData *data = (R_ProtectedEvalData *)d;

    data->val = eval(data->expression, R_GlobalEnv); 
    PROTECT(data->val);
}

SEXP
Test_tryEval(SEXP e, int *ErrorOccurred)
{
 Rboolean ok;
 R_ProtectedEvalData data;

 data.expression = e;
 data.val = NULL;

 ok = R_ToplevelExec(protectedEval, &data);
 if(ErrorOccurred) {
     *ErrorOccurred = (ok == FALSE);
 }
 if(ok == FALSE)
     data.val = NULL;
 else
     UNPROTECT(1);

 return(data.val);
}



