#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

SEXP nls_iter(SEXP m, SEXP control, SEXP doTraceArg);
SEXP numeric_deriv(SEXP expr, SEXP theta, SEXP rho);


