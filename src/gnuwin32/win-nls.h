#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) libintl_gettext (String)
#undef gettext /* needed for graphapp */
#define gettext_noop(String) String
#define N_(String) gettext_noop (String)
#else /* not NLS */
#define _(String) (String)
#define N_(String) String
#endif
