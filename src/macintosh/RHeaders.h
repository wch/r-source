#if defined(__MRC__)
#include <Carbon.h>
#else
#include <MacHeadersCarbon.h>
#endif

#ifndef macintosh
#define macintosh 1
#endif

#define Macintosh 1

#ifndef __MRC__
#define CW_F2C_MAC 1
#endif

#ifndef __MRC__
#include "config.h"
#endif
