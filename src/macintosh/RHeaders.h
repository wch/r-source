#ifdef __MRC__  /* Apple' C/C++ Compiler */
 #include <Carbon.h>
#else /* CodeWarrior */
#include <MacHeadersCarbon.h>

#ifndef macintosh
#define macintosh 1
#endif

#define Macintosh 1

#define CW_F2C_MAC 1

#include "config.h"

#endif /* __MRC__ */
