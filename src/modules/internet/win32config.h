#define HAVE_FCNTL_H

#include <io.h>

#define SOCKLEN_T int

#ifdef INCLUDE_WINSOCK
#include <winsock.h>

#define EWOULDBLOCK             WSAEWOULDBLOCK
#define EINPROGRESS             WSAEINPROGRESS
#define EALREADY                WSAEALREADY
#endif /* INCLUDE_WINSOCK */
