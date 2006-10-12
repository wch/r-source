#ifndef R_BUFFER_UTILS
#define R_BUFFER_UTILS

/* used in character.c deparse.c, printutils.c, saveload.c scan.c */

/* We can make these uint's rather than ordinary int's. */
typedef struct {
 char *data; 
 int bufsize;
 int defaultSize;
} R_StringBuffer;

/* code in deparse.c */
/* Note that R_StringBuffer *buf needs to be initialized before call */
void R_AllocStringBuffer(int blen, R_StringBuffer *buf);
void R_FreeStringBuffer(R_StringBuffer *buf);

#endif
