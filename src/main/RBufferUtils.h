#ifndef R_BUFFER_UTILS
#define R_BUFFER_UTILS

/* We can make these uint's rather than ordinary int's. */
typedef struct {
 char *data; 
 int bufsize;
 int defaultSize;
} R_StringBuffer;

void R_AllocStringBuffer(int blen, R_StringBuffer *buf);
void R_FreeStringBuffer(R_StringBuffer *buf);

#endif
