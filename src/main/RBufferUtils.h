#ifndef R_BUFFER_UTILS
#define R_BUFFER_UTILS

/* used in bind.c character.c deparse.c, printutils.c, saveload.c
   scan.c seq.c sysutils.c */

typedef struct {
 char *data; 
 size_t bufsize;
 size_t defaultSize;
} R_StringBuffer;

/* code in deparse.c */
/* Note that R_StringBuffer *buf needs to be initialized before call */
void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf);
void R_FreeStringBuffer(R_StringBuffer *buf);
void R_FreeStringBufferL(R_StringBuffer *buf);

#endif
