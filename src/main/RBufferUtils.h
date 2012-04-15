/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2007 The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_BUFFER_UTILS
#define R_BUFFER_UTILS

/* used in bind.c character.c deparse.c, printutils.c, saveload.c
   scan.c seq.c sprintf.c sysutils.c */

typedef struct {
 char *data;
 size_t bufsize;
 size_t defaultSize;
} R_StringBuffer;

/* code in ./memory.c : */
/* Note that R_StringBuffer *buf needs to be initialized before call */
void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf);
void R_FreeStringBuffer(R_StringBuffer *buf);
void R_FreeStringBufferL(R_StringBuffer *buf);

#endif
