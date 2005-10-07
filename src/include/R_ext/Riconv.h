/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005     the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef R_ICONV_H
#define R_ICONV_H

#ifdef  __cplusplus
extern "C" {
#endif

/* from sysutils.c */
#undef Riconv_open
#undef Riconv
#undef Riconv_close
void * Riconv_open (char* tocode, char* fromcode);
size_t Riconv (void * cd, char **inbuf, size_t *inbytesleft,
	       char  **outbuf, size_t *outbytesleft);
int Riconv_close (void * cd);

#ifdef  __cplusplus
}
#endif

#endif /* R_ICONV_H */
