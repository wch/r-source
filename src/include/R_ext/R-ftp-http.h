/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-5 The R Development Core Team.
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

/* Advertized entry points, for that part of libxml included in
 * the internet module.
 */

#ifndef R_FTP_HTTP_H_
#define R_FTP_HTTP_H_

#ifdef __cplusplus
extern "C" {
#endif

void *R_HTTPOpen(const char *url);
int   R_HTTPRead(void *ctx, char *dest, int len);
void  R_HTTPClose(void *ctx);

void *R_FTPOpen(const char *url);
int   R_FTPRead(void *ctx, char *dest, int len);
void  R_FTPClose(void *ctx);

void *	RxmlNanoHTTPOpen(const char *URL, char **contentType, int cacheOK);
int	RxmlNanoHTTPRead(void *ctx, void *dest, int len);
void	RxmlNanoHTTPClose(void *ctx);
int 	RxmlNanoHTTPReturnCode(void *ctx);
char * 	RxmlNanoHTTPStatusMsg(void *ctx);
int 	RxmlNanoHTTPContentLength(void *ctx);
char *	RxmlNanoHTTPContentType(void *ctx);
void	RxmlNanoHTTPTimeout(int delay);

void *	RxmlNanoFTPOpen(const char *URL);
int	RxmlNanoFTPRead(void *ctx, void *dest, int len);
int	RxmlNanoFTPClose(void *ctx);
void	RxmlNanoFTPTimeout(int delay);
int 	RxmlNanoFTPContentLength(void *ctx);

void    RxmlMessage(int level, const char *format, ...);

/* not currently used */

void RxmlNanoFTPCleanup(void);
void RxmlNanoHTTPCleanup(void);

/* sockets */
void R_SockTimeout(int delay);
int R_SockOpen(int port);
int R_SockListen(int sockp, char *buf, int len);
int R_SockConnect(int port, char *host);
int R_SockClose(int sockp);
int R_SockRead(int sockp, void *buf, int maxlen, int blocking);
int R_SockWrite(int sockp, const void *buf, int len);

#ifdef __cplusplus
}
#endif

#endif /* R_FTP_HTTP_H_ */
