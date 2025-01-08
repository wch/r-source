#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdint.h>

/* Local mod: assumes _WIN32 is i386 and little-endian generic is 32-bit */
#if defined(_WIN32) || defined(__CYGWIN__)
static uint32_t ntohl(uint32_t x)
{ 
# ifdef XDR_ASSEMBLY
#  undef XDR_ASSEMBLY
# endif

# if (defined(__i386) || defined(__x86_64)) && !defined(_MSC_VER)
#  define XDR_ASSEMBLY
# endif

# if defined(XDR_ASSEMBLY) && defined(__has_feature)
#  if __has_feature(address_sanitizer)
#   undef XDR_ASSEMBLY
#  endif
# endif

# if defined(XDR_ASSEMBLY) && \
     (defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_THREAD__))
#  undef XDR_ASSEMBLY
# endif

# ifdef XDR_ASSEMBLY
  /* could write VC++ inline assembler, but not worth it for now */
  __asm__("xchgb %b0,%h0\n\t"	/* swap lower bytes	*/
	  "rorl $16,%0\n\t"	/* swap words		*/
	  "xchgb %b0,%h0"       /* swap higher bytes	*/
	  :"=q" (x)
	  : "0" (x));
  return x;
# else
  return((x << 24) | ((x & 0xff00) << 8) | ((x & 0xff0000) >> 8) | (x >> 24));
# endif

# ifdef XDR_ASSEMBLY
#  undef XDR_ASSEMBLY
# endif
}
#else /* net is big-endian: little-endian hosts need byte-swap code */
#ifndef WORDS_BIGENDIAN
/* #ifdef LITTLE_ENDIAN */
static uint32_t ntohl (uint32_t x)
{
  return((x << 24) | ((x & 0xff00) << 8) | ((x & 0xff0000) >> 8) | (x >> 24));
}
#else
#define ntohl(x) (x)
#endif
#endif
#define htonl ntohl

/*********************************************************************
 * RPC for the Windows NT Operating System
 * 1993 by Martin F. Gergeleit
 * Users may use, copy or modify RPC for the Windows NT Operating 
 * System according to the Oracle copyright below.
 *
 * RPC for the Windows NT Operating System COMES WITH ABSOLUTELY NO 
 * WARRANTY, NOR WILL I BE LIABLE FOR ANY DAMAGES INCURRED FROM THE 
 * USE OF. USE ENTIRELY AT YOUR OWN RISK!!!
 *********************************************************************/

/* @(#)xdr_stdio.c	2.1 88/07/29 4.0 RPCSRC */
/*
 * xdr_stdio.c, XDR implementation on standard i/o file.
 *
 * Copyright (c) 2010, Oracle America, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *     * Neither the name of the "Oracle America, Inc." nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 *   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This set of routines implements a XDR on a stdio stream.
 * XDR_ENCODE serializes onto the stream, XDR_DECODE de-serializes
 * from the stream.
 */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_stdio.c 1.16 87/08/11 Copyr 2010 Oracle America";
#endif

#include <rpc/types.h>
#include <stdio.h>
#include <rpc/xdr.h>

static bool_t	xdrstdio_getlong(XDR *, int32_t *);
static bool_t	xdrstdio_putlong(XDR *, int32_t *);
static bool_t	xdrstdio_getbytes(XDR *, caddr_t, u_int);
static bool_t	xdrstdio_putbytes(XDR *, caddr_t, u_int);
static u_int	xdrstdio_getpos(XDR *);
static bool_t	xdrstdio_setpos(XDR *, u_int);
static long *	xdrstdio_inline(XDR *, u_int);
static void	xdrstdio_destroy(XDR *);

/*
 * Ops vector for stdio type XDR
 */
static struct xdr_ops	xdrstdio_ops = {
	xdrstdio_getlong,	/* deserialize a long int */
	xdrstdio_putlong,	/* serialize a long int */
	xdrstdio_getbytes,	/* deserialize counted bytes */
	xdrstdio_putbytes,	/* serialize counted bytes */
	xdrstdio_getpos,	/* get offset in the stream */
	xdrstdio_setpos,	/* set offset in the stream */
	xdrstdio_inline,	/* prime stream for inline macros */
	xdrstdio_destroy	/* destroy stream */
};

/*
 * Initialize a stdio xdr stream.
 * Sets the xdr stream handle xdrs for use on the stream file.
 * Operation flag is set to op.
 */
void
xdrstdio_create(
	register XDR *xdrs,
	FILE *file,
	enum xdr_op op)
{

	xdrs->x_op = op;
	xdrs->x_ops = &xdrstdio_ops;
	xdrs->x_private = (caddr_t)file;
	xdrs->x_handy = 0;
	xdrs->x_base = 0;
}

/*
 * Destroy a stdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrstdio_create.
 */
static void
xdrstdio_destroy(
	register XDR *xdrs)
{
	if (xdrs->x_op == XDR_ENCODE) fflush((FILE *)xdrs->x_private);
	/* xx should we close the file ?? */
}

static bool_t
xdrstdio_getlong(XDR *xdrs, int32_t *lp)
{
	if (fread((caddr_t)lp, 4, 1, (FILE *)xdrs->x_private) != 1)
		return (FALSE);
	*lp = ntohl(*lp);
	return (TRUE);
}

static bool_t
xdrstdio_putlong(XDR *xdrs, int32_t *lp)
{
	int32_t mycopy = htonl(*lp);
	lp = &mycopy;
	if (fwrite((caddr_t)lp, 4, 1, (FILE *)xdrs->x_private) != 1)
		return (FALSE);
	return (TRUE);
}

static bool_t
xdrstdio_getbytes(
	XDR *xdrs,
	caddr_t addr,
	u_int len)
{

	if ((len != 0) && (fread(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
		return (FALSE);
	return (TRUE);
}

static bool_t
xdrstdio_putbytes(
	XDR *xdrs,
	caddr_t addr,
	u_int len)
{

	if ((len != 0) && (fwrite(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
		return (FALSE);
	return (TRUE);
}

static u_int
xdrstdio_getpos(
	XDR *xdrs)
{

	return ((u_int) ftell((FILE *)xdrs->x_private));
}

static bool_t
xdrstdio_setpos(
	XDR *xdrs,
	u_int pos)
{ 

	return ((fseek((FILE *)xdrs->x_private, (long)pos, 0) < 0) ?
		FALSE : TRUE);
}

static long *
xdrstdio_inline(
	XDR *xdrs,
	u_int len)
{

	/*
	 * Must do some work to implement this: must insure
	 * enough data in the underlying stdio buffer,
	 * that the buffer is aligned so that we can indirect through a
	 * long *, and stuff this pointer in xdrs->x_buf.  Doing
	 * a fread or fwrite to a scratch buffer would defeat
	 * most of the gains to be had here and require storage
	 * management on this buffer, so we don't do this.
	 */
	return (NULL);
}
