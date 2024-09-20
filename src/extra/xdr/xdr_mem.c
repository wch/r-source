#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdint.h>

#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
 typedef unsigned long uintptr_t;
#endif

/* Local mod: the assembly assumes i386 and little-endian generic is 32-bit */
#if defined(_WIN32) && !defined(_WIN64)
static uint32_t ntohl(uint32_t x)
{ /* could write VC++ inline assembler, but not worth it for now */
#ifdef _MSC_VER
  return((x << 24) | ((x & 0xff00) << 8) | ((x & 0xff0000) >> 8) | (x >> 24));
#else
  __asm__("xchgb %b0,%h0\n\t"	/* swap lower bytes	*/
	  "rorl $16,%0\n\t"	/* swap words		*/
	  "xchgb %b0,%h0"       /* swap higher bytes	*/
	  :"=q" (x)
	  : "0" (x));
  return x;
#endif 
}
#else /* net is big-endian: little-endian hosts need byte-swap code */
#ifndef WORDS_BIGENDIAN
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

/* @(#)xdr_mem.c	2.1 88/07/29 4.0 RPCSRC */
/*
 * xdr_mem.c, XDR implementation using memory buffers.
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
 * If you have some data to be interpreted as external data representation
 * or to be converted to external data representation in a memory buffer,
 * then this is the package for you.
 */
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_mem.c 1.19 87/08/11 Copyr 2010 Oracle America";
#endif

#include <string.h> /* for memcpy */
#include <rpc/types.h>
#include <rpc/xdr.h>

/* The use of 'long' here would be problematic if it were ever to be
   used on a 64-bit system */

static bool_t	xdrmem_getlong(XDR *xdrs, int32_t *lp);
static bool_t	xdrmem_putlong(XDR *xdrs, int32_t *lp);
static bool_t	xdrmem_getbytes(XDR *, caddr_t, u_int);
static bool_t	xdrmem_putbytes(XDR *, caddr_t, u_int);
static u_int	xdrmem_getpos(XDR *);
static bool_t	xdrmem_setpos(XDR *, u_int);
static long *	xdrmem_inline(XDR *, u_int);
static void	xdrmem_destroy(XDR *);

static struct	xdr_ops xdrmem_ops = {
	xdrmem_getlong,
	xdrmem_putlong,
	xdrmem_getbytes,
	xdrmem_putbytes,
	xdrmem_getpos,
	xdrmem_setpos,
	xdrmem_inline,
	xdrmem_destroy
};

/*
 * The procedure xdrmem_create initializes a stream descriptor for a
 * memory buffer.  
 */
void
xdrmem_create(
	register XDR *xdrs,
	caddr_t addr,
	u_int size,
	enum xdr_op op)
{

	xdrs->x_op = op;
	xdrs->x_ops = &xdrmem_ops;
	xdrs->x_private = xdrs->x_base = addr;
	xdrs->x_handy = size;
}

static void
xdrmem_destroy(
	XDR *xdrs)
{
}

static bool_t
xdrmem_getlong(XDR *xdrs, int32_t *lp)
{

	if ((xdrs->x_handy -= 4) < 0)
		return (FALSE);
	*lp = (int32_t)ntohl((uint32_t)(*((int32_t *)(xdrs->x_private))));
	xdrs->x_private += 4; /* This relies on 4 bytes/long */
	return (TRUE);
}

static bool_t
xdrmem_putlong(XDR *xdrs, int32_t *lp)
{

	if ((xdrs->x_handy -= 4) < 0)
		return (FALSE);
	*(int32_t *)xdrs->x_private = (int32_t)htonl((uint32_t)(*lp));
	xdrs->x_private += 4;
	return (TRUE);
}

static bool_t
xdrmem_getbytes(
	register XDR *xdrs,
	caddr_t addr,
	register u_int len)
{

	if ((xdrs->x_handy -= len) < 0)
		return (FALSE);
	if (len) memcpy(addr, xdrs->x_private, len);
	xdrs->x_private += len;
	return (TRUE);
}

static bool_t
xdrmem_putbytes(
	register XDR *xdrs,
	caddr_t addr,
	register u_int len)
{

	if ((xdrs->x_handy -= len) < 0)
		return (FALSE);
	if (len) memcpy(xdrs->x_private, addr, len);
	xdrs->x_private += len;
	return (TRUE);
}

static u_int
xdrmem_getpos(
	register XDR *xdrs)
{

	return ((uintptr_t)xdrs->x_private - (uintptr_t)xdrs->x_base);
}

static bool_t
xdrmem_setpos(
	register XDR *xdrs,
	u_int pos)
{
	register caddr_t newaddr = xdrs->x_base + pos;
	register caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;

	if ((uintptr_t)newaddr > (uintptr_t)lastaddr)
		return (FALSE);
	xdrs->x_private = newaddr;
	xdrs->x_handy = (uintptr_t)lastaddr - (uintptr_t)newaddr;
	return (TRUE);
}

static long *
xdrmem_inline(
	register XDR *xdrs,
	u_int len)
{
	long *buf = 0;

	if (xdrs->x_handy >= len) {
		xdrs->x_handy -= len;
		buf = (long *) xdrs->x_private;
		xdrs->x_private += len;
	}
	return (buf);
}
