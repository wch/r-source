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

/* @(#)xdr_float.c	2.1 88/07/29 4.0 RPCSRC */
/*
 * xdr_float.c, Generic XDR routines impelmentation.
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
 * These are the "floating point" xdr routines used to (de)serialize
 * most common data items.  See xdr.h for more info on the interface to
 * xdr.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdint.h>

#include <rpc/types.h>
#include <rpc/xdr.h>

bool_t
xdr_double(XDR *xdrs, double *dp)
{
    int32_t *lp;

    switch (xdrs->x_op) {
    case XDR_ENCODE:
	lp = (int32_t *)dp;
#ifdef WORDS_BIGENDIAN
	return (XDR_PUTLONG(xdrs, lp++) && XDR_PUTLONG(xdrs, lp));
#else
	return (XDR_PUTLONG(xdrs, lp+1) && XDR_PUTLONG(xdrs, lp));
#endif
    case XDR_DECODE:
	lp = (int32_t *)dp;
#ifdef WORDS_BIGENDIAN
	return (XDR_GETLONG(xdrs, lp++) && XDR_GETLONG(xdrs, lp));
#else
	return (XDR_GETLONG(xdrs, lp+1) && XDR_GETLONG(xdrs, lp));
#endif
    case XDR_FREE:
	return (TRUE);
    }
    return (FALSE);
}
