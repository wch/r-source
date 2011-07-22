XDR routines for R, gnuwin32 version
====================================

R can make use of XDR to read/write binary dumps in a
machine-independent format. This directory was put together by Brian
Ripley from the version of the sunrpc distribution put together for
ONC RPC 1.10 for Windows NT by Martin F. Gergeleit. This is no longer
available directly, but is part of the NISGINA distribution
<www.dcs.qmw.ac.uk/~williams>.  Only that part of XDR used by R is
included.

[A later version of ONC/RPC is available at 
http://www.plt.rwth-aachen.de/ks/english/oncrpc.html]

For copyright see the Sun copyright in COPYRGHT.TXT and the header
of xdr.c.

The only changes I made were

(a) to comment out definitions in rpc/types.h that are duplicates of
those in Windows/Sockets.h

(b) to define ntohl and htonl via assembler code at the head of 
xdr_stdio.c. This will need to be altered for any other little-endian
system; generic code for 32-bit systems is provided.

(c) to report errors directly rather than to NT services.

Note that this code assume long is 4 bytes, and also implicitly
assumes that a pointer can be stored in a long.

These functions are only used in saveload.c, which uses xdr_bytes,
xdr_double, xdr_int, xdr_string, xdrmem_create and xdrstdio_create.

There was some further tidying up in July 2011 to enable this to be
used on other systems.  In particular, we replaced 'long' by int32_t
and 'u_long' by uint32_t to ensure 32-bit types.
