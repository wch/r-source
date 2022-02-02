#  File src/library/base/R/attr.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

`mostattributes<-` <- function(x, value)
{
    if(length(value)) {
	if(!is.list(value)) stop("'value' must be a list")
	if(h.nam <- !is.na(inam <- match("names", names(value)))) {
	    n1 <- value[[inam]];	value <- value[-inam] }
	if(h.dim <- !is.na(idin <- match("dim", names(value)))) {
	    d1 <- value[[idin]];	value <- value[-idin] }
	if(h.dmn <- !is.na(idmn <- match("dimnames", names(value)))) {
	    dn1 <- value[[idmn]];	value <- value[-idmn] }
	attributes(x) <- value
        dm <- attr(x, "dim")
	## for list-like objects with a length() method, e.g. POSIXlt
	L <- length(if(is.list(x)) unclass(x) else x)
        ## Be careful to set dim before dimnames.
	if(h.dim && L == prod(d1)) attr(x, "dim") <- dm <- d1
	if(h.dmn && !is.null(dm)) {
            ddn <- vapply(dn1, length, 1, USE.NAMES=FALSE)
            if( all((dm == ddn)[ddn > 0]) ) attr(x, "dimnames") <- dn1
        }
        ## don't set if it has 'dim' now
	if(h.nam && is.null(dm) && L == length(n1)) attr(x, "names") <- n1
    }
    x
}
