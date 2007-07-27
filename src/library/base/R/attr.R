#  File src/library/base/R/attr.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

"mostattributes<-" <- function(obj, value) {
    if(length(value)) {
	if(!is.list(value)) stop("RHS must be list")
	if(h.nam <- !is.na(inam <- match("names", names(value)))) {
	    n1 <- value[[inam]];	value <- value[-inam] }
	if(h.dim <- !is.na(idin <- match("dim", names(value)))) {
	    d1 <- value[[idin]];	value <- value[-idin] }
	if(h.dmn <- !is.na(idmn <- match("dimnames", names(value)))) {
	    dn1 <- value[[idmn]];	value <- value[-idmn] }
	attributes(obj) <- value
        dm <- dim(obj)
	if(h.nam && is.null(dm) && length(obj) == length(n1))
	    names(obj) <- n1
	if(h.dim && length(obj) == prod(d1))
	    dim(obj) <- dm <- d1
	if(h.dmn && !is.null(dm)) {
            ddn <- sapply(dn1, length)
            if( all((dm == ddn)[ddn > 0]) ) dimnames(obj) <- dn1
        }
    }
    obj
}
