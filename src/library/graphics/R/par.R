#  File src/library/graphics/R/par.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

##-- These are the ones used in ../../../main/par.c  Query(..) :
##-- Documentation in		../../../include/Graphics.h
.Pars <- c(
           "xlog", "ylog", ## must be before xaxp, yaxp
	   "adj", "ann", "ask", "bg", "bty",
	   "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", "cin",
	   "col", "col.axis", "col.lab", "col.main", "col.sub",
           "cra", "crt", "csi","cxy",	"din", "err", "family",
           "fg", "fig", "fin",
	   "font", "font.axis", "font.lab", "font.main", "font.sub",
           "lab", "las", "lend", "lheight", "ljoin", "lmitre", "lty", "lwd",
           "mai", "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh",
	   "new", "oma", "omd", "omi", "page", "pch", "pin", "plt", "ps", "pty",
	   "smo", "srt", "tck", "tcl", "usr",
	   "xaxp", "xaxs", "xaxt",  "xpd",
	   "yaxp", "yaxs", "yaxt", "ylbias"
	   )
# Replaced with function to evaluate readonly pars because "gamma"
# was at one time readonly on a per-device basis
# .Pars.readonly <- c("cin","cra","csi","cxy","din")

par <- function (..., no.readonly = FALSE)
{
    .Pars.readonly <- c("cin","cra","csi","cxy","din","page")
    single <- FALSE
    args <- list(...)
    if (!length(args))
	args <- as.list(if (no.readonly) .Pars[-match(.Pars.readonly, .Pars)]
                        else .Pars)
    else {
	if (all(unlist(lapply(args, is.character))))
	    args <- as.list(unlist(args))
	if (length(args) == 1) {
	    if (is.list(args[[1L]]) | is.null(args[[1L]]))
		args <- args[[1L]]
	    else
		if(is.null(names(args)))
		    single <- TRUE
	}
    }
    value <- .External2(C_par, args)
    if(single) value <- value[[1L]]
    if(!is.null(names(args))) invisible(value) else value
}

clip <- function(x1, x2, y1, y2)
    invisible(.External.graphics(C_clip, x1, x2, y1, y2))

