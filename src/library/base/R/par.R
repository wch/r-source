##-- These are the ones used in ../../../main/par.c  Query(..) :
##-- Documentation in		../../../include/Graphics.h
.Pars <- c(
	   "adj", "ann", "ask", "bg", "bty",
	   "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", "cin",
	   "col", "col.axis", "col.lab", "col.main", "col.sub",
           "cra", "crt", "csi","cxy",	"din", "err", "fg", "fig", "fin",
	   "font", "font.axis", "font.lab", "font.main", "font.sub",
           "lab", "las", "lty", "lwd",
           "mai", "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh",
	   "new", "oma", "omd", "omi", "pch", "pin", "plt", "ps", "pty",
	   "smo", "srt", "tck", "tmag", "type", "usr",
	   "xaxp", "xaxs", "xaxt", "xlog", "xpd",
	   "yaxp", "yaxs", "yaxt", "ylog",
	   ##-- newer ones:
	   "gamma", "tcl"
	   )
.Pars.readonly <- c("cin","cra","csi","cxy","din")

par <- function (..., no.readonly = FALSE)
{
    single <- FALSE
    args <- list(...)
    if (!length(args))
	args <- as.list(if(no.readonly)
                        .Pars[-match(.Pars.readonly, .Pars)] else .Pars)
    else {
	if (all(unlist(lapply(args, is.character))))
	    args <- as.list(unlist(args))
	if (length(args) == 1) {
	    if (is.list(args[[1]]) | is.null(args[[1]]))
		args <- args[[1]]
	    else
		if(is.null(names(args)))
		    single <- TRUE
	}
    }
    value <-
        if (single) .Internal(par(args))[[1]] else .Internal(par(args))
    if(!is.null(names(args))) invisible(value) else value
}





