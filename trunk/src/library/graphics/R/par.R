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
           "gamma", "lab", "las", "lend", "lheight", "ljoin", "lmitre",
           "lty", "lwd",
           "mai", "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh",
	   "new", "oma", "omd", "omi", "pch", "pin", "plt", "ps", "pty",
	   "smo", "srt", "tck", "tcl", "tmag", "usr",
	   "xaxp", "xaxs", "xaxt",  "xpd",
	   "yaxp", "yaxs", "yaxt"
	   )
# Replaced with function to evaluate readonly pars because "gamma"
# is readonly on a per-device basis
# .Pars.readonly <- c("cin","cra","csi","cxy","din")

par <- function (..., no.readonly = FALSE)
{
    single <- FALSE
    args <- list(...)
    if (!length(args))
	args <- as.list(if (no.readonly)
                        .Pars[-match(.Internal(readonly.pars()), .Pars)]
        else .Pars)
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
