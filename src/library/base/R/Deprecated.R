###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""), call. = FALSE)
}

## consider keeping one (commented) entry here, for easier additions
## <entry>
## Deprecated in 1.8.0
## when it is removed, remove also from stoplists in
## methods (base/R/objects.R) and tools/R/Utils.R
print.coefmat <-
    function(x, digits=max(3, getOption("digits") - 2),
             signif.stars = getOption("show.signif.stars"),
             dig.tst = max(1, min(5, digits - 1)),
             cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
             P.values = NULL,
             has.Pvalue = nc >= 4 && substr(colnames(x)[nc],1,3) == "Pr(",
             eps.Pvalue = .Machine$double.eps,
             na.print = "", ...)
{
    .Deprecated("printCoefmat")
    Call <- match.call(expand.dots = TRUE)
    if(missing(na.print)) Call$na.print <- ""
    Call[[1]] <- as.name("printCoefmat")
    eval.parent(Call)
}
## </entry>

