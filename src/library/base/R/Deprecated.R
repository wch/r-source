###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new, package=NULL) {
    warning(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
		  " is deprecated.\n",
		  if (!missing(new))
		  paste("Use", sQuote(new), "instead.\n"),
		  "See help(\"Deprecated\") ",
                  if(!is.null(package))
                  paste("and help(\"", package, "-deprecated\").", sep=""),
		  sep = ""),
            call. = FALSE)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 1.9.0
# tetragamma <- function(x) {
#     .Deprecated("psigamma(*, deriv=2)")
#     psigamma(x, deriv=2)
# }
## </entry>
