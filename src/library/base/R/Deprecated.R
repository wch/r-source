###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""))
}

provide <- function(package)
{
    .Deprecated()
    if (!exists(".Provided", inherits = TRUE))
	assign(".Provided", character(0), envir = .GlobalEnv)
    if (missing(package))
	.Provided
    else {
	package <- as.character(substitute(package))
	if (is.na(match(package, .packages())) &&
	    is.na(match(package, .Provided))) {
	    assign(".Provided", c(package, .Provided), envir = .GlobalEnv)
	    TRUE
	}
	else
	    FALSE
    }
}

