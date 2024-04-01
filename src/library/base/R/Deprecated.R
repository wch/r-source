#  File src/library/base/R/Deprecated.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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

###----- NOTE:	../man/base-deprecated.Rd   must be synchronized with this file!
###		-------------------------
.Deprecated <- function(new, package = NULL, msg,
			old = as.character(sys.call(sys.parent()))[1L])
{
    msg <- if( missing(msg) ) {
	msg <- gettextf("'%s' is deprecated.\n", old)
	if(!missing(new))
	    msg <- c(msg, gettextf("Use '%s' instead.\n", new))
	c(msg,
	  if(!is.null(package))
	  gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").",
		   package)
	  else gettext("See help(\"Deprecated\")"))
    }
    else as.character(msg)
    msg <- paste(msg, collapse = "")

    if (missing(new)) new <- NULL
    warning(warningCondition(msg, old = old, new = new, package = package,
                             class = "deprecatedWarning",
                             call = sys.call(sys.parent())))
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Docu-deprecated in 4.1.0
## Formally deprecated in 4.2.0
## default.stringsAsFactors <- function()
## {
##     .Deprecated("`stringsAsFactors = FALSE`")
##     val <- getOption("stringsAsFactors")
##     if(is.null(val)) val <- FALSE
##     if(length(val) != 1L || !is.logical(val) || is.na(val))
##         stop('options("stringsAsFactors") not set to TRUE or FALSE')
##     val
## }
## </entry>


## Docu-deprecated in 4.3.0
## Formally deprecated in 4.4.0

## as.data.frame.raw <- function(x, row.names = NULL, optional = FALSE, ..., nm = deparse1(substitute(x))) {.....}
## as.data.frame.logical ....
## as.data.frame.integer ...
## as.data.frame.numeric ..
## as.data.frame.complex
## as.data.frame.factor
## as.data.frame.ordered
## as.data.frame.Date
## as.data.frame.difftime
## as.data.frame.POSIXct
## as.data.frame.noquote
## as.data.frame.numeric_version
##                vvvvv
## >>> *end* of ./zzz.R : creating a modified version of as.data.frame.vector and assigning that in for() loop
