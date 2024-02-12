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

## Deprecated in 4.4.0
is.R <- function() {
    p <- Sys.getenv("_R_DEPRECATED_IS_R_")
    if(nzchar(p)) {
        choices <-c("warn", "error", "traceback", "where", "self")
        if (!is.na(i<- pmatch(p, choices)))
            p <- choices[i]
        else
            message("unknown value of _R_DEPRECATED_IS_R_ ", sQuote(p),
                    " will be ignored")
        warn_now <- function(msg) {
            ## Avoid deferred warnings:
            ## warning(<condition>) does not support immediate. = TRUE
            op <- getOption("warn")
            if(op == 0L) options(warn = 1L)
            warning(warningCondition(msg, class = "deprecatedWarning",
                                     call = sys.call(sys.parent())))
            if(op == 0L) options(warn = op)
        }
        msg <- paste0(gettextf("'%s' is deprecated.\n", "is.R"),
                      'See help("Deprecated") and help("base-deprecated")')
        if (p == "warn") {
            ## As default, buy immediately.
            warn_now(msg)
        }
        else if (p == "error") {
            .Deprecated(package = "base")
            ## stop will flush out deferred warnings
            ## temporary, so do not translate
            stop('deprecation turned into an error', domain = NA)
        }
        else if (p == "traceback") {
            calls <- rev(sys.calls())
            msg <- paste0(msg, "\nCalls:\n",
                          paste0(sprintf("%2i: ", seq_along(calls)),
                                 vapply(calls, deparse1, "",
                                        collapse = "\n    "),
                                 collapse = "\n"))
            message(msg, domain = NA)
        }
        else if (p == "where") {
            call <- sys.call(-1)
            if(!is.null(call)) {
                msg <- paste0(msg,"\nCall: ", deparse1(call))
                env <- environment(sys.function(-1))
                if (isNamespace(env)) {
                    env <- getNamespaceName(env)
                    msg <- paste0(msg, "\n",
                                  sprintf("from namespace %s", sQuote(env)))
                }
            }
            warn_now(msg)
        }
        else if (p == "self") {
            ## Do not warn if called from a namespace other than that under test
            call <- sys.call(-1)
            if(!is.null(call)) {
                msg <- paste0(msg,"\nCall: ", deparse1(call))
                env <- environment(sys.function(-1))
                if (isNamespace(env)) {
                    env <- getNamespaceName(env)
                    this <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", "unknown")
                    if (this != env) return(TRUE)
                }
            }
            warn_now(msg)
        }
    } else
        .Deprecated(package = "base")

    TRUE
}
