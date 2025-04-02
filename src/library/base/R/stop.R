#  File src/library/base/R/stop.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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

stop <- function(..., call. = TRUE, domain = NULL)
{
    if (...length() == 1L && inherits(..1, "condition")) {
        cond <- ..1
        if(nargs() > 1L)
            warning("additional arguments ignored in stop()")
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        .Internal(.signalCondition(cond, message, call))
        .Internal(.dfltStop(message, call))
    } else
        .Internal(stop(call., .makeMessage(..., domain = domain)))
}

stopifnot <- function(..., exprs, exprObject, local = TRUE)
{
    n <- ...length()
    if((has.e <- !missing(exprs)) || !missing(exprObject)) {
	if(n || (has.e && !missing(exprObject)))
	    stop("Only one of 'exprs', 'exprObject' or expressions, not more")
	envir <- if (isTRUE(local)) parent.frame()
		 else if(isFALSE(local)) .GlobalEnv
		 else if (is.environment(local)) local
		 else stop("'local' must be TRUE, FALSE or an environment")
	E1 <- if(has.e && is.call(exprs <- substitute(exprs))) exprs[[1]]
	cl <- if(is.symbol(E1) &&
		 E1 == quote(`{`)) {
		  exprs[[1]] <- quote(stopifnot) ## --> stopifnot(*, *, ..., *) :
		  exprs
	      }
	      else
		  as.call(c(quote(stopifnot),
			    if(!has.e) exprObject else as.expression(exprs))) # or fail ..
        names(cl) <- NULL
	return(eval(cl, envir=envir))
    }
    ## else   use '...' (and not 'exprs') :

    Dparse <- function(call, cutoff = 60L) {
	ch <- deparse(call, width.cutoff = cutoff)
	if(length(ch) > 1L) paste(ch[1L], "....") else ch
    }
    head <- function(x, n = 6L) ## basically utils:::head.default()
	x[seq_len(if(n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
    abbrev <- function(ae, n = 3L)
	paste(c(head(ae, n), if(length(ae) > n) "...."), collapse="\n  ")
    ##
    for (i in seq_len(n)) {
	r <- ...elt(i)
	if (!(is.logical(r) && !anyNA(r) && all(r))) {
	  dots <- match.call()[-1L]
          if(is.null(msg <- names(dots)) || !nzchar(msg <- msg[i])) {
	    cl.i <- dots[[i]]
	    msg <- ## special case for decently written 'all.equal*(*)':
		if(is.call(cl.i) &&
                   identical(1L, pmatch(quote(all.equal), cl.i[[1]])) &&
		   (is.null(ni <- names(cl.i)) || length(cl.i) == 3L ||
		    length(cl.i <- cl.i[!nzchar(ni)]) == 3L))

		    sprintf(gettext("%s and %s are not equal:\n  %s"),
			    Dparse(cl.i[[2]]),
			    Dparse(cl.i[[3]]), abbrev(r))
		else
		    sprintf(ngettext(length(r),
				     "%s is not TRUE",
				     "%s are not all TRUE"),
			    Dparse(cl.i))
	  }
	    stop(simpleError(msg, call = if(p <- sys.parent(1L)) sys.call(p)))
        }
    }
    invisible()
}

warning <- function(..., call. = TRUE, immediate. = FALSE,
                    noBreaks. = FALSE, domain = NULL)
{
    if (...length() == 1L && inherits(..1, "condition")) {
        cond <- ..1
        if(nargs() > 1L)
            cat(gettext("additional arguments ignored in warning()"),
                "\n", sep = "", file = stderr())
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        withRestarts({
                .Internal(.signalCondition(cond, message, call))
                .Internal(.dfltWarn(message, call))
            }, muffleWarning = function() NULL) #**** allow simpler form??
        invisible(message)
    } else
        .Internal(warning(call., immediate., noBreaks.,
                          .makeMessage(..., domain = domain)))
}

gettext <- function(..., domain = NULL, trim = TRUE) {
    char <- unlist(lapply(list(...), as.character))
    .Internal(gettext(domain, char, trim))
}

bindtextdomain <- function(domain, dirname = NULL)
    .Internal(bindtextdomain(domain, dirname))

ngettext <- function(n, msg1, msg2, domain = NULL)
    .Internal(ngettext(n, msg1, msg2, domain))

gettextf <- function(fmt, ..., domain = NULL, trim = TRUE)
    sprintf(gettext(fmt, domain=domain, trim=trim), ...)

Sys.setLanguage <- function(lang, unset = "en") #, force = FALSE
{
    stopifnot(is.character(lang), length(lang) == 1L, # e.g., "es" , "fr_CA"
              lang == "C" || grepl("^[a-z][a-z]", lang))
    curLang <- Sys.getenv("LANGUAGE", unset = NA) # so it can be reset
    if(is.na(curLang) || !nzchar(curLang))
        curLang <- unset # "factory" default
    if (!capabilities("NLS") || is.na(.popath)) {
        warning("no natural language support or missing translations",
                domain = NA)
        return(invisible(structure(curLang, ok = FALSE)))
    }
    if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX") && # e.g. LC_ALL=C
        lang != "C") {
        ## POSIX 1003.1-2024 specifies that LANGUAGE shall not override
        ## a C locale (GNU gettext also ignores LANGUAGE in C.UTF-8),
        ## and Sys.setLanguage() shouldn't try that either (by default).
        ## Most languages need non-ASCII characters, but those would get
        ## displayed as ? or * (or worse) in a C locale and attempts to
        ## change the session charset are known to be fragile.
#     if(force) {
#       lcSet <- if(.Platform[["OS.type"]] == "unix") # works to "undo LC_ALL=C"
#                    paste0(collapse="", vapply(c("LC_ALL", "LC_MESSAGES"),
#                                               \(a) Sys.setlocale(a, "en_US.UTF-8"), ""))
#       ## TODOs: 1) we assume   en_US.UTF-8  exists on all "unix"
#       ##        2) How to deal w/ Windows ? {can set things but with *no* effect}
#       ok.lc <- !is.null(lcSet) && nzchar(lcSet) # NULL or ""  are not ok
#       if(!ok.lc)
#           warning("in a C locale: LANGUAGE reset, but message language may be unchanged")
#     } else { # !force (default) :
        warning("in a C locale: cannot set language", domain = NA)
        return(invisible(structure(curLang, ok = FALSE)))
#     }
    } else ok.lc <- TRUE
    ok <- Sys.setenv(LANGUAGE=lang)
    if(!ok)
        warning(gettextf('Sys.setenv(LANGUAGE="%s") may have failed', lang), domain=NA)
    ok. <- #capabilities("NLS") && # asserted above
        isTRUE(bindtextdomain(NULL)) # only flush the cache (of already translated strings)
    invisible(structure(curLang, ok = ok && ok.lc && ok.))
}
