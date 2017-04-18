#  File src/library/utils/R/question.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

`?` <-
function(e1, e2)
{
    if (missing(e2)) {
	type <- NULL
	topicExpr <- substitute(e1)
    } else {
	type <- substitute(e1)
	topicExpr <- substitute(e2)
    }
    search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
    if(search) { # ??foo is parsed as `?`(`?`(foo))
	topicExpr <- topicExpr[[2L]]
	if (is.call(te <- topicExpr	 ) && te[[1L]] == "?" &&
	    is.call(te <- topicExpr[[2L]]) && te[[1L]] == "?") {
	    cat("Contacting Delphi...")
	    flush.console()
	    Sys.sleep(2 + stats::rpois(1,2))
	    cat("the oracle is unavailable.\nWe apologize for any inconvenience.\n")
	    return(invisible())
	}
    }

    if (is.call(topicExpr) && (topicExpr[[1L]] == "::" ||
			       topicExpr[[1L]] == ":::")) {
	package <- as.character(topicExpr[[2L]])
	topicExpr <- topicExpr[[3L]]
    }
    else
	package <- NULL

    if (search) {
	if(is.null(type))
	    return(eval(substitute(help.search(TOPIC, package = PACKAGE),
				   list(TOPIC = as.character(topicExpr),
					PACKAGE = package))))
	else
	    return(eval(substitute(help.search(TOPIC, fields = FIELD,
					       package = PACKAGE),
				   list(TOPIC = as.character(topicExpr),
					FIELD = as.character(type),
					PACKAGE = package))))
    } else {
	if (is.null(type)) {
	    if (is.call(topicExpr))
		return(.helpForCall(topicExpr, parent.frame()))
	    topic <-
		if(is.name(topicExpr)) as.character(topicExpr) else e1
	    return(eval(substitute(help(TOPIC, package = PACKAGE),
				   list(TOPIC = topic,
					PACKAGE = package))))
	} else {
	    ## interpret e1 as a type, but to allow customization, do NOT
	    ## force arbitrary expressions to be single character strings
	    ## (so that methods can be defined for topicName).
	    type <-
		if(is.name(type)) as.character(type) else e1
	    topic <-
		if(is.name(topicExpr)) as.character(topicExpr)
		else {
		    if (is.call(topicExpr) && identical(type, "method"))
			return(.helpForCall(topicExpr, parent.frame(), FALSE))
		    e2
		}
	    if (type == "package")
	    	package <- topic
            h <- .tryHelp(topicName(type, topic), package = package)
            if(is.null(h)) {
		if(is.language(topicExpr))
		    topicExpr <- deparse(topicExpr)
		stop(gettextf("no documentation of type %s and topic %s (or error in processing help)",
			      sQuote(type), sQuote(topicExpr)),
                     domain = NA)
	    }
            h
	}
    }
}

topicName <-
function(type, topic)
{
    if((length(type) == 0L) || (length(topic) == 0L))
        character(0L)
    else
        paste(paste(topic, collapse = ","), type, sep = "-")
}

.helpForCall <-
function(expr, envir, doEval = TRUE)
{
    ## There should really be a common way of formatting signatures.
    sigFormat <- function(sigNames, sigClasses) {
        paste(sprintf("%s = \"%s\"", sigNames, sigClasses),
              collapse = ", ")
    }

    f <- expr[[1L]]                     # the function specifier
    if (is.call(f) && (f[[1L]] == "::" || f[[1L]] == ":::")) {
	package <- f[[2L]]
	where <- paste0("package:", package)
	if (!(where %in% search()))
	    where <- NULL
	f <- f[[3L]]
    } else {
	package <- NULL
        where <- topenv(envir)              # typically .GlobalEnv
    }
    if(is.name(f))
        f <- as.character(f)
    if(is.null(where) || !.isMethodsDispatchOn() || !methods::isGeneric(f, where = where)) {
        if(!is.character(f) || length(f) != 1L)
            stop(gettextf("the object of class %s in the function call %s could not be used as a documentation topic",
                          dQuote(class(f)), sQuote(deparse(expr))),
                 domain = NA)
        h <- .tryHelp(f, package = package)
        if(is.null(h))
            stop(gettextf("no methods for %s and no documentation for it as a function",
                          sQuote(f)),
                 domain = NA)
    }
    else {
        ## allow generic function objects or names
        if(methods::is(f, "genericFunction")) {
            fdef <- f
            f <- fdef@generic
        }
        else
            fdef <- methods::getGeneric(f, where = where)
        sigClasses <- .signatureFromCall(fdef, expr, envir, doEval)
        sigNames <- names(sigClasses)
        method <- methods::selectMethod(f, sigClasses, optional=TRUE,
                                        fdef = fdef)
        if(methods::is(method, "MethodDefinition")) {
            sigClasses <- method@defined
            if(length(sigClasses) < length(sigNames))
                sigClasses <-
                    c(sigClasses,
                      rep.int("ANY", length(sigNames) - length(sigClasses)))
        }
        else
            warning(gettextf("no method defined for function %s and signature %s",
                             sQuote(f),
                             sQuote(sigFormat(sigNames, sigClasses))),
                    domain = NA)
        topic <- topicName("method", c(f, sigClasses))
        h <- .tryHelp(topic, package = package)
        if(is.null(h))
            stop(gettextf("no documentation for function %s and signature %s",
                          sQuote(f),
                          sQuote(sigFormat(sigNames, sigClasses))),
                 domain = NA)
    }

    h
}

.tryHelp <-
function(topic, package = NULL)
{
    ## Try finding help.
    ## Return NULL (nothing) in case we found no help pages, or an
    ## error.
    ## (Earlier versions showed what they found via print(), or gave
    ## an error.)
    h <- tryCatch(do.call("help", list(topic, package = package)),
                  error = identity)
    if(inherits(h, "error") || !length(h)) NULL else h
}
