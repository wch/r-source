#  File src/library/utils/R/question.R
#  Part of the R package, http://www.R-project.org
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

"?" <- function(e1, e2)
{
    if (missing(e2)) {
        type <- NULL
    	topicExpr <- substitute(e1)
    } else {
     	type <- substitute(e1)
     	topicExpr <- substitute(e2)
    } 	
    if (is.call(topicExpr) && topicExpr[[1]] == "?") {
            # ??foo is parsed as `?`(`?`(foo))
            	search <- TRUE
            	topicExpr <- topicExpr[[2]]
            	if (is.call(topicExpr) && topicExpr[[1]] == "?"
            	    && is.call(topicExpr[[2]]) && topicExpr[[2]][[1]] == "?") {
            	     cat("Contacting Delphi...")
            	     flush.console()
            	     Sys.sleep(2+rpois(1,2))
            	     cat("the oracle is unavailable.\nWe apologize for any inconvenience.\n")
            	     return(invisible())
            	 }
            } else 
            	search <- FALSE

    if (is.call(topicExpr) && (topicExpr[[1]] == "::" || topicExpr[[1]] == ":::")) {
		package <- as.character(topicExpr[[2]])
		topicExpr <- topicExpr[[3]]
	    } else 
		package <- NULL

    if (search) {
	if (is.null(type))
	    return(eval(substitute(help.search(TOPIC, package = PACKAGE), 
							list(TOPIC = as.character(topicExpr),
							      PACKAGE = package))))
	else
	    return(eval(substitute(help.search(TOPIC, fields = FIELD, package = PACKAGE),
	    						list(TOPIC = as.character(topicExpr),
	    						      FIELD = as.character(type),
	    						      PACKAGE = package))))
    } else {
        if (is.null(type)) {
	    if (is.call(topicExpr))
		return(.helpForCall(topicExpr, parent.frame()))
	    if (is.name(topicExpr))
	    	topic <- as.character(topicExpr)
	    else
	    	topic <- e1
	    return(eval(substitute(help(TOPIC, package = PACKAGE), 
							list(TOPIC = topic,
							      PACKAGE = package))))
	} else {
	    ## interpret e1 as a type, but to allow customization, do NOT
            ## force arbitrary expressions to be single character strings
            ## (so that methods can be defined for topicName).	    
            if (is.name(type))
		type <- as.character(type)
	    else
		type <- e1
	    if (is.name(topicExpr))
		topic <- as.character(topicExpr)
            else {
            	if (is.call(topicExpr) && identical(type, "method"))
            	    return(.helpForCall(topicExpr, parent.frame(), FALSE))
            	topic <- e2
	    }
	    doHelp <- .tryHelp(topicName(type, topic), package = package)
	    if(inherits(doHelp, "try-error")) {
                if(is.language(topicExpr))
                  topicExpr <- deparse(topicExpr)
		stop(gettextf("no documentation of type '%s' and topic '%s' (or error in processing help)", type, topicExpr), domain = NA)
            }
        }
    }
}

topicName <- function(type, topic)
{
    if((length(type) == 0) || (length(topic) == 0))
        character(0)
    else
        paste(paste(topic, collapse = ","), type, sep = "-")
}

.helpForCall <- function(expr, envir, doEval = TRUE) {
    f <- expr[[1]]                      # the function specifier
    where <- topenv(envir)              # typically .GlobalEnv
    if(is.name(f))
        f <- as.character(f)
    if(!.isMethodsDispatchOn() || !methods::isGeneric(f, where = where)) {
        if(!is.character(f) || length(f) != 1)
            stop(gettextf("the object of class \"%s\" in the function call '%s' could not be used as a documentation topic",
                          class(f), deparse(expr)), domain = NA)
        h <- .tryHelp(f)
        if(inherits(h, "try-error"))
            stop(gettextf("no methods for '%s' and no documentation for it as a function", f), domain = NA)
    }
    else {
        ## allow generic function objects or names
        if(methods::is(f, "genericFunction")) {
            fdef <- f
            f <- fdef@generic
        }
        else
            fdef <- methods::getGeneric(f, where = where)
        call <- match.call(fdef, expr)
        ## make the signature
        sigNames <- fdef@signature
        sigClasses <- rep.int("ANY", length(sigNames))
        names(sigClasses) <- sigNames
        for(arg in sigNames) {
            argExpr <- methods::elNamed(call, arg)
            if(!is.null(argExpr)) {
                simple <- (is.character(argExpr) || is.name(argExpr))
                ## TODO:  ideally, if doEval is TRUE, we would like to
                ## create the same context used by applyClosure in
                ## eval.c, but then skip the actual evaluation of the
                ## body.  If we could create this environment then
                ## passing it to selectMethod is closer to the semantics
                ## of the "real" function call than the code below.
                ## But, seems to need a change to eval.c and a flag to
                ## the evaluator.
                if(doEval || !simple) {
                    argVal <- try(eval(argExpr, envir))
                    if(methods::is(argVal, "try-error"))
                        stop(gettextf("error in trying to evaluate the expression for argument '%s' (%s)",
                                      arg, deparse(argExpr)),
                             domain = NA)
                    sigClasses[[arg]] <- class(argVal)
                }
                else
                    sigClasses[[arg]] <- as.character(argExpr)
            }
        }
        method <- methods::selectMethod(f, sigClasses, optional=TRUE,
                                        fdef = fdef)
        if(methods::is(method, "MethodDefinition")) {
            sigClasses <- method@defined
            if(length(sigClasses) < length(sigNames))
              sigClasses<- c(sigClasses, rep("ANY", length(sigNames)-length(sigClasses)))
        }
        else
            warning(gettextf("no method defined for function '%s' and signature '%s'",
                             f, paste(sigNames, " = ", dQuote(sigClasses),
                                      sep = "", collapse = ", ")), domain = NA)
        topic <- topicName("method", c(f,sigClasses))
        h <- .tryHelp(topic)
        if(methods::is(h, "try-error"))
            stop(gettextf("no documentation for function '%s' and signature '%s'",
                 f, paste(sigNames, " = ", dQuote(sigClasses), sep = "",
                          collapse = ", ")), domain = NA)
    }
}

.tryHelp <- function(topic, package = NULL) {
    opts <- options(error = function() TRUE,
                    show.error.messages = FALSE)
    on.exit(options(opts))
    x <- try(do.call("help", list(topic, package = package)))
    ## If something was found, actually show it via print().
    ## Otherwise, give an error.
    if(!inherits(x, "try-error") && length(x))
        print(x)
    else
        try(stop())
    ## Argh ...
}
