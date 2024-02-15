#  File src/library/utils/R/fineLineNum.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2009-2023 Duncan Murdoch and the R Core Team
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


.normalizePath <- function(path, wd) {
    if (!missing(wd) && !is.null(wd)) {
    	oldwd <- setwd(wd)
    	on.exit(setwd(oldwd))
    }
    suppressWarnings(normalizePath(path))
}

fnLineNum <- function(f, srcfile, line, nameonly=TRUE) {
    stopifnot(length(line) == 1)

    targetfilename <- .normalizePath(srcfile$filename)

    fnsrc <- attr(attr(f, "srcref") %||% body(f), "srcfile")
    if (is.null(fnsrc)) return(NULL)
    if (missing(srcfile))
    	srcfile <- fnsrc

    isBrace <- function(expr) is.symbol(expr) && expr == quote(`{`)

    lineNumInExpr <- function(expr, haveSrcrefs = FALSE) {
	if (typeof(expr) == "language") {
	    srcrefs <- attr(expr, "srcref")
	    for (i in seq_along(expr)) {
		srcref <- srcrefs[[i]]
		# Check for non-matching range
		if (!is.null(srcref) && (srcref[1] > line || line > srcref[3]))
                    next
		# We're in range.  See if there's a finer division
		finer <- lineNumInExpr(expr[[i]], haveSrcrefs || !is.null(srcrefs))
		if (!is.null(finer)) {
		    return(c(i, finer))
		}
		# Do we have a srcref?  It must point to this expression.
		# But do avoid matching the opening brace in a block:  match the whole block
		# instead.
		if (!(is.null(srcref) || (haveSrcrefs && isBrace(expr[[i]]))))
		    return(i)
	    }
	}
        ## in all other cases:
	NULL
    }

    perfectMatch <- identical(.normalizePath(fnsrc$filename, fnsrc$wd), targetfilename)
    if (perfectMatch ||
        (nameonly && !is.null(fnsrc$filename) &&
         basename(fnsrc$filename) == basename(targetfilename)))
    {
	timediff <- if(!is.na(srcfile$timestamp) && !is.null(fnsrc$timestamp) &&
                       fnsrc$timestamp != srcfile$timestamp)
                        fnsrc$timestamp - srcfile$timestamp else 0

	if(!is.null(at <- lineNumInExpr(body(f))))
            list(at = at,
                 filename = .normalizePath(fnsrc$filename, fnsrc$wd),
                 line = line, timediff = timediff)
    } # else NULL
}

findLineNum <- function(srcfile, line, nameonly=TRUE, envir=parent.frame(), lastenv)
{
    count <- 0L
    result <- list()

    if (!inherits(srcfile, "srcfile")) {
    	if (missing(line)) {
    	    line <- as.numeric(sub(".*#", "", srcfile))
    	    if (is.na(line)) stop("Line number missing")
    	    srcfile <- sub("#[^#]*", "", srcfile)
    	}

    	srcfile <- srcfile(srcfile)
    }

    if (missing(lastenv))
    	lastenv <- if(missing(envir)) globalenv() else emptyenv()
    if (!is.environment(envir))
    	envir <- environment(envir)

    fns <- character()
    envirs <- list()
    e <- envir
    repeat {
    	fns <- c(fns, lsf.str(envir=e, all.names=TRUE))
    	oldlen <- length(envirs)
    	length(envirs) <- length(fns)
    	if (length(envirs) > oldlen)
    	    for (i in seq.int(oldlen+1L, length(envirs))) envirs[[i]] <- e
    	if (identical(e, lastenv) || identical(e, emptyenv())) break
    	e <- parent.env(e)
    }

    for (i in seq_along(fns)) {
	functionName <- fns[i]
	fn <- get(functionName, envir=envirs[[i]])
	loc <- fnLineNum(fn, srcfile=srcfile, line=line, nameonly=nameonly)
    	if (!is.null(loc)) {
    	    count <- count + 1L
    	    result[[count]] <- c(list(name=functionName, env=envirs[[i]]), loc)
    	}
    	gen <- tryCatch(methods::isGeneric(functionName, envirs[[i]], fdef=fn),
                        error = identity)
    	if (isTRUE(gen)) {
    	    e1 <- environment(fn)$.AllMTable
            for (sig in names(e1)) {
                fn <- e1[[sig]]
                if (typeof(fn) != "closure")
                    next
                loc <- fnLineNum(fn, srcfile=srcfile, line=line,
                                 nameonly=nameonly)
                if (is.null(loc)
                    && length(bf <- body(fn)) >= 2L
                    && length(bf[[2L]]) > 2L
                    && typeof(bf.i <- bf[[iloc <- c(2L,3L)]]) == "closure") {
                    ## desperate try:  look for
                    ##   .local <- original defn
                    loc <- fnLineNum(bf.i, srcfile=srcfile, line=line, nameonly=nameonly)
                                        # FIXME:  can trace() set a breakpoint
                                        #	  within a function like this?
                    if (!is.null(loc)) loc$at <- iloc
                }
                if (!is.null(loc)) {
                    count <- count + 1L
                    result[[count]] <- c(list(name=functionName, env=envirs[[i]],
                                              signature=strsplit(sig, "#")[[1L]]),
                                         loc)
                }
            }
	}
    }
    structure(result, class="findLineNumResult")
}

print.findLineNumResult <- function(x, steps=TRUE, ...) {
    if (!length(x)) cat("No source refs found.\n")
    filename <- NULL
    line <- 0
    for (xi in x) {
	if (!identical(filename, xi$filename) ||
	    !identical(line, xi$line)) {
	    filename <- xi$filename
	    line <- xi$line
	    cat(filename, "#", line, ":\n", sep = "")
	}
        cat(" ", xi$name, if (steps) paste(" step ", paste(xi$at, collapse=",")) else "", sep = "")
        if (!is.null(xi$signature))
            cat(" signature ", paste(xi$signature, collapse=","), sep = "")
        cat(" in ", format(xi$env), "\n", sep = "")
    }
    invisible(x)
}


setBreakpoint <- function(srcfile, line, nameonly=TRUE, envir=parent.frame(), lastenv,
                          verbose = TRUE, tracer, print=FALSE, clear=FALSE,
                          ...)
{
    if (missing(lastenv))
    	lastenv <- if(missing(envir)) globalenv() else emptyenv()
    locations <- findLineNum(srcfile, line, nameonly, envir, lastenv)
    if (verbose) print(locations, steps=!clear)
    breakpoint <- missing(tracer)
    while (length(locations)) {
    	what <- locations[[1]]$name
    	where <- locations[[1]]$env
    	at <- list(locations[[1]]$at)
    	signature <- locations[[1]]$signature
    	if (breakpoint) {
	    loc1 <- locations[[1]]
    	    filename <- basename(loc1$filename)
    	    linenum <- loc1$line
	    tracer <- bquote({cat(paste0(.(filename), "#", .(linenum), "\n"))
    	                      browser(skipCalls=4L)})
    	}
    	locations[[1]] <- NULL
        i <- 1L
    	while (i <= length(locations)) {
    	    if (what == locations[[i]]$name &&
    	        identical(where, locations[[i]]$env) &&
    	        identical(signature, locations[[i]]$signature))	 {
    	    	at <- c(at, list(locations[[i]]))
    	    	locations[[i]] <- NULL
    	    } else
    	    	i <- i+1L
    	}
    	if (clear) {
    	    if (is.null(signature))
  		untrace(what,                      where=where)
    	    else
    	    	untrace(what, signature=signature, where=where)
    	} else if (is.null(signature))
    	    trace(what, tracer, at=at, print=print,                      where=where, ...)
    	else
    	    trace(what, tracer, at=at, print=print, signature=signature, where=where, ...)
    }
}
