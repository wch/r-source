#  File src/library/utils/R/fineLineNum.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2009-2014 Duncan Murdoch and the R Core Team
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

    fnsrc <- attr(f, "srcref")
    if (!is.null(fnsrc))
    	fnsrc <- attr(fnsrc, "srcfile")
    else
    	fnsrc <- attr(body(f), "srcfile")
    if (is.null(fnsrc)) return(NULL)

    if (missing(srcfile)) {
    	srcfile <- fnsrc
    }

    isBrace <- function(expr)
        typeof(expr) == "symbol" && identical(as.character(expr), "{")

    lineNumInExpr <- function(expr, haveSrcrefs = FALSE) {
	if (typeof(expr) == "language") {
	    srcrefs <- attr(expr, "srcref")
	    for (i in seq_along(expr)) {
		srcref <- srcrefs[[i]]
		# Check for non-matching range
		if (!is.null(srcref) && (srcref[1] > line || line > srcref[3]))  next
		# We're in range.  See if there's a finer division
		finer <- lineNumInExpr(expr[[i]], haveSrcrefs || !is.null(srcrefs))
		if (!is.null(finer)) {
		    return(c(i, finer))
		}
		# Do we have a srcref?  It must point to this expression.
		# But do avoid matching the opening brace in a block:  match the whole block
		# instead.

		havebrace <- isBrace(expr[[i]])
		if (!is.null(srcref)
		    && (!haveSrcrefs || !havebrace)) {
		    return(i)
		}
	    }
	}
	return(NULL)
    }

    perfectMatch <- identical(.normalizePath(fnsrc$filename, fnsrc$wd), targetfilename)
    if (perfectMatch ||
        (nameonly && !is.null(fnsrc$filename) && basename(fnsrc$filename) == basename(targetfilename))) {
	if (!is.na(srcfile$timestamp) && !is.null(fnsrc$timestamp) && fnsrc$timestamp != srcfile$timestamp)
	    timediff <- fnsrc$timestamp - srcfile$timestamp
	else
	    timediff <- 0
	at <- lineNumInExpr(body(f))
	if (!is.null(at))
	  return(list(at=at, filename=.normalizePath(fnsrc$filename, fnsrc$wd), line=line,
	              timediff=timediff))
    }
    return(NULL)
}

findLineNum <- function(srcfile, line, nameonly=TRUE, envir=parent.frame(),
			lastenv) {
    count <- 0
    result <- list()

    if (!inherits(srcfile, "srcfile")) {
    	if (missing(line)) {
    	    line <- as.numeric(sub(".*#", "", srcfile))
    	    if (is.na(line)) stop("Line number missing")
    	    srcfile <- sub("#[^#]*", "", srcfile)
    	}

    	srcfile <- srcfile(srcfile)
    }

    if (missing(lastenv)) {
    	if (missing(envir)) lastenv <- globalenv()
    	else lastenv <- emptyenv()
    }

    if (!is.environment(envir))
    	envir <- environment(envir)

    fns <- character()
    envirs <- list()
    e <- envir
    repeat {
    	fns <- c(fns, lsf.str(envir=e, all=TRUE))
    	oldlen <- length(envirs)
    	length(envirs) <- length(fns)
    	if (length(envirs) > oldlen)
    	    for (i in seq.int(oldlen+1, length(envirs))) envirs[[i]] <- e
    	if (identical(e, lastenv) || identical(e, emptyenv())) break
    	e <- parent.env(e)
    }

    for (i in seq_along(fns)) {
	functionName <- fns[i]
	fn <- get(functionName, envir=envirs[[i]])
	loc <- fnLineNum(fn, srcfile=srcfile, line=line,
    	                  nameonly=nameonly)
    	if (!is.null(loc)) {
    	    count <- count + 1
    	    result[[count]] <- c(list(name=functionName, env=envirs[[i]]), loc)
    	}
    	gen <- tryCatch(methods::isGeneric(functionName, envirs[[i]], fdef=fn),
                        error = identity)
    	if (isTRUE(gen)) {
    	    e1 <- environment(fn)$.AllMTable
    	    if (!is.null(e1)) {
		sigs <- ls(e1)
		for (j in seq_along(sigs)) {
		    sig <- sigs[j]
		    fn <- get(sig, e1)
		    if (typeof(fn) != "closure") next

		    loc <- fnLineNum(fn, srcfile=srcfile, line=line,
				    nameonly=nameonly)
		    if (is.null(loc)
		        && length(body(fn)) > 1
		        && length(body(fn)[[2]]) > 2
		        && typeof(body(fn)[[c(2,3)]]) == "closure") {
				# desperate try:  look for
		    		# .local <- original defn
		    	fn2 <- body(fn)[[c(2,3)]]

		    	loc <- fnLineNum(fn2, srcfile=srcfile, line=line,
				    nameonly=nameonly)
		 	# FIXME:  can trace() set a breakpoint
		 	#	  within a function like this?
		        if (!is.null(loc)) loc$at <- c(2,3)
		    }
		    if (!is.null(loc)) {
			count <- count + 1
			result[[count]] <- c(list(name=functionName, env=envirs[[i]],
						signature=strsplit(sig, "#")[[1]]), loc)
		    }
		}
	    }
	}
    }
    return(structure(result, class="findLineNumResult"))
}

print.findLineNumResult <- function(x, steps=TRUE, ...) {
    if (!length(x)) cat("No source refs found.\n")
    filename <- NULL
    line <- 0
    for (i in seq_along(x)) {
    	if (!identical(filename, x[[i]]$filename) ||
    	    !identical(line, x[[i]]$line)) {
    	    filename <- x[[i]]$filename
    	    line <- x[[i]]$line
    	    cat(filename, "#", line, ":\n", sep = "")
    	}
        cat(" ", x[[i]]$name, if (steps) paste(" step ", paste(x[[i]]$at, collapse=",")) else "", sep = "")
        if (!is.null(x[[i]]$signature))
            cat(" signature ", paste(x[[i]]$signature, collapse=","), sep = "")
        cat(" in ", format(x[[i]]$env), "\n", sep = "")
    }
}


setBreakpoint <- function(srcfile, line, nameonly=TRUE, envir=parent.frame(), lastenv,
                          verbose = TRUE, tracer, print=FALSE, clear=FALSE,
                         ...) {

    if (missing(lastenv)) {
    	if (missing(envir)) lastenv <- globalenv()
    	else lastenv <- emptyenv()
    }
    locations <- findLineNum(srcfile, line, nameonly, envir, lastenv)
    if (verbose) print(locations, steps=!clear)
    breakpoint <- missing(tracer)
    while (length(locations)) {
    	what <- locations[[1]]$name
    	where <- locations[[1]]$env
    	at <- list(locations[[1]]$at)
    	signature <- locations[[1]]$signature
    	if (breakpoint) {
    	    filename <- basename(locations[[1]]$filename)
    	    linenum <- locations[[1]]$line
	    tracer <- bquote({cat(paste0(.(filename), "#", .(linenum), "\n"))
    	                      browser(skipCalls=4L)})
    	}
    	locations[[1]] <- NULL
        i <- 1
    	while (i <= length(locations)) {
    	    if (what == locations[[i]]$name &&
    	        identical(where, locations[[i]]$env) &&
    	        identical(signature, locations[[i]]$signature))	 {
    	    	at <- c(at, list(locations[[i]]))
    	    	locations[[i]] <- NULL
    	    } else
    	    	i <- i+1
    	}
    	if (clear) {
    	    if (is.null(signature))
  		untrace(what, where=where)
    	    else
    	    	untrace(what, signature=signature, where=where)
    	} else if (is.null(signature))
    	    trace(what, tracer, at=at, where=where, print=print, ...)
    	else
    	    trace(what, signature=signature, tracer, at=at, where=where, ...)
    }
}
