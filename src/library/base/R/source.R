source <-
function(file, local = FALSE, echo = verbose, print.eval = echo,
	 verbose = getOption("verbose"),
	 prompt.echo = getOption("prompt"),
	 max.deparse.length = 150, chdir = FALSE)
{
    eval.with.vis <-
	function (expr, envir = parent.frame(),
		  enclos = if (is.list(envir) || is.pairlist(envir))
		  parent.frame())
	.Internal(eval.with.vis(expr, envir, enclos))

    envir <- if (local) parent.frame() else .GlobalEnv
    if (!missing(echo)) {
	if (!is.logical(echo))
	    stop("echo must be logical")
	if (!echo && verbose) {
	    warning(paste("verbose is TRUE, echo not; ... coercing",
			  sQuote("echo <- TRUE")))
	    echo <- TRUE
	}
    }
    if (verbose) {
	cat(sQuote("envir"), "chosen:")
	print(envir)
    }
    Ne <- length(exprs <- parse(n = -1, file = file))
    if (verbose)
	cat("--> parsed", Ne, "expressions; now eval(.)ing them:\n")
    if (Ne == 0)
	return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
	owd <- getwd()
	on.exit(setwd(owd))
	setwd(path)
    }

    if (echo) {
	## Reg.exps for string delimiter/ NO-string-del /
	## odd-number-of-str.del needed, when truncating below
	sd <- "\""
	nos <- "[^\"]*"
	oddsd <- paste("^", nos, sd, "(", nos, sd, nos, sd, ")*",
		       nos, "$", sep = "")
    }
    for (i in 1:Ne) {
	if (verbose)
	    cat("\n>>>> eval(expression_nr.", i, ")\n\t	 =================\n")
	ei <- exprs[i]
	if (echo) {
	    # drop "expression("
	    dep <- substr(paste(deparse(ei, control = c("showAttributes","useSource")),
	    		  collapse = "\n"), 12, 1e+06)
	    # -1: drop ")"
            ## <FIXME> really want widths here, not #chars.
	    nd <- nchar(dep, type="c") - 1
	    do.trunc <- nd > max.deparse.length
	    dep <- strtrim(dep, if (do.trunc) max.deparse.length else nd)
	    cat("\n", prompt.echo, dep, if (do.trunc)
		paste(if (length(grep(sd, dep)) && length(grep(oddsd, dep)))
		      " ...\" ..."
		      else " ....", "[TRUNCATED] "), "\n", sep = "")
	}
	yy <- eval.with.vis(ei, envir)
	i.symbol <- mode(ei[[1]]) == "name"
	if (!i.symbol) {
	    ## ei[[1]] : the function "<-" or other
	    curr.fun <- ei[[1]][[1]]
	    if (verbose) {
		cat("curr.fun:")
		str(curr.fun)
	    }
	}
	if (verbose >= 2) {
	    cat(".... mode(ei[[1]])=", mode(ei[[1]]), "; paste(curr.fun)=")
	    str(paste(curr.fun))
	}
	if (print.eval && yy$visible)
	    print(yy$value)
	if (verbose)
	    cat(" .. after ", sQuote(deparse(ei,
	    	control = c("showAttributes","useSource"))), "\n", sep = "")
    }
    invisible(yy)
}

sys.source <-
function(file, envir = NULL, chdir = FALSE,
	 keep.source = getOption("keep.source.pkgs"))
{
    if(!(is.character(file) && file.exists(file)))
	stop(paste(sQuote(file), "is not an existing file"))
    oop <- options(keep.source = as.logical(keep.source),
		   topLevelEnvironment = as.environment(envir))
    on.exit(options(oop))
    exprs <- parse(n = -1, file = file)
    if (length(exprs) == 0)
	return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
	owd <- getwd()
	on.exit(setwd(owd), add = TRUE)
	setwd(path)
    }
    for (i in exprs) eval(i, envir)
    invisible()
}
