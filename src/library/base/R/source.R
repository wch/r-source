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

    envir <- if (local)
	parent.frame()
    else .GlobalEnv
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
    #-- ass1 :	the  '<-' symbol/name
#    ass1 <- expression(y <- x)[[1]][[1]]
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
	    dep <- substr(paste(deparse(ei), collapse = "\n"),
			  12, 1e+06)
	    # -1: drop ")"
	    nd <- nchar(dep) - 1
	    do.trunc <- nd > max.deparse.length
	    dep <- substr(dep, 1, if (do.trunc)
			  max.deparse.length
			  else nd)
	    cat("\n", prompt.echo, dep, if (do.trunc)
		paste(if (length(grep(sd, dep)) && length(grep(oddsd,
							       dep)))
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
	    cat(" .. after ", sQuote(deparse(ei)), "\n", sep = "")
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

demo <-
function(topic, device = getOption("device"),
         package = .packages(), lib.loc = NULL,
         character.only = FALSE, verbose = getOption("verbose"))
{
    paths <- .find.package(package, lib.loc, verbose = verbose)

    ## Find the directories with a 'demo' subdirectory.
    paths <- paths[tools::fileTest("-d", file.path(paths, "demo"))]
    ## Earlier versions remembered given packages with no 'demo'
    ## subdirectory, and warned about them.

    if(missing(topic)) {
        ## List all possible demos.

        ## Build the demo db.
        db <- matrix(character(0), nr = 0, nc = 4)
        noindex <- character(0)
        for(path in paths) {
            entries <- NULL
            ## Check for new-style 'Meta/demo.rds', then for '00Index'.
            if(tools::fileTest("-f",
                               INDEX <-
                               file.path(path, "Meta", "demo.rds"))) {
                entries <- .readRDS(INDEX)
            }
            else if(tools::fileTest("-f",
                                    INDEX <-
                                    file.path(path, "demo", "00Index")))
                entries <- read.00Index(INDEX)
            else {
                ## No index: check whether subdir 'demo' contains demos.
                demoDir <- file.path(path, "demo")
                entries <- tools::listFilesWithType(demoDir, "demo")
                if(length(entries) > 0) {
                    entries <-
                        unique(tools::filePathSansExt(basename(entries)))
                    entries <- cbind(entries, "")
                }
                else
                    noindex <- c(noindex, basename(path))
            }
            if(NROW(entries) > 0) {
                db <- rbind(db,
                            cbind(basename(path), dirname(path),
                                  entries))
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        if(length(noindex) > 0) {
            if(!missing(package) && (length(package) > 0)) {
                ## Warn about given packages which do not have a demo
                ## index.
                packagesWithNoIndex <- package[package %in% noindex]
                if(length(packagesWithNoIndex) > 0)
                    warning(paste("packages with demos",
                                  "but no index:",
                                  paste(sQuote(packagesWithNoIndex),
                                        collapse = ",")))
            }
        }

        footer <- if(missing(package))
            paste("Use ",
                  sQuote(paste("demo(package =",
                               ".packages(all.available = TRUE))")),
                  "\n",
                  "to list the demos in all *available* packages.",
                  sep = "")
        else
            NULL
        y <- list(title = "Demos", header = NULL, results = db,
                  footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    if(!character.only)
        topic <- as.character(substitute(topic))
    available <- character(0)
    paths <- file.path(paths, "demo")
    for(p in paths) {
        files <- basename(tools::listFilesWithType(p, "demo"))
        ## Files with base names sans extension matching topic
        files <- files[topic == tools::filePathSansExt(files)]
        if(length(files) > 0)
            available <- c(available, file.path(p, files))
    }
    if(length(available) == 0)
        stop(paste("No demo found for topic", sQuote(topic)))
    if(length(available) > 1) {
        available <- available[1]
        warning(paste("Demo for topic ",
                      sQuote(topic),
                      " found more than once,\n",
                      "using the one found in ",
                      sQuote(dirname(available[1])),
                      sep = ""))
    }
    cat("\n\n",
        "\tdemo(", topic, ")\n",
        "\t---- ", rep("~", nchar(topic)), "\n",
        sep="")
    if(interactive()) {
        cat("\nType  <Return>	 to start : ")
        readline()
    }
    source(available, echo = TRUE, max.deparse.length = 250)
}

example <-
function(topic, package = .packages(), lib.loc = NULL, local = FALSE,
         echo = TRUE, verbose = getOption("verbose"),
         prompt.echo = paste(abbreviate(topic, 6), "> ", sep = ""))
{
    topic <- substitute(topic)
    if(!is.character(topic))
	topic <- deparse(topic)[1]
    INDICES <- .find.package(package, lib.loc, verbose = verbose)
    file <- index.search(topic, INDICES, "AnIndex", "R-ex")
    if(file == "") {
	warning(paste("No help file found for", sQuote(topic)))
	return(invisible())
    }
    packagePath <- dirname(dirname(file))
    if(length(file) > 1) {
        packagePath <- packagePath[1]
	warning(paste("More than one help file found: using package",
                      sQuote(basename(packagePath))))
        file <- file[1]
    }
    pkg <- basename(packagePath)
    lib <- dirname(packagePath)
    ## experimental code
    zfile <- zip.file.extract(file, "Rex.zip")
    if(zfile != file) on.exit(unlink(zfile))
    ## end of experimental code
    if(!file.exists(zfile)) {
	warning(paste(sQuote(topic),
                      "has a help file but no examples file"))
	return(invisible())
    }
    if(pkg != "base")
	library(pkg, lib = lib, character.only = TRUE)
    source(zfile, local, echo = echo, prompt.echo = prompt.echo,
           verbose = verbose, max.deparse.length = 250)
}
