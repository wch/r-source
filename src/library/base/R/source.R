source <-
function(file, local = FALSE, echo = verbose, print.eval = echo,
         verbose = getOption("verbose"),
         prompt.echo = getOption("prompt"),
         max.deparse.length = 150, chdir = FALSE)
{
##-     if(!(is.character(file) && file.exists(file)))
##- 	stop(paste('"',file,'" is not an existing file', sep=""))
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
	    warning("verbose is TRUE, echo not; ... coercing `echo <- TRUE'")
	    echo <- TRUE
	}
    }
    if (verbose) {
	cat("`envir' chosen:")
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
    #-- ass1 :	the  `<-' symbol/name
    ass1 <- expression(y <- x)[[1]][[1]]
    if (echo) {
	## Reg.exps for string delimiter/ NO-string-del / odd-number-of-str.del
	## needed, when truncating below
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
	    cat(" .. after `", deparse(ei), "'\n", sep = "")
    }
    invisible(yy)
}

sys.source <-
function(file, envir = NULL, chdir = FALSE,
         keep.source = getOption("keep.source.pkgs"))
{
    if(!(is.character(file) && file.exists(file)))
	stop(paste("`", file, "' is not an existing file", sep = ""))
    oop <- options(keep.source = as.logical(keep.source))
    on.exit(options(oop))
    exprs <- parse(n = -1, file = file)
    if (length(exprs) == 0)
	return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
	owd <- getwd()
	on.exit(setwd(owd), add = TRUE)
	setwd(path)
    }
    for (i in exprs) {
	yy <- eval(i, envir)
    }
    invisible()
}

demo <-
function(topic, device = getOption("device"),
         package = .packages(), lib.loc = NULL,
         character.only = FALSE, verbose = getOption("verbose"))
{
    sQuote <- function(s) paste("`", s, "'", sep = "")

    paths <- .find.package(package, lib.loc, verbose = verbose)

    if(missing(topic)) {
        ## List all available demos.
        ## This code could be made more similar to data().

        ## Build the demo db.
        db <- matrix(character(0), nr = 0, nc = 4)
        for(path in paths) {
            INDEX <- file.path(path, "demo", "00Index")
            if(file.exists(INDEX)) {
                entries <- read.00Index(INDEX)
                if(NROW(entries) > 0) {
                    db <- rbind(db,
                                cbind(basename(path),
                                      dirname(path),
                                      entries))
                }
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        footer <- if(missing(package))
            paste("Use `demo(package = ",
                  ".packages(all.available = TRUE))'\n",
                  "to list the demos in all ",
                  "*available* packages.", sep = "")
        else
            NULL
        y <- list(type = "demo",
                  header = NULL, results = db, footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    if(!character.only)
        topic <- as.character(substitute(topic))
    available <- character(0)
    for(p in paths) {
        if(file.exists(p <- file.path(p, "demo"))) {
            files <- list.files(p)
            ## Files with extension `R' or `r'
            files <- files[sub(".*\\.", "", files) %in% c("R", "r")]
            ## Files with base names matching topic
            files <- files[grep(topic, files)]
            if(length(files) > 0)
                available <- c(available, file.path(p, files))
        }
    }
    if(length(available) == 0)
        stop(paste("No demo found for topic", sQuote(topic)))
    if(length(available) > 1) {
        available <- available[1]
        warning("Demo for topic",
                sQuote(topic),
                "found more than once,\n",
                "using the one found in",
                sQuote(dirname(available[1])))
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
function(topic, package = .packages(), lib.loc = NULL,
         echo = TRUE, verbose = getOption("verbose"),
         prompt.echo = paste(abbreviate(topic, 6), "> ", sep = ""))
{
    sQuote <- function(s) paste("`", s, "'", sep = "")

    topic <- substitute(topic)
    if(!is.character(topic))
	topic <- deparse(topic)[1]
    INDICES <- .find.package(package, lib.loc, verbose = verbose)
    file <- index.search(topic, INDICES, "AnIndex", "R-ex")
    if(file == "") {
	warning(paste("No help file found for", sQuote(topic)))
	return(invisible())
    }
    comp <- strsplit(file, .Platform$file.sep)[[1]]
    pkg <- comp[length(comp) - 2]
    if(length(file) > 1)
	warning(paste("More than one help file found: using package",
                      sQuote(pkg)))
    lib <- sub(file.path("", pkg, "R-ex", ".*\\.R"), "", file[1])
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
    source(zfile, echo = echo, prompt.echo = prompt.echo, verbose =
	   verbose, max.deparse.length = 250)
}
