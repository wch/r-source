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
	"\t---- ", rep.int("~", nchar(topic)), "\n",
	sep="")
    if(interactive()) {
	cat("\nType  <Return>	 to start : ")
	readline()
    }
    source(available, echo = TRUE, max.deparse.length = 250)
}
