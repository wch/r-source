#  File src/library/utils/R/demo.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

demo <-
function(topic, package = NULL, lib.loc = NULL,
	 character.only = FALSE, verbose = getOption("verbose"),
	 echo = TRUE, ask = getOption("demo.ask"),
         encoding = getOption("encoding"))
{
    paths <- find.package(package, lib.loc, verbose = verbose)

    ## Find the directories with a 'demo' subdirectory.
    paths <- paths[dir.exists(file.path(paths, "demo"))]
    ## Earlier versions remembered given packages with no 'demo'
    ## subdirectory, and warned about them.

    if(missing(topic)) {
	## List all possible demos.

	## Build the demo db.
	db <- matrix(character(), nrow = 0L, ncol = 4L)
	for(path in paths) {
	    entries <- NULL
	    ## Check for new-style 'Meta/demo.rds', then for '00Index'.
	    if(file_test("-f", INDEX <- file.path(path, "Meta", "demo.rds"))) {
		entries <- readRDS(INDEX)
	    }
	    if(NROW(entries)) {
		db <- rbind(db,
			    cbind(basename(path), dirname(path),
				  entries))
	    }
	}
	colnames(db) <- c("Package", "LibPath", "Item", "Title")

	footer <- if(missing(package))
	    paste0("Use ",
                   sQuote(paste("demo(package =",
                                ".packages(all.available = TRUE))")),
                   "\n",
                   "to list the demos in all *available* packages.")
	else
	    NULL
	y <- list(title = "Demos", header = NULL, results = db,
		  footer = footer)
	class(y) <- "packageIQR"
	return(y)
    }

    if(!character.only) {
    	topic <- substitute(topic)
    	if (is.call(topic) && (topic[[1L]] == "::" || topic[[1L]] == ":::")) {
	    package <- as.character(topic[[2L]])
	    topic <- as.character(topic[[3L]])
	} else
	    topic <- as.character(topic)
    }

    available <- character()
    paths <- file.path(paths, "demo")
    for(p in paths) {
	files <- basename(tools::list_files_with_type(p, "demo"))
	## Files with base names sans extension matching topic
	files <- files[topic == tools::file_path_sans_ext(files)]
	if(length(files))
	    available <- c(available, file.path(p, files))
    }
    if(length(available) == 0L)
	stop(gettextf("No demo found for topic %s", sQuote(topic)), domain = NA)
    if(length(available) > 1L) {
	available <- available[1L]
	warning(gettextf("Demo for topic %s' found more than once,\nusing the one found in %s",
                sQuote(topic), sQuote(dirname(available[1L]))), domain = NA)
    }

    ## now figure out if the package has an encoding
    pkgpath <- dirname(dirname(available))
    if (file.exists(file <- file.path(pkgpath, "Meta", "package.rds"))) {
        desc <- readRDS(file)$DESCRIPTION
        if (length(desc) == 1L) {
            enc <- as.list(desc)[["Encoding"]]
            !if(!is.null(enc)) encoding <- enc
        }
    }

    if(ask == "default")
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)

    if(.Device != "null device") {
	oldask <- grDevices::devAskNewPage(ask = ask)
        on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
    }

    op <- options(device.ask.default = ask)
    on.exit(options(op), add = TRUE)

    if (echo) {
	cat("\n\n",
	    "\tdemo(", topic, ")\n",
	    "\t---- ", rep.int("~", nchar(topic, type = "w")), "\n",
	    sep = "")
	if(ask && interactive())
	    readline("\nType  <Return>	 to start : ")
    }
    source(available, echo = echo, max.deparse.length = Inf,
           keep.source = TRUE, encoding = encoding)
}
