#  File src/library/utils/R/demo.R
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

demo <-
function(topic, package = NULL, lib.loc = NULL,
	 character.only = FALSE, verbose = getOption("verbose"))
{
    paths <- .find.package(package, lib.loc, verbose = verbose)

    ## Find the directories with a 'demo' subdirectory.
    paths <- paths[file_test("-d", file.path(paths, "demo"))]
    ## Earlier versions remembered given packages with no 'demo'
    ## subdirectory, and warned about them.

    if(missing(topic)) {
	## List all possible demos.

	## Build the demo db.
	db <- matrix(character(0), nrow = 0, ncol = 4)
	for(path in paths) {
	    entries <- NULL
	    ## Check for new-style 'Meta/demo.rds', then for '00Index'.
	    if(file_test("-f", INDEX <- file.path(path, "Meta", "demo.rds"))) {
		entries <- .readRDS(INDEX)
	    }
	    if(NROW(entries) > 0) {
		db <- rbind(db,
			    cbind(basename(path), dirname(path),
				  entries))
	    }
	}
	colnames(db) <- c("Package", "LibPath", "Item", "Title")

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
	files <- basename(tools::list_files_with_type(p, "demo"))
	## Files with base names sans extension matching topic
	files <- files[topic == tools::file_path_sans_ext(files)]
	if(length(files) > 0)
	    available <- c(available, file.path(p, files))
    }
    if(length(available) == 0)
	stop(gettextf("No demo found for topic '%s'", topic), domain = NA)
    if(length(available) > 1) {
	available <- available[1]
	warning(gettextf("Demo for topic '%s' found more than once,\nusing the one found in '%s'",
                topic, dirname(available[1])), domain = NA)
    }
    cat("\n\n",
	"\tdemo(", topic, ")\n",
	"\t---- ", rep.int("~", nchar(topic, type="w")), "\n",
	sep="")
    if(interactive()) {
	cat("\nType  <Return>	 to start : ")
	readline()
    }
    source(available, echo = TRUE, max.deparse.length = 250)
}
