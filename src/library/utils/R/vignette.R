#  File src/library/utils/R/vignette.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

vignette <-
    function(topic, package = NULL, lib.loc = NULL, all = TRUE)
{
    if (is.null(package)) {
        package <- .packages(all.available = all, lib.loc)
        ## allow for misnamed dirs
        paths <- find.package(package, lib.loc, quiet = TRUE)
    } else paths <- find.package(package, lib.loc)

    ## Find the directories with a 'doc' subdirectory *possibly*
    ## containing vignettes.

    paths <- paths[file_test("-d", file.path(paths, "doc"))]

    vignettes <-
        lapply(paths,
               function(dir) {
                   tools::list_files_with_type(file.path(dir, "doc"),
                                               "vignette")
               })

    if(!missing(topic)) {
        topic <- topic[1L]               # Just making sure ...
        vignettes <- as.character(unlist(vignettes))
        vidx <- (tools::file_path_sans_ext(basename(vignettes)) == topic)
        if(any(vidx)) {

            pdf <- sub("\\.[[:alpha:]]+$", ".pdf", vignettes)
            pidx <- file_test("-f", pdf)
            ok <- vidx & pidx

            if(any(ok)){
                idx <- min(which(ok))
                if(sum(ok)>1){
                    ## <FIXME>
                    ## Should really offer a menu to select from.
                    warning(gettextf("vignette %s found more than once,\nusing the one found in %s",
                                     sQuote(topic), sQuote(dirname(pdf[idx]))),
                            call. = FALSE, domain = NA)
                    ## </FIXME>
                }

                z <- list(file=vignettes[idx], pdf=pdf[idx])
            }
            else{
                z <- list(file=vignettes[vidx][1L], pdf=character(0L))
            }
            z$topic <- topic
            class(z) <- "vignette"
            return(z)
        }
        else
            warning(gettextf("vignette %s not found", sQuote(topic)),
                    call. = FALSE, domain = NA)
    }

    if(missing(topic)) {
        ## List all possible vignettes.

        vDB <- matrix(character(0L), nrow = 0L, ncol = 4L)
        colnames(vDB) <- c("Dir", "File", "Title", "PDF")

        for(db in vignettes[sapply(vignettes, length) > 0L]) {
            dir <- dirname(dirname(db[1L]))
            entries <- NULL
            ## Check for new-style 'Meta/vignette.rds' ...
            if(file.exists(INDEX <-
                           file.path(dir, "Meta", "vignette.rds")))
                entries <- readRDS(INDEX)
            if(NROW(entries) > 0)
                vDB <- rbind(vDB,
                             cbind(dir,
                                   entries$File,
                                   entries$Title,
                                   entries$PDF))
        }

        ## Now compute info on available PDFs ...
        title <- if(NROW(vDB)) {
            paste(vDB[, "Title"],
                  paste(rep.int("(source", NROW(vDB)),
                        ifelse(vDB[, "PDF"] != "", ", pdf", ""),
                        ")",
                        sep = ""))
        }
        else
            character()
        ## ... and rewrite into the form used by packageIQR.
        db <- cbind(Package = basename(vDB[, "Dir"]),
                    LibPath = dirname(vDB[, "Dir"]),
                    Item = tools::file_path_sans_ext(basename(vDB[, "File"])),
                    Title = title)
	footer <- if (all) NULL else
		  paste("Use ",
	                sQuote("vignette(all = TRUE)"),
	                "\n",
	                "to list the vignettes in all *available* packages.",
                  	sep = "")

        y <- list(type = "vignette", title = "Vignettes", header = NULL,
                  results = db, footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
}

print.vignette <- function(x, ...){

    if(length(x$pdf)){
        ## <FIXME>
        ## Should really abstract this into a BioC style
        ## openPDF() along the lines of browseURL() ...
        pdfviewer <- getOption("pdfviewer")
        if(identical(pdfviewer, "false")) {
        } else if(.Platform$OS.type == "windows" &&
                  identical(pdfviewer, file.path(R.home("bin"), "open.exe")))
            shell.exec(x$pdf)
        else system2(pdfviewer, shQuote(x$pdf), wait = FALSE)
        ## </FIXME>
    } else {
        warning(gettextf("vignette %s has no PDF", sQuote(x$topic)),
                call. = FALSE, domain = NA)
    }
    invisible(x)
}

edit.vignette <- function(name, ...)
{

    f <- tempfile(name$topic, fileext=".R")
    Stangle(name$file, output=f, quiet=TRUE)
    file.edit(file=f, ...)
}
