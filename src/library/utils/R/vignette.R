#  File src/library/utils/R/vignette.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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
    vinfo <- tools:::getVignetteInfo(package, lib.loc, all)
    
    if(!missing(topic)) {
        topic <- topic[1L]               # Just making sure ...
        vinfo <- vinfo[tools::file_path_sans_ext(vinfo[, "File"]) == topic,,drop=FALSE]
        if(length(vinfo)) {

            pdf <- vinfo[, "PDF"]
            pidx <- file_test("-f", file.path(vinfo[, "Dir"], "doc", vinfo[, "PDF"]))

            if(any(pidx)){
                idx <- min(which(pidx))
                if(sum(pidx)>1){
                    ## <FIXME>
                    ## Should really offer a menu to select from.
                    warning(gettextf("vignette %s found more than once,\nusing the one found in %s",
                                     sQuote(topic), sQuote(dirname(pdf[idx]))),
                            call. = FALSE, domain = NA)
                    ## </FIXME>
                }
		vinfo <- vinfo[idx,,drop=FALSE]
		Dir <- vinfo[, "Dir"]
		File <- vinfo[, "File"]
		PDF <- vinfo[, "PDF"]
                z <- list(file=file.path(Dir, "doc", File),
                          pdf=file.path(Dir, "doc", PDF))
            }
            else{
		Dir <- vinfo[1, "Dir"]
		File <- vinfo[1, "File"]
                z <- list(file=file.path(Dir, "doc", File),
                          pdf=character(0L))
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

        title <- if(nrow(vinfo)) {
            paste(vinfo[, "Title"],
                  paste0(rep.int("(source", nrow(vinfo)),
                        ifelse(vinfo[, "PDF"] != "", paste0(", ", tools::file_ext(vinfo[, "PDF"])), ""),
                        ")"))
        }
        else
            character()
        ## ... and rewrite into the form used by packageIQR.
        db <- cbind(Package = basename(vinfo[, "Dir"]),
                    LibPath = dirname(vinfo[, "Dir"]),
                    Item = tools::file_path_sans_ext(basename(vinfo[, "File"])),
                    Title = title)
	footer <- if (all) NULL else
		  paste0("Use ",
                         sQuote("vignette(all = TRUE)"),
                         "\n",
                         "to list the vignettes in all *available* packages.")

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
        ext <- tools::file_ext(x$pdf)
        if (tolower(ext) == "pdf") {
            pdfviewer <- getOption("pdfviewer")
            if(identical(pdfviewer, "false")) {
            } else if(.Platform$OS.type == "windows" &&
                      identical(pdfviewer, file.path(R.home("bin"), "open.exe")))
            	shell.exec(x$pdf)
            else system2(pdfviewer, shQuote(x$pdf), wait = FALSE)
        ## </FIXME>         
        } else 
             browseURL(x$pdf)

    } else {
        warning(gettextf("vignette %s has no PDF/HTML", sQuote(x$topic)),
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
