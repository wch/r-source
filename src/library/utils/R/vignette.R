#  File src/library/utils/R/vignette.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

vignette <-
    function(topic, package = NULL, lib.loc = NULL, all = TRUE)
{
    vinfo <- tools::getVignetteInfo(package, lib.loc, all)

    if(!missing(topic)) {
        stopic <- substitute(topic)
        if(is.call(stopic) &&
           (deparse1(stopic[[1L]]) == "::")) {
            package <- as.character(stopic[[2L]])
            topic <- as.character(stopic[[3L]])
        } else
            topic <- topic[1L] # Just making sure ...
        vinfo <- vinfo[vinfo[, "Topic"] == topic, , drop = FALSE]
        if(length(vinfo)) {
            pos <- which(file_test("-f",
                                   file.path(vinfo[, "Dir"], "doc",
                                             vinfo[, "PDF"])))
            if(!length(pos)) {
                z <- as.list(vinfo[1L, ])
                z$PDF <- ""
            } else {
                if(length(pos) > 1L) {
                    ## <FIXME>
                    ## Should really offer a menu to select from.
                    pos <- pos[1L]
                    warning(gettextf("vignette %s found more than once,\nusing the one found in %s",
                                     sQuote(topic),
                                     sQuote(file.path(vinfo[pos, "Dir"],
                                                      "doc"))),
                            call. = FALSE, domain = NA)
                    ## </FIXME>
                }
                z <- as.list(vinfo[pos, ])
            }
            if(!file_test("-f", file.path(z$Dir, "doc", z$R)))
                z$R <- ""
            class(z) <- "vignette"
            return(z)
        }
        else
            warning(gettextf("vignette %s not found", sQuote(topic)),
                    call. = FALSE, domain = NA)

    } else { ## missing(topic)) -- List all possible vignettes.
        title <- if(nrow(vinfo)) {
            paste(vinfo[, "Title"],
                  paste0(rep.int("(source", nrow(vinfo)),
                        ifelse(nzchar(vinfo[, "PDF"]),
                               paste0(", ", tools::file_ext(vinfo[, "PDF"])),
                               ""),
                         ")"))
        }
        else
            character()
        ## ... and rewrite into the form used by packageIQR.
        db <- cbind(Package = basename(vinfo[, "Dir"]),
                    LibPath = dirname(vinfo[, "Dir"]),
                    Item = vinfo[, "Topic"],
                    Title = title)
	footer <- if (all) NULL else
		  paste0("Use ", sQuote("vignette(all = TRUE)"), "\n",
                         "to list the vignettes in all *available* packages.")
        ## return
        structure(class = "packageIQR",
                  list(type = "vignette", title = "Vignettes", header = NULL,
                       results = db, footer = footer))
    }
}

print.vignette <-
function(x, ...)
{
    if(nzchar(out <- x$PDF)) {
        ext <- tools::file_ext(out)
	port <- if (tolower(ext) == "html") tools::startDynamicHelp(NA) else 0L
	out <- if(port > 0L)
	    sprintf("http://127.0.0.1:%d/library/%s/doc/%s",
                    port, basename(x$Dir), out)
               else
                   file.path(x$Dir, "doc", out)
        if(tolower(ext) == "pdf") {
            pdfviewer <- getOption("pdfviewer")
            if(identical(pdfviewer, "false")) {
            }
            else if(.Platform$OS.type == "windows" &&
                    identical(pdfviewer,
                              file.path(R.home("bin"), "open.exe")))
            	shell.exec(out)
            else system2(pdfviewer, shQuote(out), wait = FALSE)
        } else
            browseURL(out)
    } else {
        warning(gettextf("vignette %s has no PDF/HTML",
                         sQuote(x$Topic)),
                call. = FALSE, domain = NA)
    }
    invisible(x)
}

## Not exported yet
getRcode <- function(x, ...) UseMethod("getRcode")

getRcode.vignette <- function(x, strict=TRUE, ...) {
    if(nzchar(p <- x$R)) {
        file.path(x$Dir, "doc", p)
    } else {
        ## Could try to extract the R code from the source via tangle,
        ## using
        ##   tools::buildVignette(tangle = TRUE, weave = FALSE)
        ## but why should this not have been done at install time?
        (if(strict) stop else warning)(
            gettextf("vignette %s has no R code", sQuote(x$Topic)),
            call. = FALSE, domain = NA)
    }
}

edit.vignette <- function(name, ...)
{
    if(is.character(src <- getRcode(name, strict=FALSE))) {
        f <- tempfile(name$Topic, fileext = ".R")
        file.copy(src, f)
        file.edit(file = f, ...)
    } # getRcode() did warn already
}
