#  File src/library/utils/R/RShowDoc.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

RShowDoc <- function(what, type = c("pdf", "html", "txt"), package)
{
    paste. <- function(x, ext) paste(x, ext, sep=".")
    pdf_viewer <- function(path) {
        pdfviewer <- getOption("pdfviewer")
        if(identical(pdfviewer, "false")) {
        } else if(.Platform$OS.type == "windows" &&
                  identical(pdfviewer, file.path(R.home("bin"), "open.exe")))
            shell.exec(path)
        else system2(pdfviewer, shQuote(path), wait = FALSE)
    }

    html_viewer <- function(path) {
        ## we don't use browseURL under Windows as shell.exec does
        ## not want an encoded URL.
        browser <- getOption("browser")
        if(is.null(browser) && .Platform$OS.type == "windows")
            shell.exec(chartr("/", "\\", path))
        else browseURL(paste0("file://", URLencode(path)))
    }

    type <- match.arg(type)
    if(missing(what) || length(what) != 1L || !is.character(what)) {
        message("   RShowDoc() should be used with a character string argument specifying\n   a documentation file")
        return(invisible())
    }
    if(!missing(package)) {
        pkgpath <- find.package(package)
        if(type == "pdf") {
            path <- file.path(pkgpath, "doc", paste.(what, "pdf"))
            if(file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            path <- file.path(pkgpath, paste.(what, "pdf"))
            if(file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if(type == "html") {
            path <- file.path(pkgpath, "doc", paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
            path <- file.path(pkgpath, paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        path <- file.path(pkgpath, "doc", what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        path <- file.path(pkgpath, what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        stop(gettextf("no documentation for %s found in package %s",
                      sQuote(what), sQuote(package)), domain = NA)
    }
    if(what == "FAQ") what <- "R-FAQ"
    if(what == "NEWS") {
	if(type == "pdf") {
	    path <- file.path(R.home("doc"), paste.(what, "pdf"))
	    if(file.exists(path)) {
		pdf_viewer(path)
		return(invisible(path))
	    }
	    type <- "html"
	}
        if(type == "html") {
            path <- file.path(R.home("doc"), "html", paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        ## This is in UTF-8 and has a BOM on the first line
        path <- file.path(R.home("doc"), what)
        tf <- tempfile()
        tmp <- readLines(path)
        tmp[1] <- ""
        writeLines(tmp, tf)
        file.show(tf, delete.file = TRUE, encoding = "UTF-8")
        return(invisible(path))
    } else if(what == "COPYING") {
        path <- file.path(R.home("doc"), what)
        file.show(path)
        return(invisible(path))
    } else if(what %in% dir(file.path(R.home("share"), "licenses"))) {
        path <- file.path(R.home("share"), "licenses", what)
        file.show(path)
        return(invisible(path))
    } else if(what %in% c("R-admin", "R-data", "R-exts", "R-FAQ", "R-intro",
                          "R-ints", "R-lang")) {
        if(type == "pdf") {
            path <- file.path(R.home("doc"), "manual", paste.(what, "pdf"))
            if(file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if(type == "html") {
            path <- file.path(R.home("doc"), "manual", paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        if(what == "R-FAQ" &&
           file.exists(path <- file.path(R.home("doc"), "FAQ"))) {
            file.show(path)
            return(invisible(path))
        }
    } else if(.Platform$OS.type == "windows" && what %in% "rw-FAQ") {
        if(type == "pdf") type <- "html"
        if(type == "html") {
            path <- file.path(R.home("doc"), "html", paste.(what, "html"))
            if(file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        path <- file.path(R.home("doc"), what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        path <- file.path(R.home(), "src", "gnuwin32", what)
        if(file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
    } else {
        rdocdir <- R.home("doc")
        docs <- dir(rdocdir, full.names=TRUE)
        docs <- docs[sapply(docs, function(x) file_test("-f", x))]
        m <- match(what, basename(docs), 0L)
        if(m > 0L) {
            file.show(docs[m])
            return(invisible(docs[m]))
        }
    }
    stop("document not found")
}
