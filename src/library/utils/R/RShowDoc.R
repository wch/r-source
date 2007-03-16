RShowDoc <- function(what, type=c("pdf", "html", "txt"), package)
{
    paste. <- function(x, ext) paste(x, ext, sep=".")
    pdf_viewer <- function(path) {
        if(.Platform$OS.type == "windows") shell.exec(path)
        else system(paste(shQuote(getOption("pdfviewer")), shQuote(path)),
                    wait = FALSE)
    }

    html_viewer <- function(path) {
        ## we don't use browseURL under Windows as shell.exec does
        ## not want an encoded URL.
        if(.Platform$OS.type == "windows") shell.exec(chartr("/", "\\", path))
        else browseURL(paste("file://", URLencode(path), sep=""))
    }

    type <- match.arg(type)
    if(missing(what) || length(what) != 1 || !is.character(what)) {
        message("   RShowDoc() should be used with a character string argument specifying\n   a documentation file")
        return(invisible())
    }
    if(!missing(package)) {
        pkgpath <- .find.package(package)
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
        stop(gettextf("no documentation for '%s' found in package '%s'",
                      what, package), domain = NA)
    }
    if(what == "FAQ") what <- "R-FAQ"
    if(what %in% c("NEWS", "COPYING")) {
        path <- file.path(R.home(), what)
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
                shell.exec(chartr("/", "\\", path))
                return(invisible(path))
            }
        }
        path <- file.path(R.home(), "doc", what)
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
        m <- match(what, basename(docs), 0)
        if(m > 0) {
            file.show(docs[m])
            return(invisible(docs[m]))
        }
    }
    stop("document not found")
}
