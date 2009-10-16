#  File src/library/utils/R/help.R
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

help <-
function(topic, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         try.all.packages = getOption("help.try.all.packages"),
         help_type = getOption("help_type"),
         chmhelp = getOption("chmhelp"),
         htmlhelp = getOption("htmlhelp"),
         offline = FALSE)
{
    if(!missing(package))
        if(is.name(y <- substitute(package)))
            package <- as.character(y)

    ## If no topic was given ...
    if(missing(topic)) {
        if(!missing(package))           # "Help" on package.
            return(library(help = package, lib.loc = lib.loc,
                           character.only = TRUE))
        if(!missing(lib.loc))           # "Help" on library.
            return(library(lib.loc = lib.loc))
        ## ultimate default is to give help on help
        topic <- "help"; package <- "utils"; lib.loc <- .Library
    }

    ischar <- tryCatch(is.character(topic) && length(topic) == 1L,
                       error = identity)
    if(inherits(ischar, "error")) ischar <- FALSE
    ## if this was not a length-one character vector, try for the name.
    if(!ischar) {
        ## the reserved words that could be parsed as a help arg:
        reserved <-
            c("TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
              "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse(substitute(topic))
        if(!is.name(substitute(topic)) && ! stopic %in% reserved)
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }

    help_type <- if(!length(help_type)) {
        if(offline) {
            warning('offline = TRUE is deprecated: use help_type ="postscript"')
            "ps"
        } else if(.Platform$OS.type == "windows" &&
                is.logical(chmhelp) && !is.na(chmhelp) && chmhelp) {
            warning('chmhelp = TRUE is no longer supported: use help_type ="text"')
            "txt"
        } else if(is.logical(htmlhelp) && !is.na(htmlhelp) && htmlhelp) {
            warning('htmhelp = TRUE is deprecated: use help_type ="html"')
            "html"
        } else
            "text"
    } else match.arg(tolower(help_type),
                     c("text", "html", "postscript", "ps", "pdf"))
    type <- switch(help_type,
                   "text" = "help",
                   "postscript" =,
                   "ps" =,
                   "pdf" = "latex",
                   help_type)

    ## Note that index.search() (currently?) only returns the first
    ## match for the given sequence of indices, and returns the empty
    ## string in case of no match.
    paths <- sapply(.find.package(package, lib.loc, verbose = verbose),
                    function(p) index.search(topic, p, "AnIndex", type))
    paths <- paths[paths != ""]

    tried_all_packages <- FALSE
    if(!length(paths)
       && is.logical(try.all.packages) && !is.na(try.all.packages)
       && try.all.packages && missing(package) && missing(lib.loc)) {
        ## Try all the remaining packages.
        lib.loc <- .libPaths()
        packages <- .packages(all.available = TRUE, lib.loc = lib.loc)
        packages <- packages[is.na(match(packages, .packages()))]
        for(lib in lib.loc) {
            ## <FIXME>
            ## Why does this loop over packages *inside* the loop
            ## over libraries?
            for(pkg in packages) {
                dir <- system.file(package = pkg, lib.loc = lib)
                paths <- c(paths,
                           index.search(topic, dir, "AnIndex", "help"))
            }
            ## </FIXME>
        }
        paths <- paths[paths != ""]
        tried_all_packages <- TRUE
    }

    paths <- unique(paths)
    attributes(paths) <-
        list(call = match.call(), topic = topic,
             tried_all_packages = tried_all_packages, type = help_type)
    class(paths) <- "help_files_with_topic"
    paths
}

print.help_files_with_topic <-
function(x, ...)
{
    browser <- getOption("browser")
    topic <- attr(x, "topic")
    type <- attr(x, "type")
    if (.Platform$GUI == "AQUA" && type == "html") {
        browser <- function(x, ...) {
            .Internal(aqua.custom.print("help-files", x))
    	    return(invisible(x))
        }
    }
    paths <- as.character(x)
    if(!length(paths)) {
        writeLines(c(gettextf("No documentation for '%s' in specified packages and libraries:",
                              topic),
                     gettextf("you could try '??%s'",
                              topic)))
        return(invisible(x))
    }

    if(type == "html")
        if (tools:::httpdPort == 0L) tools::startDynamicHelp()

    if(attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic '%s' is not in any loaded package but can be found in the following packages:",
                        topic)
        if (type == "html" && tools:::httpdPort > 0L) {
            path <- file.path(tempdir(), ".R/doc/html")
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            out <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
                         '<html><head><title>R: help</title>\n',
                         '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">\n',
                         '<link rel="stylesheet" type="text/css" href="/doc/html/R.css">\n',
                         '</head><body>\n\n<hr>\n', sep="")
            out <- c(out, '<p>', msg, '</p><br>')
            out <- c(out, '<table width="100%" summary="R Package list">\n',
                     '<tr align="left" valign="top">\n',
                     '<td width="25%">Package</td><td>Library</td></tr>\n')
            pkgs <- basename(paths)
            links <- paste('<a href="http://127.0.0.1:', tools:::httpdPort,
                           '/library/', pkgs, '/help/', topic, '">',
                           pkgs, '</a>', sep = "")
            out <- c(out, paste('<tr align="left" valign="top">\n',
                                '<td>', links, '</td><td>',
                                dirname(paths), '</td></tr>\n',
                                sep = ""))
            out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
            writeLines(out, file.path(path, "all.available.html"))
            browseURL(paste("http://127.0.0.1:", tools:::httpdPort,
                            "/doc/html/all.available.html", sep=""), browser)
        } else {
            writeLines(c(strwrap(msg), "",
                         paste(" ",
                               formatDL(c(gettext("Package"), basename(paths)),
                                        c(gettext("Library"), dirname(paths)),
                                        indent = 22))))
        }
    } else {
        if(length(paths) > 1L) {
            if (type == "html" && tools:::httpdPort > 0L) { # Redo the search if dynamic help is running
		browseURL(paste("http://127.0.0.1:", tools:::httpdPort,
                                "/library/NULL/help/", topic, sep=""), browser)
		return(invisible(x))
	    }
            file <- paths[1L]
            p <- paths
            msg <- gettextf("Help on topic '%s' was found in the following packages:",
                            topic)
            paths <- dirname(dirname(paths))
            txt <- formatDL(c("Package", basename(paths)),
                            c("Library", dirname(paths)),
                            indent = 22L)
            writeLines(c(strwrap(msg), "", paste(" ", txt), ""))
            if(interactive()) {
                fp <- file.path(paths, "Meta", "Rd.rds")
                tp <- basename(p)
                titles <- tp
                if(type == "html" || type == "latex")
                    tp <- tools::file_path_sans_ext(tp)
                for (i in seq_along(fp)) {
                    tmp <- try(.readRDS(fp[i]))
                    titles[i] <- if(inherits(tmp, "try-error"))
                        "unknown title" else
                    tmp[tools::file_path_sans_ext(tmp$File) == tp[i], "Title"]
                }
                txt <- paste(titles, " {", basename(paths), "}", sep="")
                ## FIXME: use html page for HTML help.
                res <- menu(txt, title = gettext("Choose one"),
                            graphics = getOption("menu.graphics"))
                if(res > 0) file <- p[res]
            } else {
                writeLines(gettext("\nUsing the first match ..."))
            }
        }
        else
            file <- paths

        if(type == "html") {
            if (tools:::httpdPort > 0L) {
		path <- dirname(file)
		dirpath <- dirname(path)
		pkgname <- basename(dirpath)
		browseURL(paste("http://127.0.0.1:", tools:::httpdPort,
                                "/library/", pkgname, "/html/", basename(file),
                                sep=""), browser)
            } else {
                warning("HTML help is unavailable", call. = FALSE)
                att <- attributes(x)
                xx <- sub("/html/([^/]*)\\.html$", "/help/\\1", x)
                attributes(xx) <- att
                attr(xx, "type") <- "text"
                print(xx)
            }
        } else if(type == "text") {
            path <- dirname(file)
            dirpath <- dirname(path)
            pkgname <- basename(dirpath)
            RdDB <- file.path(path, pkgname)
            if(file.exists(paste(RdDB, "rdx", sep="."))) {
                temp <- tools::Rd2txt(tools:::fetchRdDB(RdDB, basename(file)),
                                      out=tempfile("Rtxt"), package=pkgname)
                file.show(temp,
                          title = gettextf("R Help on '%s'", topic),
                          delete.file = TRUE)
            } else {
                ## Fallback to the old system with help pages
                ## stored as plain text
                zfile <- zip.file.extract(file, "Rhelp.zip")
                if (file.exists(zfile)) {
                    first <- readLines(zfile, n = 1L)
                    enc <- if(length(grep("\\(.*\\)$", first)))
                        sub("[^(]*\\((.*)\\)$", "\\1", first) else ""
                    if(enc == "utf8") enc <- "UTF-8"
                    ## allow for 'smart' quotes on Windows, which work
                    ## in all but CJK encodings
                    if(.Platform$OS.type == "windows" && enc == ""
                       && l10n_info()$codepage < 1000) enc <- "CP1252"
                    file.show(zfile,
                              title = gettextf("R Help on '%s'", topic),
                              delete.file = (zfile != file),
                              encoding = enc)
                } else
		    stop(gettextf("No text help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
            }
        }
        else if(type %in% c("ps", "postscript", "pdf")) {
            ok <- FALSE
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if(zfile != file) on.exit(unlink(zfile))
            if(file.exists(zfile)) {
                .show_help_on_topic_offline(zfile, topic, type)
                ok <- TRUE
            } else {
                ## look for stored Rd files
                path <- dirname(file) # .../pkg/latex
                dirpath <- dirname(path)
                pkgname <- basename(dirpath)
                RdDB <- file.path(dirpath, "help", pkgname)
                if(file.exists(paste(RdDB, "rdx", sep="."))) {
                    ## message("on-demand Rd conversion for ", sQuote(topic))
                    key <- sub("\\.tex$", "", basename(file))
                    tf2 <- tempfile("Rlatex")
                    tools::Rd2latex(tools:::fetchRdDB(RdDB, key), tf2)
                    .show_help_on_topic_offline(tf2, topic, type)
                    ok <- TRUE
                }
            }
            if(!ok)
                stop(gettextf("No offline help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
        }
    }

    invisible(x)
}

.show_help_on_topic_offline <- function(file, topic, type = "postscript")
{
    encoding <-""
    lines <- readLines(file)
    encpatt <- "^\\\\inputencoding\\{(.*)\\}$"
    if(length(res <- grep(encpatt, lines, perl = TRUE, useBytes = TRUE)))
        encoding <- sub(encpatt, "\\1", lines[res],
                        perl = TRUE, useBytes = TRUE)
    texfile <- paste(topic, ".tex", sep = "")
    on.exit(unlink(texfile)) ## ? leave to helper
    opt <- if(type == "PDF") {
        if(nzchar(opt <- Sys.getenv("R_RD4PDF"))) opt else "times"
    } else {
        if(nzchar(opt <- Sys.getenv("R_RD4DVI"))) opt else "ae"
    }
    cat("\\documentclass[", getOption("papersize"), "paper]{article}\n",
        "\\usepackage[", opt, "]{Rd}\n",
        if(nzchar(encoding)) sprintf("\\usepackage[%s]{inputenc}\n", encoding),
        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
        "\\begin{document}\n",
        file = texfile, sep = "")
    file.append(texfile, file)
    cat("\\end{document}\n", file = texfile, append = TRUE)
    helper <- if (exists("offline_help_helper", envir = .GlobalEnv))
        get("offline_help_helper", envir = .GlobalEnv)
    else utils:::offline_help_helper
    helper(texfile, type)
    invisible()
}
