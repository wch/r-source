#  File src/library/tools/R/dynamicHelp.R
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


.HTMLdirListing <- function(dir, base)
{
    files <- list.files(dir) # note, no hidden files
    out <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
        '<html><head><title>R: ', dir , '</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">\n',
        '<link rel="stylesheet" type="text/css" href="/doc/html/R.css">\n',
        '</head><body>\n',
        '<h1>', "Listing of directory<br>", dir, '</h1>\n\n<hr>\n', sep="")
    if(!length(files))
        out <- c(out, gettext("No files in this directory"))
    else {
        urls <- paste('<a href="', base, '/', files, '">', files, '</a>',
                      sep = "")
        out <- c(out, "<dl>",
                 paste("<dd>", iconv(urls, "", "UTF-8"), "</dd>", sep = ""),
                 "</dl>")
    }
    out <- c(out, "<hr>\n</BODY></HTML>")
    list(payload = paste(out, collapse="\n"))
}

.HTMLsearch <- function(query)
{
    res <- if(identical(names(query), "category"))
        help.search(keyword = query)$matches
    else {
       nm <- names(query)
       m <- match("exact", nm)
       if(is.na(m)) help.search(query[1L], nm)$matches
       else help.search(query[1L], nm[-m], agrep = FALSE)$matches
    }
    title <- "Search Results"
    out <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
                 '<html><head><title>R: ', title , '</title>\n',
                 '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">\n',
                 '<link rel="stylesheet" type="text/css" href="/doc/html/R.css">\n',
                 '</head><body>\n',
                 '<h1>', title, '</h1>\n',
                 'The search string was <b>"', query[1L], '"</b><hr>\n',
                 sep="")

    if(nrow(res)) {
        paths <- paste("/library/", res[, "Package"], "/html/",
                       res[, "topic"], ".html", sep = "")
        urls <- paste('<a href="', paths, '">',
                      res[, "Package"], "::", res[, "topic"],
                      '</a>', sep = "")
        out <- c(out, "<dl>",
                 paste("<dt>", iconv(urls, "", "UTF-8"), "</dt>\n",
                       "<dd>", res[, "title"], "</dd>", sep = ""),
                 "</dl>")
    } else out <- c(out, gettext("No results found"))
    out <- c(out, "<hr>\n</BODY></HTML>")
    list(payload = paste(out, collapse="\n"))
}

## This may be asked for
##  R.css, favicon.ico
##  searches with path = "/doc/html/Search"
##  documentation with path = "/doc/....", possibly updated under tempdir()/.R
##  html help, either by topic, /library/<pkg>/help/<topic> (pkg=NULL means any)
##             or by file, /library/<pkg>/html/<file>.html
httpd <- function(path, query, ...)
{
    unfix <- function(file)
    {
        ## we need to re-fix links altered by fixup.package.URLs
        ## in R < 2.10.0
        fixedfile <- sub("/html/.*", "/fixedHTMLlinks", file)
        if(file.exists(fixedfile)) {
            top <- readLines(fixedfile)
            lines <- readLines(file)
            lines <- gsub(paste(top, "library", sep="/"),
                          "../../", lines, fixed = TRUE)
            lines <- gsub(paste(top, "doc/", sep = "/"),
                          "../../../doc/", lines, fixed = TRUE)
            return(list(payload=paste(lines, collapse="\n")))
        }
        list(file=file)
    }

    mime_type <- function(path)
    {
        ext <- strsplit(path, ".", fixed = TRUE)[[1L]]
        if(n <- length(ext)) ext <- ext[n] else ""
        switch(ext,
               "css" = "text/css",
               "gif" = "image/gif", # in R2HTML
               "jpg" = "image/jpeg",
               "html" = "text/html",
               "pdf" = "application/pdf",
               "eps" =,
               "ps" = "application/postscript", # in GLMMGibbs, mclust
               "sgml"= "text/sgml", # in RGtk2
               "xml" = "text/xml",  # in RCurl
               "text/plain")
    }

    sQuote <- function(text) # needs to be in UTF-8
        ## paste("\u2018", text, "\u2019", sep = "") # if we require MBCS
        paste("'", text, "'", sep="")

    error_page <- function(msg)
    {
        out <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
                     '<html><head><title>R: doc not found</title>\n',
                     '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">\n',
                     '<link rel="stylesheet" type="text/css" href="/doc/html/R.css">\n',
                     '</head><body>\n', msg, sep = "")
        list(payload = out)
    }

    if (grepl("R\\.css$", path))
        return(list(file = file.path(R.home("doc"), "html", "R.css")))
    else if(path == "/favicon.ico")
        return(list(file = file.path(R.home("doc"), "html", "favicon.ico")))
    else if(!grepl("^/(doc|library)/", path))
        return(error_page("Only URLs under /doc and /library are allowed"))

    ## ----------------------- per-package documentation ---------------------
    ## seems we got ../..//<pkg> in the past
    fileRegexp <- "^/library/+([^/]*)/html/([^/]*)\\.html$"
    topicRegexp <- "^/library/+([^/]*)/help/([^/]*)$"
    docRegexp <- "^/library/([^/]*)/doc(.*)"
    file <- NULL
    if (grepl(topicRegexp, path)) {
        ## ----------------------- package help by topic ---------------------
    	pkg <- sub(topicRegexp, "\\1", path)
    	if (pkg == "NULL") pkg <- NULL  # how can this occur?
    	topic <- sub(topicRegexp, "\\2", path)
        ## if a package is specified, look there first
    	if (!is.null(pkg)) # () avoids deparse here
    	    file <- help(topic, package=(pkg), help_type = "text")
    	if (!length(file))
            file <- help(topic, help_type = "text", try.all.packages = TRUE)
	if (!length(file)) {
##             msg <- if(!is.null(pkg))
##                 gettextf("No help found for topic '%s' in package '%s'.",
##                         topic, pkg)
##             else
            msg <- gettextf("No help found for topic '%s' in any package.",
                            topic)
	    return(list(payload = error_page(msg)))
	} else if (length(file) == 1L) {
	    path <- dirname(dirname(file))
	    file <- paste('../../', basename(path), '/html/',
                          basename(file), '.html', sep='')
	    return(list(payload=paste('Redirect to <a href="', file, '">"',
                        basename(file), '"</a>', sep=''),
	    		"content-type"='text/html',
	    		header=paste('Location: ', file, '\n', sep=''),
	    		"status code" = 302L)) # temporary redirect
	} else if (length(file) > 1L) {
            paths <- dirname(dirname(file))
            fp <- file.path(paths, "Meta", "Rd.rds")
            tp <- basename(file)
            titles <- tp
            for (i in seq_along(fp)) {
                tmp <- try(.readRDS(fp[i]))
                titles[i] <- if(inherits(tmp, "try-error"))
                    "unknown title" else
                    tmp[tools::file_path_sans_ext(tmp$File) == tp[i], "Title"]
            }
            packages <- paste('<dt><a href="../../', basename(paths), '/html/',
                              basename(file), '.html">', titles,
                              '</a></dt><dd> (in package <a href="../../', basename(paths),
                              '/html/00Index.html">', basename(paths),
                              '</a> in library ', dirname(paths), ")</dd>",
                              sep="", collapse="\n")

            return(list(payload =
                        paste(gettextf("<p>Help on topic '%s' was found in the following packages:</p><dl>\n", topic),
                              packages, "</dl>", sep="", collapse="\n") ))
        }
    } else if (grepl(fileRegexp, path)) {
        ## ----------------------- package help by file ---------------------
    	pkg <- sub(fileRegexp, "\\1", path)
    	helpdoc <- sub(fileRegexp, "\\2", path)
        if (helpdoc == "00Index") {
            file <- system.file("html", "00Index.html", package=pkg)
            if(!nzchar(file) || !file.exists(file)) {
                if(nzchar(system.file(package=pkg)))
                    return(error_page(gettextf("No package index found for package %d", sQuote(pkg))))
                else
                    return(error_page(gettextf("No package of name %s could be located", sQuote(pkg) )))
            } else {
                if(.Platform$OS.type == "windows") return(unfix(file))
                return(list(file = file))
            }
    	} else {
            file <- system.file("help", package = pkg)
            if (!nzchar(file)) {
                if(nzchar(system.file(package = pkg)))
                    return(error_page(gettextf("No help found for package %s", sQuote(pkg) )))
                else
                   return(error_page(gettextf("No package of name %s could be located", sQuote(pkg) )))
            }
            ## if 'topic' is not a help doc, try it as an alias in the package
            contents <- .readRDS(sub("/help", "/Meta/Rd.rds", file, fixed = TRUE))
            files <- sub("\\.[Rr]d$", "", contents$File)
            if(! helpdoc %in% files) {
                ## or call help()
                aliases <- contents$Aliases
                lens <- sapply(aliases, length)
                aliases <- structure(rep.int(contents$File, lens),
                                     names = unlist(aliases))
                tmp <- sub("\\.[Rr]d$", "", aliases[helpdoc])
                if(is.na(tmp)) {
                    msg <- gettextf("Link %s in package %s could not be located",
                                    sQuote(helpdoc), sQuote(pkg))
                    files <- help(helpdoc, help_type = "text",
                                  try.all.packages = TRUE)
                    if (length(files)) {
                        path <- dirname(dirname(files))
                        files <- paste(basename(path), '/html/',
                                       basename(files), '.html', sep='')
                        msg <- c(msg, "<br>",
                                 "However, you might be looking for one of",
                                 "<p></p>",
                                 paste('<p><a href="/library/', files, '">',
                                       ".../", files, "</a></p>", sep="")
                                 )
                    }
                    return(error_page(paste(msg, collapse ="\n")))
                }
                helpdoc <- tmp
            }
            ## this is not a real file [*]
    	    file <- file.path(file, helpdoc)
        }
    } else if (grepl(docRegexp, path)) {
        ## ----------------------- package doc directory ---------------------
        ## vignettes etc directory
    	pkg <- sub(docRegexp, "\\1", path)
    	rest <- sub(docRegexp, "\\2", path)
        docdir <- system.file("doc", package = pkg)
        if(!nzchar(docdir))
            return(error_page(gettextf("No docs found for package %s",
                                       sQuote(pkg))))
        if(nzchar(rest)) {
            file <- paste(docdir, rest, sep = "")
            return(list(file = file, "content-type" = mime_type(path)))
        } else {
            ## request to list <pkg>/doc
            return(.HTMLdirListing(docdir,
                                   paste("/library", pkg, "doc", sep="/")))
        }
    }
    ## to get here we came from [*] or this was not within a package
    if (!is.null(file)) {
	path <- dirname(file)
	dirpath <- dirname(path)
	pkgname <- basename(dirpath)
	RdDB <- file.path(path, pkgname)
	if(file.exists(paste(RdDB, "rdx", sep="."))) {
	    outfile <- tempfile("Rhttpd")
	    temp <- tools::Rd2HTML(tools:::fetchRdDB(RdDB, basename(file)),
                                   out = outfile, package = pkgname,
                                   dynamic = TRUE)
	    on.exit(unlink(outfile))
	    return(list(payload = paste(readLines(temp), collapse = "\n")))
	} else {
            ## Try for pre-generated HTML.
            file2 <- paste(sub("/help/", "/html/", file, fixed = TRUE),
                           "html", sep = ".")
            if(file.exists(file2)) {
                if(.Platform$OS.type == "windows") return(unfix(file2))
                return(list(file = file2))
            }
            return(list(file = file))
        }
    }

    ## ----------------------- R docs ---------------------
    if(path == "/doc/html/Search.html") {
        ## redirect to the page that has search enabled
        list(file = file.path(R.home("doc"), "html/SearchOn.html"),
             "content-type" = "text/html")
    } else if(grepl("doc/html/.*html$" , path) &&
              file.exists(tmp <- file.path(tempdir(), ".R", path))) {
        ## use updated version, e.g. of packages.html
        list(file = tmp, "content-type" = "text/html")
    } else if(path == "/doc/html/Search") {
        .HTMLsearch(query)
    } else {
        file <- if(grepl("^/doc/", path)) {
            ## /doc/AUTHORS and so on.
            file.path(R.home("doc"), sub("^/doc", "", path))
        } else file.path(R.home(), path) # not clear there any of these
        list(file = file, "content-type" = mime_type(path))
    }
}

## 0 = untried, < 0 = failed to start,  > 0 = actual port
httpdPort <- 0L

startDynamicHelp <- function(start=TRUE)
{
    env <- environment(startDynamicHelp)
    if(nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
        unlockBinding("httpdPort", env)
        httpdPort <<- -1L
        lockBinding("httpdPort", env)
        warning("httpd server disabled by R_DISABLE_HTTPD",
                immediate. = TRUE)
        return(httpdPort)
    }
    if (start && httpdPort) {
        if(httpdPort > 0) stop("server already running")
        else stop("server could not be started earlier")
    }
    if(!start && httpdPort <= 0L)
        stop("no running server to stop")
    unlockBinding("httpdPort", env)
    if (start) {
        message("starting httpd help server ...", appendLF = FALSE)
        utils::flush.console()
        OK <- FALSE
	for(i in 1:10) {
            tmp <- as.integer(runif(1, 10000, 32000))
            ## the next can throw an R-level error,
            ## so do not assign port unless it succeeds.
	    status <- .Internal(startHTTPD("127.0.0.1", tmp))
	    if (status == 0L) {
                OK <- TRUE
                httpdPort <<- tmp
                break
            }
            if (status != 2L) break
            ## so status was -2, which means port in use
	}
        if (OK) {
            message(" done")
            utils::flush.console()
            ## FIXME: actually test the server
        } else {
            warning("failed to start the httpd server",
                    immediate. = TRUE)
            httpdPort <<- -1L
        }
    } else {
        ## Not really tested
        .Internal(stopHTTPD())
    	httpdPort <<- 0L
    }
    lockBinding("httpdPort", env)
    invisible(httpdPort)
}
