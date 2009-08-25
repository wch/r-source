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

### TODO

## Better directory listing, possibly via a redirect to a file:// link
## Handle more of the types what might be in a vignette directory.
## More checks on existence of packages, error-reporting pages.

## basic version - a placeholder
.HTMLdirListing <- function(dir)
{
    files <- list.files(dir) # note, no hidden files
    ## Use (UTF-8) quotes here?
    title <- paste("Listing of directory", dir)
    out <- paste('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\n',
        '<html><head><title>R: ', title, '</title>\n',
        '<meta http-equiv="Content-Type" content="text/html; charset="UTF-8">',
        '</head><body>\n',
        '<h1>', title, '</h1>\n\n<hr>\n', sep="")
    if(!length(files))
        out <- c(out, "No files in this directory")
    else {
        out <- c(out, "<ul>",
                 paste("<LI>", iconv(files, "", "UTF-8"), "</LI>", sep = ""),
                 "</ul>")
    }
    out <- c(out, "<hr>\n</BODY></HTML>")
    list(payload = paste(out, collapse="\n"))
}


## 'query' is unused.
httpd <- function(path, query, ...)
{
    unfix <- function(file)
    {
        ## we need to re-fix links altered by fixup.package.URLs
        fixedfile <- sub("/html/.*", "/fixedHTMLlinks", file)
        if(file.exists(fixedfile)) {
            top <- readLines(fixedfile)
            lines <- readLines(file)
            lines <- gsub(paste(top, "library", sep="/"),
                          "../../", lines)
            lines <- gsub(paste(top, "doc/", sep = "/"),
                          "../../../doc/", lines)
            return(list(payload=paste(lines, collapse="\n")))
        }
        list(file=file)
    }

    fileRegexp <- "^/library/([^/]*)/html/([^/]*)\\.html$"
    topicRegexp <- "^/library/([^/]*)/help/([^/]*)$"
    docRegexp <- "^/library/([^/]*)/doc(.*)"
    file <- NULL
    if (grepl(topicRegexp, path)) {
    	pkg <- sub(topicRegexp, "\\1", path)
    	if (pkg == "NULL") pkg <- NULL
    	topic <- sub(topicRegexp, "\\2", path)
    	file <- NULL
    	if (!is.null(pkg))
    	    file <- help(topic, package=(pkg), htmlhelp=FALSE, chmhelp=FALSE)
    	if (length(file) == 0L)
            file <- help(topic, htmlhelp=FALSE, chmhelp=FALSE,
                         try.all.packages=TRUE)
	if (length(file) == 0L) {
	    return(list(payload=paste('<p>No help found for topic ', topic,
                        ' in package ', pkg, '.</p>\n',
                        '<hr><div align="center">[<a href="00Index.html">Index</a>]</div>\n',
                        sep="", collapse="")))
	} else if (length(file) == 1L) {
	    path <- dirname(dirname(file))
	    file <- paste('../../', basename(path), '/html/', basename(file), '.html', sep='')
	    return(list(payload=paste('Redirect to <a href="', file, '">"',
                        basename(file), '"</a>', sep=''),
	    		"content-type"='text/html',
	    		header=paste('Location: ', file, '\n', sep=''),
	    		"status code" = 302L)) # temporary redirect
	} else if (length(file) > 1) {
            paths <- dirname(dirname(file))
            packages <- paste('<dt><a href="../../', basename(paths), '/html/',
                              basename(file), '.html">', basename(paths), '</a></dt><dd> (in library ',
                              dirname(paths), ")</dd>",
                              sep="", collapse="\n")
            return(list(payload=paste(gettextf("<p>Help on topic '%s' was found in the following packages:</p><dl>\n",
                        topic),
                        packages, "</dl>", sep="", collapse="\n")))
        }
    } else if (grepl(fileRegexp, path)) {
    	pkg <- sub(fileRegexp, "\\1", path)
    	topic <- sub(fileRegexp, "\\2", path)
        ## FIXME: what if package not found?
    	if (basename(path) == "00Index.html") {
            file <- file.path(system.file("html", package=pkg), "00Index.html")
            if(.Platform$OS.type == "windows") return(unfix(file))
    	    return(list(file=file))
    	} else
    	    file <- file.path(system.file("help", package=pkg), topic)
    } else if (grepl(docRegexp, path)) {
        ## vignettes etc directory
    	pkg <- sub(docRegexp, "\\1", path)
    	rest <- sub(docRegexp, "\\2", path)
        ## FIXME: what if package not found
        if(nzchar(rest)) {
            file <- paste(system.file("doc", package = pkg), rest, sep = "")
            ## FIXME: cope with more types, e.g. .R, .Rnw, .bib
            content_type <- ifelse(grepl("pdf$", path),  "application/pdf",
                                   "text/html")
            return(list(file=file, "content-type"=content_type))
        } else {
            ## request to list <pkg>/doc
            return(.HTMLdirListing(system.file("doc", package = pkg)))
        }
    }
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
	    return(list(payload=paste(readLines(temp), collapse="\n")))
	} else {
            ## This is a link of the form .../pkg/help/topic
            ## Try for pre-generated HTML.
            file2 <- paste(sub("/help/", "/html/", file, fixed = TRUE),
                           "html", sep=".")
            if(file.exists(file2)) {
                if(.Platform$OS.type == "windows") return(unfix(file2))
                return(list(file=file2))
            }
            return(list(file=file))
        }
    } else if(grepl("doc/html/.*html$" , path) &&
              file.exists(tmp <- file.path(tempdir(), ".R", path))) {
        ## use generated (or symlinked) copy
        return(list(file=tmp, "content-type"="text/html"))
    } else {
        ## If we got here, we've followed a link that's not to a man page.
        ## FIXME some of those things are vignette listings/overviews
    	file <- file.path(R.home(), path)
        content_type <- ifelse(grepl("css$", path),  "text/css",
                               ifelse(grepl("jpg$", path),  "image/jpeg",
                                      "text/html"))
    	return(list(file=file, "content-type"=content_type))
    }
}

httpdPort <- NULL

startDynamicHelp <- function(start=TRUE)
{
    env <- environment(startDynamicHelp)
    unlockBinding("httpdPort", env)
    if (start) {
	httpdPort <<- 8080L
	repeat { ## FIXME, no infinite loop please
	    status <- .Internal(startHTTPD("127.0.0.1", httpdPort))
	    if (status == 0L) break
	    httpdPort <<- httpdPort + 1L
	}
        ## FIXME: only assign this if we know it is working.
    } else {
        # FIXME actually shut down the server?
    	httpdPort <<- NULL
    }
    lockBinding("httpdPort", env)
    invisible(httpdPort)
}
