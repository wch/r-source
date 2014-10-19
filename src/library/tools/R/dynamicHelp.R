#  File src/library/tools/R/dynamicHelp.R
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


## This may be asked for
##  R.css, favicon.ico
##  searches with path = "/doc/html/Search"
##  documentation with path = "/doc/....", possibly updated under tempdir()/.R
##  demos with path "/demo/*"
##  Running demos, using path "/Demo/*"
##  html help, either by topic, /library/<pkg>/help/<topic> (pkg=NULL means any)
##             or by file, /library/<pkg>/html/<file>.html
httpd <- function(path, query, ...)
{
    .HTMLdirListing <- function(dir, base, up)
    {
        files <- list.files(dir)    # note, no hidden files are listed
        out <- HTMLheader(paste0("Listing of directory<br/>", dir),
        		  headerTitle = paste("R:", dir), logo=FALSE,
        		  up = up)
        if(!length(files))
            out <- c(out, gettext("No files in this directory"))
        else {
            urls <- paste0('<a href="', base, '/', files, '">', files, '</a>')
            out <- c(out, "<dl>",
                     paste0("<dd>", mono(iconv(urls, "", "UTF-8")), "</dd>"),
                     "</dl>")
        }
        out <- c(out, "<hr/>\n</body></html>")
        list(payload = paste(out, collapse="\n"))
    }

    .HTMLusermanuals <- function()
    {
        pkgs <- unlist(.get_standard_package_names())

        out <- HTMLheader("R User Manuals")
        for (pkg in pkgs) {
            vinfo <- getVignetteInfo(pkg)
     	    if (nrow(vinfo))
         	out <- c(out, paste0('<h2>Manuals in package', sQuote(pkg),'</h2>'),
         		 makeVignetteTable(cbind(Package=pkg, vinfo[,c("File", "Title", "PDF", "R"), drop = FALSE])))
     	}
        out <- c(out, "<hr/>\n</body></html>")
        list(payload = paste(out, collapse="\n"))
    }

    .HTMLsearch <- function(query)
    {
    	bool <- function(x) as.logical(as.numeric(x))
        res <- if(identical(names(query), "category"))
            help.search(keyword = query, verbose = 1L, use_UTF8 = TRUE)
        else {
            fields = c("alias", "concept", "title")
            args <- list(pattern = ".")
            for (i in seq_along(query))
            	switch(names(query)[i],
            		pattern = args$pattern <- query[i],
            		title = if (!bool(query[i])) fields <- setdiff(fields, "title"),
            		keyword = if (bool(query[i])) fields <- union(fields, "keyword"),
            		alias = if (!bool(query[i])) fields <- setdiff(fields, "alias"),
            		concept = if (!bool(query[i])) fields <- setdiff(fields, "concept"),
            		name = if (bool(query[i])) fields <- union(fields, "name"),
            		agrep = {
            		    args$agrep <- as.logical(query[i])
            		    if (is.na(args$agrep))
            		    	args$agrep <- as.numeric(query[i])
            		    if (is.na(args$agrep))
            		     	args$agrep <- query[i]
            		},
            		ignore.case = args$ignore.case <- bool(query[i]),
            		types = args$types <- strsplit(query[i], ";")[[1L]],
            		package = args$package <- strsplit(query[i], ";")[[1L]],
            		lib.loc = args$lib.loc <- strsplit(query[i], ";")[[1L]],
            		warning("Unrecognized search field: ", names(query)[i],
                                domain = NA)
                       )
            args$fields <- fields
            args$use_UTF8 <- TRUE
            do.call(help.search, args)
        }
        types <- res$types
        res <- res$matches
        title <- "Search Results"
        out <- c(HTMLheader(title),
                 if ("pattern" %in% names(query))
                     paste0('The search string was <b>"', query["pattern"], '"</b>'),
                 '<hr/>\n')

        if(!NROW(res))
            out <- c(out, gettext("No results found"))
        else {
            vigfile0 <- ""
            vigDB <- NULL
            for (type in types) {
		if(NROW(temp <- res[res[,"Type"] == type,,drop=FALSE]) > 0)
		    switch(type,
		    vignette = {
			out <- c(out, paste0("<h3>", gettext("Vignettes:"), "</h3>"), "<dl>")
			n <- NROW(temp)
			vignettes <- matrix("", n, 5)
			colnames(vignettes) <- c("Package", "File",
			                         "Title", "PDF","R")
			for (i in seq_len(NROW(temp))) {
			    topic <- temp[i, "topic"]
			    pkg <- temp[i, "Package"]
			    vigfile <- file.path(temp[i, "LibPath"], "Meta", "vignette.rds")
			    if (!identical(vigfile, vigfile0)) {
			    	vigDB <- readRDS(vigfile)
			    	vigfile0 <- vigfile
			    }
			    vignette <- vigDB[topic == file_path_sans_ext(vigDB$PDF),]
			    # There should be exactly one row in the result, but
			    # bad packages might have more, e.g. vig.Snw and vig.Rnw
			    vignettes[i,] <- c(pkg, unlist(vignette[1,c("File", "Title", "PDF", "R")]))
			 }
			 out <- c(out, makeVignetteTable(vignettes))
		    },
		    demo = {
			out <- c(out, paste0("<h3>", gettext("Code demonstrations:"), "</h3>"))
			out <- c(out, makeDemoTable(temp))
		    },
		    help = {
			out <- c(out, paste0("<h3>", gettext("Help pages:"), "</h3>"))
			out <- c(out, makeHelpTable(temp))
		    })
	    }
        }
        out <- c(out, "<hr/>\n</body></html>")
        list(payload = paste(out, collapse="\n"))
    }

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
        list(file = file)
    }

    mime_type <- function(path)
    {
        ext <- strsplit(path, ".", fixed = TRUE)[[1L]]
        if(n <- length(ext)) ext <- ext[n] else ""
        switch(ext,
               "css" = "text/css",
               "gif" = "image/gif", # in R2HTML
               "jpg" = "image/jpeg",
               "png" = "image/png",
               "svg" = "image/svg+xml",
               "html" = "text/html",
               "pdf" = "application/pdf",
               "eps" =,
               "ps" = "application/postscript", # in GLMMGibbs, mclust
               "sgml" = "text/sgml", # in RGtk2
               "xml" = "text/xml",  # in RCurl
               "text/plain")
    }

    charsetSetting <- function(pkg) {
    	encoding <-read.dcf(system.file("DESCRIPTION", package=pkg), "Encoding")
	if (is.na(encoding))
	    ""
        else
    	    paste0("; charset=", encoding)
    }

    sQuote <- function(text)
        paste0("&lsquo;", text, "&rsquo;")
    mono <- function(text)
        paste0('<span class="samp">', text, "</span>")

    error_page <- function(msg)
        list(payload =
             paste0(HTMLheader("httpd error"), msg, "\n</body></html>"))

    cssRegexp <- "^/library/([^/]*)/html/R.css$"
    if (grepl("R\\.css$", path) && !grepl(cssRegexp, path))
        return(list(file = file.path(R.home("doc"), "html", "R.css"),
                    "content-type" = "text/css"))
    else if(path == "/favicon.ico")
        return(list(file = file.path(R.home("doc"), "html", "favicon.ico")))
    else if(path == "/NEWS")
         return(list(file = file.path(R.home("doc"), "html", "NEWS.html")))
    else if(grepl("^/NEWS[.][[:digit:]]$", path))
    	return(list(file = file.path(R.home("doc"), sub("/", "", path)),
    	            "content-type" = "text/plain; encoding=utf-8"))
    else if(!grepl("^/(doc|library|session)/", path))
        return(error_page(paste("Only NEWS and URLs under", mono("/doc"),
                                "and", mono("/library"), "are allowed")))
    else if(path == "/doc/html/UserManuals.html")
    	return(.HTMLusermanuals())

    ## ----------------------- per-package documentation ---------------------
    ## seems we got ../..//<pkg> in the past
    fileRegexp <- "^/library/+([^/]*)/html/([^/]*)\\.html$"
    topicRegexp <- "^/library/+([^/]*)/help/([^/]*)$"
    docRegexp <- "^/library/([^/]*)/doc(.*)"
    demoRegexp <- "^/library/([^/]*)/demo$"
    demosRegexp <- "^/library/([^/]*)/demo/([^/]*)$"
    DemoRegexp <- "^/library/([^/]*)/Demo/([^/]*)$"
    newsRegexp <- "^/library/([^/]*)/NEWS$"
    figureRegexp <- "^/library/([^/]*)/(help|html)/figures/([^/]*)$"
    sessionRegexp <- "^/session/"

    file <- NULL
    if (grepl(topicRegexp, path)) {
        ## ----------------------- package help by topic ---------------------
    	pkg <- sub(topicRegexp, "\\1", path)
    	if (pkg == "NULL") pkg <- NULL  # There were multiple hits in the console
    	topic <- sub(topicRegexp, "\\2", path)
        ## if a package is specified, look there first, then everywhere
    	if (!is.null(pkg)) # () avoids deparse here
    	    file <- help(topic, package = (pkg), help_type = "text")
    	if (!length(file))
            file <- help(topic, help_type = "text", try.all.packages = TRUE)
	if (!length(file)) {
            msg <- gettextf("No help found for topic %s in any package.",
                            mono(topic))
	    return(list(payload = error_page(msg)))
	} else if (length(file) == 1L) {
	    path <- dirname(dirname(file))
	    file <- paste0('../../', basename(path), '/html/',
                           basename(file), '.html')
            ## cat("redirect to", file, "\n")
            ## We need to do this because there are static HTML pages
            ## with links to "<file>.html" for topics in the same
            ## package, and if we served one of such a page as a link from
            ## a different package those links on the page would not work.
	    return(list(payload = paste0('Redirect to <a href="', file, '">"',
                                         basename(file), '"</a>'),
	    		"content-type" = 'text/html',
	    		header = paste0('Location: ', file),
	    		"status code" = 302L)) # temporary redirect
	} else if (length(file) > 1L) {
            paths <- dirname(dirname(file))
            fp <- file.path(paths, "Meta", "Rd.rds")
            tp <- basename(file)
            titles <- tp
            for (i in seq_along(fp)) {
                tmp <- try(readRDS(fp[i]))
                titles[i] <- if(inherits(tmp, "try-error"))
                    "unknown title" else
                    tmp[file_path_sans_ext(tmp$File) == tp[i], "Title"]
            }
            packages <- paste('<dt><a href="../../', basename(paths), '/html/',
                              basename(file), '.html">', titles,
                              '</a></dt><dd> (in package <a href="../../',
                              basename(paths),
                              '/html/00Index.html">', basename(paths),
                              '</a> in library ', dirname(paths), ")</dd>",
                              sep = "", collapse = "\n")

            return(list(payload =
                        paste("<p>",
                              ## for languages with multiple plurals ....
                              sprintf(ngettext(length(paths),
                                               "Help on topic '%s' was found in the following package:",
                                               "Help on topic '%s' was found in the following packages:"
                                               ), topic),
                              "</p><dl>\n",
                              packages, "</dl>", sep = "", collapse = "\n")
                        ))
        }
    } else if (grepl(fileRegexp, path)) {
        ## ----------------------- package help by file ---------------------
    	pkg <- sub(fileRegexp, "\\1", path)
    	helpdoc <- sub(fileRegexp, "\\2", path)
        if (helpdoc == "00Index") {
            ## ------------------- package listing ---------------------
            file <- system.file("html", "00Index.html", package = pkg)
            if(!nzchar(file) || !file.exists(file)) {
                msg <- if(nzchar(system.file(package = pkg)))
                    gettextf("No package index found for package %s",
                             mono(pkg))
                else
                    gettextf("No package named %s could be found",
                             mono(pkg))
                return(error_page(msg))
            } else {
                if(.Platform$OS.type == "windows") return(unfix(file))
                return(list(file = file))
            }
    	}
        ## ----------------------- package help file ---------------------
        path <- system.file("help", package = pkg)
        if (!nzchar(path)) {
            msg <- if(nzchar(system.file(package = pkg)))
                gettextf("No help found for package %s", mono(pkg) )
            else
                gettextf("No package named %s could be found", mono(pkg))
            return(error_page(msg))
        }
        ## if 'topic' is not a help doc, try it as an alias in the package
        contents <- readRDS(sub("/help", "/Meta/Rd.rds", path, fixed = TRUE))
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
                                mono(helpdoc), mono(pkg))
                files <- help(helpdoc, help_type = "text",
                              try.all.packages = TRUE)
                if (length(files)) {
                    path <- dirname(dirname(files))
                    files <- paste0('/library/', basename(path), '/html/',
                                    basename(files), '.html')
                    msg <- c(msg, "<br/>",
                             "However, you might be looking for one of",
                             "<p></p>",
                             paste0('<p><a href="', files, '">',
                                    mono(files), "</a></p>")
                             )
                }
                return(error_page(paste(msg, collapse = "\n")))
            }
            helpdoc <- tmp
        }

        ## Now we know which document we want in which package
	dirpath <- dirname(path)
##	pkgname <- basename(dirpath)
##	RdDB <- file.path(path, pkgname)
        outfile <- tempfile("Rhttpd")
        Rd2HTML(utils:::.getHelpFile(file.path(path, helpdoc)),
                out = outfile, package = dirpath,
                dynamic = TRUE)
        on.exit(unlink(outfile))
        return(list(payload = paste(readLines(outfile), collapse = "\n")))
    } else if (grepl(docRegexp, path)) {
        ## ----------------------- package doc directory ---------------------
    	pkg <- sub(docRegexp, "\\1", path)
    	rest <- sub(docRegexp, "\\2", path)
        docdir <- system.file("doc", package = pkg)
        up <- paste0("/library/", pkg, "/html/00Index.html")
        if(!nzchar(docdir))
            return(error_page(gettextf("No docs found for package %s",
                                       mono(pkg))))
        if(nzchar(rest) && rest != "/") {
            ## FIXME should we check existence here?
            file <- paste0(docdir, rest)
            if(dir.exists(file))
                return(.HTMLdirListing(file,
                                       paste0("/library/", pkg, "/doc", rest),
                                       up))
            else
                return(list(file = file, "content-type" = mime_type(rest)))
        } else {
            ## request to list <pkg>/doc
            return(.HTMLdirListing(docdir,
                                   paste("/library", pkg, "doc", sep="/"),
                                   up))
        }
    } else if (grepl(demoRegexp, path)) {
    	pkg <- sub(demoRegexp, "\\1", path)

    	url <- paste0("http://127.0.0.1:", httpdPort,
                      "/doc/html/Search?package=",
                      pkg, "&agrep=FALSE&types=demo")
    	return(list(payload = paste0('Redirect to <a href="', url,
    				'">help.search()</a>'),
		    		"content-type" = 'text/html',
		    		header = paste0('Location: ', url),
	    		"status code" = 302L)) # temporary redirect
    } else if (grepl(demosRegexp, path)) {
	    pkg <- sub(demosRegexp, "\\1", path)
	    demo <- sub(demosRegexp, "\\2", path)
	    file <- system.file(file.path("demo", demo), package=pkg)
	    return(list(file = file, "content-type" = mime_type(demo)))

    } else if (grepl(DemoRegexp, path)) {
    	pkg <- sub(DemoRegexp, "\\1", path)
    	demo <- sub(DemoRegexp, "\\2", path)
    	demo(demo, package=pkg, character.only=TRUE, ask=FALSE)
	return( list(payload = paste0("Demo '", pkg, "::", demo,
				"' was run in the console.",
				" To repeat, type 'demo(",
				pkg, "::", demo,
				")' in the console.")) )
    } else if (grepl(newsRegexp, path)) {
    	pkg <- sub(newsRegexp, "\\1", path)
    	formatted <- toHTML(news(package = pkg),
    		            title=paste("NEWS in package", sQuote(pkg)),
    			    up="html/00Index.html")
        if (length(formatted))
    	    return( list(payload = paste(formatted, collapse="\n")) )
    	else
    	    return( list(file = system.file("NEWS", package = pkg),
    	                 "content-type" = paste0("text/plain", charsetSetting(pkg) ) ) )
    } else if (grepl(figureRegexp, path)) {
        pkg <- sub(figureRegexp, "\\1", path)
        fig <- sub(figureRegexp, "\\3", path)
        file <- system.file("help", "figures", fig, package=pkg)
        return( list(file=file, "content-type" = mime_type(fig)) )
    } else if (grepl(sessionRegexp, path)) {
        tail <- sub(sessionRegexp, "", path)
    	file <- file.path(tempdir(), tail)
    	return( list(file=file, "content-type" = mime_type(tail)) )
    } else if (grepl(cssRegexp, path)) {
    	pkg <- sub(cssRegexp, "\\1", path)
        return( list(file = system.file("html", "R.css", package = pkg),
                     "content-type" = "text/css") )
    } else if (grepl("^/library/", path)) {
        descRegexp <- "^/library/+([^/]+)/+DESCRIPTION$"
        if(grepl(descRegexp, path)) {
            pkg <- sub(descRegexp, "\\1", path)
            file <- system.file("DESCRIPTION", package = pkg)
            return(list(file = file, "content-type" = paste0("text/plain", charsetSetting(pkg))))
        } else
            return(error_page(gettextf("Only help files, %s, %s and files under %s and %s in a package can be viewed", mono("NEWS"),
                              mono("DESCRIPTION"), mono("doc/"), mono("demo/"))))
    }

    ## ----------------------- R docs ---------------------
    if(path == "/doc/html/Search.html") {
        ## redirect to the page that has search enabled
        list(file = file.path(R.home("doc"), "html/SearchOn.html"))
    } else if(path == "/doc/html/Search") {
        .HTMLsearch(query)
    } else if(path == "/doc/html/packages.html") {
        ## remake as needed
        utils::make.packages.html(temp = TRUE)
        list(file = file.path(tempdir(), ".R", path))
    } else if(grepl("doc/html/.*html$" , path) &&
              file.exists(tmp <- file.path(tempdir(), ".R", path))) {
        ## use updated version, e.g. of packages.html
        list(file = tmp)
    } else {
        if(grepl("^/doc/", path)) {
            ## /doc/AUTHORS and so on.
            file <- file.path(R.home("doc"), sub("^/doc", "", path))
        } else return(error_page(gettextf("unsupported URL %s", mono(path))))
        if(!file.exists(file))
            error_page(gettextf("URL %s was not found", mono(path)))
        else
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
        warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
        utils::flush.console()
        return(httpdPort)
    }
    if (start && httpdPort) {
        if(httpdPort > 0) stop("server already running")
        else stop("server could not be started on an earlier attempt")
    }
    if(!start && httpdPort <= 0L)
        stop("no running server to stop")
    unlockBinding("httpdPort", env)
    if (start) {
        message("starting httpd help server ...", appendLF = FALSE)
        utils::flush.console()
        OK <- FALSE
        ports <- getOption("help.ports")
        if (is.null(ports)) {
	    ## Choose 10 random port numbers between 10000 and 32000.
	    ## The random seed might match
	    ## on multiple instances, so add the time as well.  But the
	    ## time may only be accurate to seconds, so rescale it to
	    ## 5 minute units.
            ports <- 10000 + 22000*((stats::runif(10) + unclass(Sys.time())/300) %% 1)
        }
        ports <- as.integer(ports)
        for(i in seq_along(ports)) {
            ## the next can throw an R-level error,
            ## so do not assign port unless it succeeds.
	    status <- .Call(startHTTPD, "127.0.0.1", ports[i])
	    if (status == 0L) {
                OK <- TRUE
                httpdPort <<- ports[i]
                break
            }
            if (status != -2L) break
            ## so status was -2, which means port in use
	}
        if (OK) {
            message(" done")
            utils::flush.console()
            ## FIXME: actually test the server
        } else {
            warning("failed to start the httpd server", immediate. = TRUE)
            utils::flush.console()
            httpdPort <<- -1L
        }
    } else {
        ## Not really tested
        .Call(stopHTTPD)
    	httpdPort <<- 0L
    }
    lockBinding("httpdPort", env)
    invisible(httpdPort)
}

## environment holding potential custom httpd handlers
.httpd.handlers.env <- new.env()
