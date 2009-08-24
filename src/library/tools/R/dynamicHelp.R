httpd <- function(path, query, ...) {
    fileRegexp <- "^/library/([^/]*)/html/([^/]*)\\.html$"
    topicRegexp <- "^/library/([^/]*)/help/([^/]*)$"
    file <- NULL
    if (grepl(topicRegexp, path)) {
    	pkg <- sub(topicRegexp, "\\1", path)
    	if (pkg == "NULL") pkg <- NULL
    	topic <- sub(topicRegexp, "\\2", path)
    	file <- NULL
    	if (!is.null(pkg))
    	    file <- help(topic, package=(pkg), htmlhelp=FALSE, chmhelp=FALSE)
    	if (length(file) == 0)
            file <- help(topic, htmlhelp=FALSE, chmhelp=FALSE, try.all.packages=TRUE)
	if (length(file) == 0) {
	    return(list(payload=paste('<p>No help found for topic ', topic, ' in package ', 
	                         pkg, '.</p>\n',
	                         '<hr><div align="center">[<a href="00Index.html">Index</a>]</div>\n', 
	                         sep="", collapse="")))
	} else if (length(file) == 1) {
	    path <- dirname(dirname(file))
	    file <- paste('../../', basename(path), '/html/', basename(file), '.html', sep='')
	    return(list(payload=paste('Redirect to <a href="', file, '">"', basename(file), '"</a>', sep=''),
	    		"content-type"='text/html',
	    		header=paste('Location: ', file, '\n', sep=''),
	    		"status code"=302L)) # temporary redirect
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
    	if (basename(path) == "00Index.html") 
    	    return(list(file=file.path(system.file("html", package=pkg), "00Index.html")))
    	else
    	    file <- file.path(system.file("help", package=pkg), topic)
    }
    if (!is.null(file)) {
	path <- dirname(file)
	dirpath <- dirname(path)
	pkgname <- basename(dirpath)
	RdDB <- file.path(path, pkgname)
	if(file.exists(paste(RdDB, "rdx", sep="."))) {
	    outfile <- tempfile()
	    temp <- tools::Rd2HTML(tools:::fetchRdDB(RdDB, basename(file)), 
				       out = outfile, package = pkgname,
				       dynamic = TRUE)
	    on.exit(unlink(outfile))
	    return(list(payload=paste(readLines(temp), collapse="\n")))
	} else return(list(file=file))    
    } else {
    	# If we got here, we've followed a link that's not to a man page.
    	file <- file.path(R.home(), path)
        content_type <- ifelse(grepl("css$", path),  "text/css",
                        ifelse(grepl("jpg$", path),  "image/jpeg",
                                                     "text/html"))
        
    	return(list(file=file, "content-type"=content_type))   
    }
}

httpdPort <- NULL

startDynamicHelp <- function() {
   env <- environment(startDynamicHelp)
   unlockBinding("httpdPort", env)
   httpdPort <<- 8080L
   repeat {
       status <- .Internal(startHTTPD("127.0.0.1",httpdPort))
       if (status == 0) break
       httpdPort <<- httpdPort + 1L
   }
   lockBinding("httpdPort", env)
   ## FIXME:  the line below is enough for Windows; what needs to be done on Unix to make this work?
   options(htmlhelp=TRUE)
   invisible(httpdPort)
}

