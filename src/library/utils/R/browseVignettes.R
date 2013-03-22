#  File src/library/utils/R/browseVignettes.R
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



browseVignettes <- function(package = NULL, lib.loc = NULL, all = TRUE)
{
    ## adapted from vignette()
    if (is.null(package)) {
        package <- .packages(all.available = all, lib.loc)
        ## allow for misnamed dirs
        paths <- find.package(package, lib.loc, quiet = TRUE)
    } else paths <- find.package(package, lib.loc)
    paths <- paths[tools:::file_test("-d", file.path(paths, "doc"))]
    vignettes <- lapply(paths, function(dir) {
        tools::list_files_with_type(file.path(dir, "doc"), "vignette")
    })
    names(vignettes) <- basename(paths)
    getVinfo <- function(db) {
        dir <- dirname(dirname(db[1L]))
        entries <- NULL
        if (file.exists(INDEX <- file.path(dir, "Meta", "vignette.rds")))
            entries <- readRDS(INDEX)
        if (NROW(entries) > 0) {
            cbind(Dir = dir,
                  File = basename(entries$File),
                  Title = entries$Title,
                  ## FIXME: test unnecessary once packages are reinstalled
                  R = if (is.null(entries$R)) "" else entries$R,
                  PDF = entries$PDF)[order(entries$Title), , drop=FALSE]
        }
        else NULL
    }
    vinfo <- lapply(vignettes[sapply(vignettes, length) > 0L], getVinfo)
    attr(vinfo, "call") <- sys.call()
    attr(vinfo, "footer") <-
        if (all) ""
        else sprintf(gettext("Use <code> %s </code> \n to list the vignettes in all <strong>available</strong> packages."),
                     "browseVignettes(all = TRUE)")
    class(vinfo) <- "browseVignettes"
    return(vinfo)
}



print.browseVignettes <- function(x, ...)
{
    if (length(x) == 0L) {
        message(gettextf("No vignettes found by %s",
                         paste(deparse(attr(x, "call")), collapse=" ")),
                domain = NA)
        return(invisible(x))
    }

    oneLink <- function(s) {
        if (length(s) == 0L) return(character(0L))
        title <- s[, "Title"]
        if (tools:::httpdPort > 0L)
            prefix <- sprintf("/library/%s/doc", pkg)
        else
            prefix <- sprintf("file://%s/doc", s[, "Dir"])
        src <- s[, "File"]
        pdf <- s[, "PDF"]
        rcode <- s[, "R"]
        pdfext <- sub("^.*\\.", "", pdf)
        sprintf("  <li>%s  -  \n    %s  \n    %s  \n    %s \n  </li>\n",
                title,
                ifelse(nzchar(pdf),
                       sprintf("<a href='%s/%s'>%s</a>&nbsp;", 
                               prefix, pdf, toupper(pdfext)),
                       ""),
		sprintf("<a href='%s/%s'>source</a>&nbsp;", prefix, src),
		ifelse(nzchar(rcode),
                       sprintf("<a href='%s/%s'>R code</a>&nbsp;", prefix, rcode),
                       ""))
    }
    
    if (tools:::httpdPort == 0L)
        tools::startDynamicHelp()

    file <- tempfile("Rvig.", fileext=".html")
    sink(file)
    if (tools:::httpdPort > 0)
    	css_file <- "/doc/html/R.css"
    else
    	css_file <- file.path(R.home("doc"), "html", "R.css")
    cat(sprintf("<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>
<html>
<head>
<title>R Vignettes</title>
<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>
<link rel='stylesheet' type='text/css' href='%s'>
</head>
<body>\n", css_file))
    cat(sprintf("<h2>Vignettes found by <code><q>%s</q></code></h2>",
                paste(deparse(attr(x, "call")), collapse=" ")))
    cat("<div class=\"vignettes\">")
    for (pkg in names(x))
    {
        cat(sprintf("<h3>Vignettes in package <code>%s</code></h3>\n", pkg))
        cat("<ul>\n")
        links <- oneLink(x[[pkg]])
        cat(paste(links), collapse = "\n")
        cat("\n</ul>\n")
    }
    cat("</div>")
    cat(sprintf("<hr/><p>%s</p>", attr(x, "footer")))
    cat("</body></html>\n")
    sink()
    ## the first two don't work on Windows with browser=NULL.
    ## browseURL(URLencode(sprintf("file://%s", file)))
    ## browseURL(URLencode(file))
    if (tools:::httpdPort > 0L)
	browseURL(sprintf("http://127.0.0.1:%d/session/%s", tools:::httpdPort, basename(file)))
    else
    	browseURL(sprintf("file://%s", file))
    ## browseURL(file)
    invisible(x)
}
