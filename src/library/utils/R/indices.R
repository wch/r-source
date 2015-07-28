#  File src/library/utils/R/indices.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

packageDescription <-
    function(pkg, lib.loc = NULL, fields = NULL, drop = TRUE, encoding = "")
{
    retval <- list()
    if(!is.null(fields)){
        fields <- as.character(fields)
        retval[fields] <- NA
    }

    ## If the NULL default for lib.loc is used,
    ## the loaded packages/namespaces are searched before the libraries.
    pkgpath <-
	if(is.null(lib.loc)) {
	    if(pkg == "base")
		file.path(.Library, "base")
	    else if(isNamespaceLoaded(pkg))
		getNamespaceInfo(pkg, "path")
	    else if((envname <- paste0("package:", pkg)) %in% search()) {
		attr(as.environment(envname), "path")
		## could be NULL if a perverse user has been naming
		## environments to look like packages.
	    }
	}
    if(is.null(pkgpath)) pkgpath <- ""

    if(pkgpath == "") {
        libs <- if(is.null(lib.loc)) .libPaths() else lib.loc
        for(lib in libs)
            if(file.access(file.path(lib, pkg), 5) == 0L) {
                pkgpath <- file.path(lib, pkg)
                break
            }
    }

    if(pkgpath == "") {
        warning(gettextf("no package '%s' was found", pkg), domain = NA)
        return(NA)
    }

    ## New in 2.7.0: look for installed metadata first.
    ## We always need to be able to drop back to the file as this
    ## is used during package installation.

    if(file.exists(file <- file.path(pkgpath, "Meta", "package.rds"))) {
        desc <- readRDS(file)$DESCRIPTION
        if(length(desc) < 1)
            stop(gettextf("metadata of package '%s' is corrupt", pkg),
                 domain = NA)
        desc <- as.list(desc)
    } else if(file.exists(file <- file.path(pkgpath,"DESCRIPTION"))) {
        dcf <- read.dcf(file=file)
        if(NROW(dcf) < 1L)
            stop(gettextf("DESCRIPTION file of package '%s' is corrupt", pkg),
                 domain = NA)
        desc <- as.list(dcf[1,])
    } else file <- ""

    if(nzchar(file)) {
        ## read the Encoding field if any
        enc <- desc[["Encoding"]]
        if(!is.null(enc) && !is.na(encoding)) {
            ## Determine encoding and re-encode if necessary and possible.
            if (missing(encoding) && Sys.getlocale("LC_CTYPE") == "C")
                encoding <- "ASCII//TRANSLIT"
            ## might have an invalid encoding ...
            newdesc <- try(lapply(desc, iconv, from = enc, to = encoding))
            if(!inherits(newdesc, "try-error")) desc <- newdesc
            else
                warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible", call. = FALSE)
        }
        if(!is.null(fields)){
            ok <- names(desc) %in% fields
            retval[names(desc)[ok]] <- desc[ok]
        }
        else
            retval[names(desc)] <- desc
    }

    if((file == "") || (length(retval) == 0)){
        warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", pkg), domain = NA)
        return(NA)
    }

    if(drop & length(fields) == 1L)
        return(retval[[1L]])

    class(retval) <- "packageDescription"
    if(!is.null(fields)) attr(retval, "fields") <- fields
    attr(retval, "file") <- file
    retval
}


print.packageDescription <-
    function(x, abbrCollate = 0.8 * getOption("width"), ...)
{
    xx <- x
    xx[] <- lapply(xx, function(x) if(is.na(x)) "NA" else x)
    if(abbrCollate > 0 && any(names(xx) == "Collate")) {
        ## trim a long "Collate" field -- respecting word boundaries
	wrds <- strsplit(xx$Collate,"[ \n]")[[1L]]
	k <- which.max(cumsum(nchar(wrds)) > abbrCollate) - 1L
	xx$Collate <- paste(c(wrds[seq_len(k)], "....."), collapse=" ")
    }
    write.dcf(as.data.frame.list(xx, optional = TRUE))
    cat("\n-- File:", attr(x, "file"), "\n")
    if(!is.null(attr(x, "fields"))){
        cat("-- Fields read: ")
        cat(attr(x, "fields"), sep = ", ")
        cat("\n")
    }
    invisible(x)
}

# Simple convenience functions

maintainer <- function(pkg)
{
    force(pkg)
    desc <- packageDescription(pkg)
    if(is.list(desc)) gsub("\n", " ", desc$Maintainer, fixed = TRUE)
    else NA_character_
}

packageVersion <- function(pkg, lib.loc = NULL)
{
    res <- suppressWarnings(packageDescription(pkg, lib.loc=lib.loc,
                                               fields = "Version"))
    if (!is.na(res)) package_version(res) else
    stop(gettextf("package %s not found", sQuote(pkg)), domain = NA)
}

## used with firstOnly = TRUE for example()
## used with firstOnly = FALSE in help()
index.search <- function(topic, paths, firstOnly = FALSE)
{
    res <- character()
    for (p in paths) {
        if(file.exists(f <- file.path(p, "help", "aliases.rds")))
            al <- readRDS(f)
        else if(file.exists(f <- file.path(p, "help", "AnIndex"))) {
            ## aliases.rds was introduced before 2.10.0, as can phase this out
            foo <- scan(f, what = list(a="", b=""), sep = "\t", quote = "",
                        na.strings = "", quiet = TRUE)
            al <- structure(foo$b, names = foo$a)
        } else next
        f <- al[topic]
        if(is.na(f)) next
        res <- c(res, file.path(p, "help", f))
        if(firstOnly) break
    }
    res
}

print.packageIQR <- function(x, ...)
{
    db <- x$results
    ## Split according to Package.
    out <- if(nrow(db) > 0L)
	       lapply(split(seq_len(nrow(db)), db[, "Package"]),
		      function(ind) db[ind, c("Item", "Title"), drop = FALSE])
    outFile <- tempfile("RpackageIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(pkg in names(out)) {
        writeLines(paste0(ifelse(first, "", "\n"), x$title,
                          " in package ", sQuote(pkg), ":\n"),
                   outConn)
        writeLines(formatDL(out[[pkg]][, "Item"],
                            out[[pkg]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        writeLines(paste("no", tolower(x$title), "found"))
        if(!is.null(x$footer))
            writeLines(c("", x$footer))
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = paste("R", tolower(x$title)))
    }
    invisible(x)
}
