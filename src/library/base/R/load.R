#  File src/library/base/R/load.R
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

load <-
    function (file, envir = parent.frame())
{
    if (is.character(file)) {
        ## files are allowed to be of an earlier format
        ## gzfile can open gzip, bzip2, xz and uncompressed files.
        con <- gzfile(file)
        on.exit(close(con))
        magic <- readChar(con, 5L, useBytes = TRUE)
        if (!grepl("RD[AX]2\n", magic)) {
            ## a check while we still know the args
            if(grepl("RD[ABX][12]\r", magic))
                stop("input has been corrupted, with LF replaced by CR")
            ## Not a version 2 magic number, so try the old way.
            warning(gettextf("file '%s' has magic number '%s'\n   Use of save versions prior to 2 is deprecated",
                             basename(file), gsub("[\n\r]*", "", magic)),
                    domain = NA, call. = FALSE)
            return(.Internal(load(file, envir)))
        }
    } else if (inherits(file, "connection")) {
        con <- if(inherits(file, "gzfile")) file else gzcon(file)
    } else stop("bad 'file' argument")

    .Internal(loadFromConn2(con, envir))
}

save <- function(..., list = character(0L),
                 file = stop("'file' must be specified"),
                 ascii = FALSE, version = NULL, envir = parent.frame(),
                 compress = !ascii, compression_level,
                 eval.promises = TRUE, precheck = TRUE)
{
    opts <- getOption("save.defaults")
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(version)) version <- opts$version
    if (!is.null(version) && version < 2)
        warning("Use of save versions prior to 2 is deprecated")

    names <- as.character( substitute( list(...)))[-1L]
    list<- c(list, names)
    if (! is.null(version) && version == 1)
        invisible(.Internal(save(list, file, ascii, version, envir,
                                 eval.promises)))
    else {
        if (precheck) {
            ## check for existence of objects before opening connection
            ## (and e.g. clobering file)
            ok <- unlist(lapply(list, exists, envir=envir))
            if(!all(ok)) {
                n <- sum(!ok)
                stop(sprintf(ngettext(n,
                                      "object %s not found",
                                      "objects %s not found"
                                      ),
                             paste(sQuote(list[!ok]), collapse = ", ")
                             ), domain = NA)
            }
        }
        if (is.character(file)) {
            if (file == "") stop("'file' must be non-empty string")
            con <- if (identical(compress, "bzip2")) {
                if (!missing(compression_level))
                    bzfile(file, "wb", compression = compression_level)
                else bzfile(file, "wb")
            } else if (identical(compress, "xz")) {
                if (!missing(compression_level))
                    xzfile(file, "wb", compression = compression_level)
                else xzfile(file, "wb", compression = 9)
            } else if (identical(compress, "gzip") || compress) {
                if (!missing(compression_level))
                    gzfile(file, "wb", compression = compression_level)
                else gzfile(file, "wb")
            } else file(file, "wb")
            on.exit(close(con))
        }
        else if (inherits(file, "connection"))
            con <- file
        else stop("bad file argument")
        if(isOpen(con) && summary(con)$text != "binary")
            stop("can only save to a binary connection")
        invisible(.Internal(saveToConn(list, con, ascii, version, envir,
                                       eval.promises)))
    }
}

save.image <- function (file = ".RData", version = NULL, ascii = FALSE,
                        compress = !ascii, safe = TRUE)
{
    if (! is.character(file) || file == "")
        stop("'file' must be non-empty string")

    opts <- getOption("save.image.defaults")
    if(is.null(opts)) opts <- getOption("save.defaults")

    if (missing(safe) && ! is.null(opts$safe))
        safe <- opts$safe
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(version)) version <- opts$version

    if (safe) {
        ## find a temporary file name in the same directory so we can
        ## rename it to the final output file on success
        outfile <- paste(file, "Tmp", sep = "")
        i <- 0
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste(file, "Tmp", i, sep = "")
        }
    }
    else outfile <- file

    on.exit(file.remove(outfile))
    save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = outfile,
         version = version, ascii = ascii, compress = compress,
         envir = .GlobalEnv, precheck = FALSE)
    if (safe)
        if (! file.rename(outfile, file)) {
            on.exit()
            stop("image could not be renamed and is left in ", outfile)
        }
    on.exit()
}

sys.load.image <- function(name, quiet)
{
    if (file.exists(name)) {
        load(name, envir = .GlobalEnv)
        if (! quiet)
	    message("[Previously saved workspace restored]", "\n")
    }
}

sys.save.image <- function(name)
{
    ## Ensure that there is a reasonable chance that we can open a
    ## connection.
    closeAllConnections()
    save.image(name)
}

findPackageEnv <- function(info)
{
    if(info %in% search()) return(as.environment(info))
    message(gettextf("Attempting to load the environment '%s'", info),
            domain = NA)
    pkg <- substr(info, 9L, 1000L)
    if(require(pkg, character.only=TRUE, quietly = TRUE))
        return(as.environment(info))
    message("not found: using .GlobalEnv instead")
    return(.GlobalEnv)
}
