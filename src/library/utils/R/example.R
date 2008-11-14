#  File src/library/utils/R/example.R
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

example <-
function(topic, package = NULL, lib.loc = NULL, local = FALSE,
	 echo = TRUE, verbose = getOption("verbose"), setRNG = FALSE,
         ask = getOption("example.ask"),
	 prompt.prefix = abbreviate(topic, 6))
{
    topic <- substitute(topic)
    if(!is.character(topic))
	topic <- deparse(topic)[1]
    INDICES <- .find.package(package, lib.loc, verbose = verbose)
    file <- index.search(topic, INDICES, "AnIndex", "R-ex")
    if(file == "") {
	warning(gettextf("no help file found for '%s'", topic), domain = NA)
	return(invisible())
    }
    packagePath <- dirname(dirname(file))
    if(length(file) > 1) {
	packagePath <- packagePath[1]
	warning(gettextf("more than one help file found: using package '%s'",
		basename(packagePath)), domain = NA)
	file <- file[1]
    }
    pkg <- basename(packagePath)
    lib <- dirname(packagePath)
    zfile <- zip.file.extract(file, "Rex.zip")
    if(zfile != file) on.exit(unlink(zfile))
    if(!file.exists(zfile)) {
	warning(gettextf("'%s' has a help file but no examples file", topic),
		domain = NA)
	return(invisible())
    }
    if(pkg != "base")
	library(pkg, lib.loc = lib, character.only = TRUE)
    if(!is.logical(setRNG) || setRNG) {
	## save current RNG state:
	if((exists(".Random.seed", envir = .GlobalEnv))) {
	    oldSeed <- get(".Random.seed", envir = .GlobalEnv)
	    on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv))
	} else {
	    oldRNG <- RNGkind()
	    on.exit(RNGkind(oldRNG[1], oldRNG[2]))
	}
	## set RNG
	if(is.logical(setRNG)) { # i.e. == TRUE: use the same as R CMD check
	    ## see ../../../../share/perl/massage-Examples.pl
	    RNGkind("default", "default")
	    set.seed(1)
	} else eval(setRNG)
    }
    encoding <-
	if(length(enc <- localeToCharset()) > 1)
	    c(enc[-length(enc)], "latin1")
	else ""
    ## peek at the file, but note we can't usefully translate to C.
    zz <- readLines(zfile, n=1)
    if(length(grep("^### Encoding: ", zz)) > 0 &&
       !identical(Sys.getlocale("LC_CTYPE"), "C"))
	encoding <- substring(zz, 15)
    skips <- 0
    if (echo) {
	## skip over header
	zcon <- file(zfile, open="rt")
	while(length(zz) && !length(grep("^### \\*\\*", zz))) {
	    skips <- skips + 1
	    zz <- readLines(zcon, n=1)
	}
	close(zcon)
    }
    if(ask == "default")
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if(ask) {
	if(.Device != "null device") {
	    oldask <- grDevices::devAskNewPage(ask = TRUE)
            if(!oldask) on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
        }
        ## <FIXME>
        ## This ensures that any device opened by the examples will
        ## have ask = TRUE set, but it does not return the device to
        ## the expected 'ask' state if it is left as the current device.
        ## </FIXME>
        op <- options(device.ask.default = TRUE)
        on.exit(options(op), add = TRUE)
    }
    source(zfile, local, echo = echo,
           prompt.echo = paste(prompt.prefix, getOption("prompt"), sep=""),
           continue.echo = paste(prompt.prefix, getOption("continue"), sep=""),
           verbose = verbose, max.deparse.length = Inf, encoding = encoding,
    	   skip.echo = skips, keep.source=TRUE)
}
