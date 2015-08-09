#  File src/library/utils/R/example.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

## Examples as from 2.11.0 will always be new-style and hence in UTF-8
example <-
function(topic, package = NULL, lib.loc = NULL,
         character.only = FALSE, give.lines = FALSE, local = FALSE,
	 echo = TRUE, verbose = getOption("verbose"), setRNG = FALSE,
         ask = getOption("example.ask"),
	 prompt.prefix = abbreviate(topic, 6),
	 run.dontrun = FALSE, run.donttest = interactive())
{
    if (!character.only) {
        topic <- substitute(topic)
        if(!is.character(topic)) topic <- deparse(topic)[1L]
    }
    pkgpaths <- find.package(package, lib.loc, verbose = verbose)
    ## will only return at most one path
    file <- index.search(topic, pkgpaths, TRUE)
    if(!length(file)) {
	warning(gettextf("no help found for %s", sQuote(topic)), domain = NA)
	return(invisible())
    }
    packagePath <- dirname(dirname(file))
    pkgname <- basename(packagePath)
    lib <- dirname(packagePath)
    tf <- tempfile("Rex")
    tools::Rd2ex(.getHelpFile(file), tf, commentDontrun = !run.dontrun,
		 commentDonttest = !run.donttest)
    if (!file.exists(tf)) {
	if(give.lines) return(character())
        warning(gettextf("%s has a help file but no examples", sQuote(topic)),
                domain = NA)
        return(invisible())
    }
    on.exit(unlink(tf))
    if(give.lines)
	return(readLines(tf))
    if(pkgname != "base")
        library(pkgname, lib.loc = lib, character.only = TRUE)
    if(!is.logical(setRNG) || setRNG) {
	## save current RNG state:
	if((exists(".Random.seed", envir = .GlobalEnv))) {
	    oldSeed <- get(".Random.seed", envir = .GlobalEnv)
	    on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv),
                    add = TRUE)
	} else {
	    oldRNG <- RNGkind()
	    on.exit(RNGkind(oldRNG[1L], oldRNG[2L]), add = TRUE)
	}
	## set RNG
	if(is.logical(setRNG)) { # i.e. == TRUE: use the same as R CMD check
	    ## see share/R/examples-header.R
	    RNGkind("default", "default")
	    set.seed(1)
	} else eval(setRNG)
    }
    zz <- readLines(tf, n = 1L)
    skips <- 0L
    if (echo) {
	## skip over header
	zcon <- file(tf, open="rt")
	while(length(zz) && !length(grep("^### \\*\\*", zz))) {
	    skips <- skips + 1L
	    zz <- readLines(zcon, n=1L)
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
    source(tf, local, echo = echo,
           prompt.echo = paste0(prompt.prefix, getOption("prompt")),
           continue.echo = paste0(prompt.prefix, getOption("continue")),
           verbose = verbose, max.deparse.length = Inf, encoding = "UTF-8",
    	   skip.echo = skips, keep.source=TRUE)
}
