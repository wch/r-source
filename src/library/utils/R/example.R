#  File src/library/utils/R/example.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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
	 type = c("console", "html"), echo = TRUE, verbose = getOption("verbose"),
         setRNG = FALSE, ask = getOption("example.ask"),
	 prompt.prefix = abbreviate(topic, 6),
         catch.aborts = FALSE,
	 run.dontrun = FALSE, run.donttest = interactive())
{
    type <- match.arg(type)
    html <- type == "html" ## only two options for now
    if (html) {
        enhancedHTML <- str2logical(Sys.getenv("_R_HELP_ENABLE_ENHANCED_HTML_", "TRUE"))
        ## silently ignore (but note in documentation)
        if (!interactive() || !enhancedHTML || !requireNamespace("knitr", quietly = TRUE))
            html <- FALSE
    }
    if (html) {
        port <- tools::startDynamicHelp(NA)
        if (port <= 0L) html <- FALSE # silently fall back to console output
        else {
            if (!is.null(lib.loc)) lib.loc <- NULL
            browser <- if (.Platform$GUI == "AQUA") {
                           get("aqua.browser", envir = as.environment("tools:RGUI"))
                       } else getOption("browser")
        }
    }
    if (!character.only) {
        topic <- substitute(topic)
        if(!is.character(topic)) topic <- deparse(topic)[1L]
    }
    pkgpaths <- find.package(package, lib.loc, verbose = verbose)
    ## will only return at most one path
    file <- index.search(topic, pkgpaths, firstOnly=TRUE)
                                        # typically nowadays *not* a file
    if(!length(file)) {
	warning(gettextf("no help found for %s", sQuote(topic)), domain = NA)
	return(invisible())
    }
    if(verbose) cat("Found file =", sQuote(file), "\n")
    packagePath <- dirname(dirname(file))
    pkgname <- basename(packagePath)

    ## At this point, we are ready to invoke HTML help if requested
    if (html) {
        query <- if (local) "" else "?local=FALSE"
        browseURL(paste0("http://127.0.0.1:", port,
                         "/library/", pkgname, "/Example/", topic, query),
                  browser)
        return(invisible())
    }
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
	    on.exit(RNGkind(oldRNG[1L], oldRNG[2L], oldRNG[3L]), add = TRUE)
	}
	## set RNG
	if(is.logical(setRNG)) { # i.e. == TRUE: use the same as R CMD check
	    ## see share/R/examples-header.R
	    RNGkind("default", "default", "default")
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
        ## set ask=TRUE for a plotting device used, but do *not* leave it unless it was already
        oldask <- if(.Device != "null device")
                      grDevices::devAskNewPage(ask = TRUE)
                  else
                      getOption("device.ask.default", FALSE)
        on.exit(if(.Device != "null device") grDevices::devAskNewPage(oldask), add = TRUE)
        ## This ensures that any device opened by the examples will
        ## have ask = TRUE set
        op <- options(device.ask.default = TRUE)
        on.exit(options(op), add = TRUE)
    }
    source(tf, local, echo = echo,
             prompt.echo = paste0(prompt.prefix, getOption("prompt")),
           continue.echo = paste0(prompt.prefix, getOption("continue")),
           verbose = verbose, max.deparse.length = Inf, encoding = "UTF-8",
           catch.aborts = catch.aborts,
    	   skip.echo = skips, keep.source=TRUE)
}
