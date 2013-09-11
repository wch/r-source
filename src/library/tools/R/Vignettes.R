#  File src/library/tools/R/Vignettes.R
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

vignette_is_tex <- function(file, ...) {
    (regexpr("[.]tex$", file, ignore.case = TRUE) != -1L)
}

# Infers the vignette type (PDF or HTML) from the filename of the
# final vignette product.
vignette_type <- function(file) {
    ext <- tolower(file_ext(file))
    type <- c(pdf="PDF", html="HTML")[ext]
    if (is.na(type))
        stop("Vignette product ", sQuote(file), " does not have a known filename extension (", paste(sQuote(names(type)), collapse=", "), ")")
    unname(type)
}

# Locates the vignette weave, tangle and texi2pdf product(s) based on the
# vignette name.   All such products must have the name as their filename
# prefix (i.e. "^<name>").
# For weave, final = TRUE will look for <name>.pdf and <name>.pdf, whereas
# with final = FALSE it also looks for <name>.tex (if <name>.pdf is also
# found, it will be returned).  For tangle, main = TRUE will look <name>.R,
# whereas main = FALSE will look for <name><anything>*.R.
# For texipdf, <name>.pdf is located.
find_vignette_product <-
    function(name, by = c("weave", "tangle", "texi2pdf"),
             final = FALSE, main = TRUE, dir = ".", engine, ...)
{
    stopifnot(length(name) == 1L)
    by <- match.arg(by)
    stopifnot(file_test("-d", dir))

    if (by == "weave") {
        if (final)
            exts <- c("pdf", "html")
        else
            exts <- c("pdf", "html", "tex")
    } else if (by == "tangle") {
        exts <- c("r", "s")
    } else if (by == "texi2pdf") {
        exts <- "pdf"
    }
    exts <- c(exts, toupper(exts))
    pattern1 <- sprintf("^%s[.](%s)$", name, paste(exts, collapse = "|"))
    output0 <- list.files(path = dir, all.files = FALSE, full.names = FALSE, no..=TRUE)
    output0 <- output0[file_test("-f", file.path(dir, output0))]
    output <- grep(pattern1, output0, value = TRUE)
    # If main is FALSE, we want to find all other files with related names.  We make sure
    # that the main file is in position 1.
    # FIXME:  we should check a timestamp or something to see that these were produced by tangling
    #	      for the "name" vignette, they aren't just coincidentally similar names.
    if (!main) {
	pattern2 <- sprintf("^%s.*[.](%s)$", name, paste(exts, collapse = "|"))
	output2 <- grep(pattern2, output0, value = TRUE)
	output <- c(output, setdiff(output2, output))
    }

    if (by == "weave") {
        if (length(output) == 0L)
            stop("Failed to locate the ", sQuote(by), " output file (by engine ", sQuote(sprintf("%s::%s", engine$package, engine$name)), ") for vignette with name ", sQuote(name), ". The following files exists in directory ", sQuote(dir), ": ", paste(sQuote(output0), collapse=", "))
        if (length(output) > 1L) {
            if (final)
                stop("Located more than one ", sQuote(by), " output file (by engine ", sQuote(sprintf("%s::%s", engine$package, engine$name)), ") for vignette with name ", sQuote(name), ": ", paste(sQuote(output), collapse=", "))
            # If weave produced a TeX and then a PDF without cleaning out
            # the TeX, consider PDF as the weave product
            idxs <- match(tolower(file_ext(output)), exts)
            output <- output[order(idxs)][1L]
            stopifnot(length(output) == 1L)
        }
    } else if (by == "tangle") {
        if (main)
            stopifnot(length(output) <= 1L)
    } else if (by == "texi2pdf") {
        if (length(output) == 0L)
            stop("Failed to locate the ", sQuote(by), " output file (by engine ", sQuote(sprintf("%s::%s", engine$package, engine$name)), ") for vignette with name ", sQuote(name), ". The following files exists in directory ", sQuote(dir), ": ", paste(sQuote(output0), collapse=", "))
        if (length(output) > 1L)
            stop("Located more than one ", sQuote(by), " output file (by engine ", sQuote(sprintf("%s::%s", engine$package, engine$name)), ") for vignette with name ", sQuote(name), ": ", paste(sQuote(output), collapse=", "))
    }

    if (length(output) > 0L) {
        if (dir == ".")
            output <- basename(output)
        else
            output <- file.path(dir, output)
    } else {
        output <- NULL
    }

    output
}



### * checkVignettes
###
### Run a tangle+source and a weave on all vignettes of a package.

checkVignettes <-
function(package, dir, lib.loc = NULL,
         tangle = TRUE, weave = TRUE, latex = FALSE,
         workdir = c("tmp", "src", "cur"),
         keepfiles = FALSE)
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc)
    if(is.null(vigns)) return(NULL)

    workdir <- match.arg(workdir)
    wd <- getwd()
    if (is.null(wd))
        stop("current working directory cannot be ascertained")
    if(workdir == "tmp") {
        tmpd <- tempfile("Sweave")   ## <= Rename?
        if(!dir.create(tmpd))
            stop(gettextf("unable to create temp directory %s ", sQuote(tmpd)),
                 domain = NA)
        setwd(tmpd)
    }
    else {
        keepfiles <- TRUE
        if(workdir == "src") setwd(vigns$dir)
    }

    on.exit({
        setwd(wd)
        if(!keepfiles) unlink(tmpd, recursive = TRUE)
    })

    file.create(".check.timestamp")
    result <- list(tangle = list(), weave = list(),
                   source = list(), latex = list())

    loadVignetteBuilder(vigns$pkgdir)

    startdir <- getwd()
    for(i in seq_along(vigns$docs)) {
        file <- vigns$docs[i]
        file <- basename(file)
        name <- vigns$names[i]
    	engine <- vignetteEngine(vigns$engines[i])

        if(tangle) {
            message("  Running ", sQuote(file))
            .eval_with_capture({
                result$tangle[[file]] <- tryCatch({
                    engine$tangle(file, quiet = TRUE)
                    setwd(startdir) # in case a vignette changes the working dir
                    find_vignette_product(name, by = "tangle", main = FALSE, engine = engine)
                }, error = function(e) e)
            })
        }
        if(weave) {
            setwd(startdir) # in case a vignette changes the working dir then errored out
            .eval_with_capture({
                result$weave[[file]] <- tryCatch({
                    engine$weave(file, quiet = TRUE)
                    setwd(startdir)
                    find_vignette_product(name, by = "weave", engine = engine)
                }, error = function(e) e)
            })
        }
        setwd(startdir) # in case a vignette changes the working dir then errored out
    }

    # Assert that output files were not overwritten
    for (name in c("weave", "tangle")) {
        resultsT <- result[[name]]
        if (length(resultsT) <= 1L)
            next

        for (i in 1L:(length(resultsT)-1L)) {
            outputsI <- resultsT[[i]]
            if (inherits(outputsI, "error"))
                next;
            outputsI <- normalizePath(outputsI)

            for (j in (i+1L):length(resultsT)) {
                 outputsJ <- resultsT[[j]]
                 if (inherits(outputsJ, "error"))
                     next;
                 outputsJ <- normalizePath(outputsJ)
                 bad <- intersect(outputsJ, outputsI)
                 if (length(bad) > 0L) {
                     stop("Vignette ", sQuote(basename(names(resultsT)[j])), " overwrites the following ", sQuote(name), " output by vignette ", sQuote(basename(names(resultsT)[i])), ": ", paste(basename(bad), collapse=", "))
                 }
            }
        }
    }

    if(tangle) {
        ## Tangling can create several source files if splitting is on,
        ## and these can be .R or .S (at least).  However, there is
        ## no guarantee that running them in alphabetical order in a
        ## session will work -- with named chunks it normally will not.
        cwd <- getwd()
        if (is.null(cwd))
            stop("current working directory cannot be ascertained")
        for(i in seq_along(result$tangle)) {
            sources <- result$tangle[[i]]
            if (inherits(sources, "error"))
                next
            sources <- sources[file_test("-nt", sources, ".check.timestamp")]
            for(file in sources) {
                .eval_with_capture({
                    result$source[[file]] <- tryCatch({
                        source(file)
                    }, error = function(e) e)
                })
                setwd(startdir)
            }
        }
    }

    if(weave && latex) {
        if(!("Makefile" %in% list.files(vigns$dir))) {
            ## <NOTE>
            ## This used to run texi2dvi on *all* vignettes, including
            ## the ones already known from the above to give trouble.
            ## In addition, texi2dvi errors were not caught, so that in
            ## particular the results of the previous QC analysis were
            ## *not* returned in case of such errors ...
            ## Hence, let us
            ## * Only run texi2dvi() on previously unproblematic vignettes
            ## * Catch texi2dvi() errors similar to the above.
            ## * Do *not* immediately show texi2dvi() output as part of
            ##   running checkVignettes().
            ## (For the future, maybe keep this output and provide it as
            ## additional diagnostics ...)
            ## </NOTE>
            for (i in seq_along(result$weave)) {
                file <- names(result$weave)[i]
                output <- result$weave[i]
                if (inherits(output, "error"))
                    next
                if (!vignette_is_tex(output))
                    next
                .eval_with_capture({
                    result$latex[[file]] <- tryCatch({
                       texi2pdf(file = output, clean = FALSE, quiet = TRUE)
                       find_vignette_product(name, by = "texi2pdf", engine = engine)
                    }, error = function(e) e)
                })
            }
        }
    }

    # Cleanup results
    for (name in c("tangle", "weave", "source", "latex")) {
        resultsT <- result[[name]]
        resultsT <- lapply(resultsT, FUN = function(res) {
          if (inherits(res, "error"))
              conditionMessage(res)
          else
              NULL
        })
        resultsT <- resultsT[!sapply(resultsT, FUN = is.null)]
        result[[name]] <- resultsT
    }

    file.remove(".check.timestamp")
    class(result) <- "checkVignettes"
    result
}

print.checkVignettes <-
function(x, ...)
{
    mycat <- function(y, title) {
        if(length(y)){
            cat("\n", title, "\n\n", sep = "")
            for(k in seq_along(y)) {
                cat("File", names(y)[k], ":\n")
                cat(as.character(y[[k]]), "\n")
            }
        }
    }

    mycat(x$tangle, "*** Tangle Errors ***")
    mycat(x$source, "*** Source Errors ***")
    mycat(x$weave,  "*** Weave Errors ***")
    mycat(x$latex,  "*** PDFLaTeX Errors ***")

    invisible(x)
}

### get the engine from a file

getVignetteEngine <- function(filename, lines = readLines(filename, warn=FALSE)) {
    c(.get_vignette_metadata(lines, "Engine"), "utils::Sweave")[1L]
}

### * engineMatches
###
### does the engine from a vignette match one of the registered ones?
###
engineMatches <- function(regengine, vigengine) {
    if (!grepl("::", vigengine))
	regengine <- sub("^.*::", "", regengine)
    regengine == vigengine
}

### * pkgVignettes
###
### Get an object of class pkgVignettes which contains a list of
### vignette source files, the registered vignette engine for
### each of them, and the name of the directory which contains them.

pkgVignettes <-
function(package, dir, subdirs = NULL, lib.loc = NULL, output = FALSE, source = FALSE)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
    }
    if(missing(dir))
	stop("you must specify 'package' or 'dir'")
    ## Using sources from directory @code{dir} ...
    if(!file_test("-d", dir))
	stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else {
	# This code is for 3.0.x only:  3.1.0 will assume all vignettes are in the vignettes directory
	dir <- file_path_as_absolute(dir)
	if (is.null(subdirs))
	    subdirs <- c("vignettes", if (missing(package)) file.path("inst", "doc") else file.path("doc") )
	docdirs <- file.path(dir, subdirs)
	docdirs <- docdirs[file_test("-d", docdirs)]
    }

    if(!length(docdirs)) return(NULL)

    # Locate all vignette files
    buildPkgs <- loadVignetteBuilder(dir, mustwork = FALSE)
    engineList <- vignetteEngine(package = buildPkgs)

    docs <- names <- engines <- patterns <- character()
    allFiles <- list.files(docdirs, all.files = FALSE, full.names = TRUE)
    if (length(allFiles) > 0L) {
        for (name in names(engineList)) {
            engine <- engineList[[name]]
            patternsT <- engine$pattern
            for (pattern in patternsT) {
                idxs <- grep(pattern, allFiles)
		docsT <- allFiles[idxs]
		keep <- logical(length(docsT))
		for (i in seq_along(docsT)) 
		    keep[i] <- engineMatches(name, getVignetteEngine(docsT[i]))
		idxs  <- idxs[keep]
                nidxs <- length(idxs)
                if (nidxs > 0L) {
                    if (is.function(engine$weave)) {
                        docsT <- allFiles[idxs]
                        docs <- c(docs, docsT)
                        names <- c(names, gsub(pattern, "", basename(docsT)))
                        engines <- c(engines, rep(name, times = nidxs))
                        patterns <- c(patterns, rep(pattern, times = nidxs))
                    }
                    allFiles <- allFiles[-idxs]
                    if (length(allFiles) == 0L)
                        break
                }
            }
        }
    }
    # In 3.0.x, we might see the same vignette in both vignettes and inst/doc.  Remove the inst/doc one
    if (length(docdirs) > 1) {
	inVignettes <- docdirs[1]  == substr(docs, 1L, nchar(docdirs[1]))
	basenames <- basename(docs)
        dup <- !inVignettes & (basenames %in% basenames[inVignettes])
	docs <- docs[!dup]
	names <- names[!dup]
	engines <- engines[!dup]
	patterns <- patterns[!dup]
    }

    # Assert
    stopifnot(length(names) == length(docs))
    stopifnot(length(engines) == length(docs))
    stopifnot(length(patterns) == length(docs))
    stopifnot(!any(duplicated(docs)))

    z <- list(docs=docs, names=names, engines=engines, patterns=patterns, dir=docdirs[1L], pkgdir=dir)

    if (output) {
        outputs <- character(length(docs))
        for (i in seq_along(docs)) {
            file <- docs[i]
            name <- names[i]
            outputI <- find_vignette_product(name, by = "weave", dir = dirname(file), engine = engine)
            outputs[i] <- outputI
        }
        z$outputs <- outputs
    }

    if (source) {
        sources <- list()
        for (i in seq_along(docs)) {
            file <- docs[i]
            name <- names[i]
            sourcesI <- find_vignette_product(name, by = "tangle", main = FALSE, dir = dirname(file), engine = engine)
            sources[[file]] <- sourcesI
        }
        z$sources <- sources
    }

    class(z) <- "pkgVignettes"
    z
}


### * buildVignettes
###
### Run a weave and pdflatex on all vignettes of a package and try to
### remove all temporary files that were created.

buildVignettes <-
function(package, dir, lib.loc = NULL, quiet = TRUE, clean = TRUE, tangle = FALSE)
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc)
    if(is.null(vigns)) return(invisible())

    ## 3.0.x:  We'll only build the ones in the vigns$dir directory.  In later versions
    ## there won't be any others
    dobuild <- vigns$dir == substr(vigns$docs, 1L, nchar(vigns$dir))
    ## Assert that duplicated vignette names do not exist, e.g.
    ## 'vig' and 'vig' from 'vig.Rnw' and 'vig.Snw'.
    dups <- duplicated(vigns$names)
    if (any(dups[dobuild])) {
        names <- unique(vigns$names[dups])
        docs <- sort(basename(vigns$docs[vigns$names %in% names]))
        stop("Detected vignette source files (", paste(sQuote(docs), collapse=", "), ") with shared names (", paste(sQuote(names), collapse=", "), ") and therefore risking overwriting each others output files")
    }

    ## unset SWEAVE_STYLEPATH_DEFAULT here to avoid problems
    Sys.unsetenv("SWEAVE_STYLEPATH_DEFAULT")

    op <- options(warn = 1) # we run vignettes in this process
    wd <- getwd()
    if (is.null(wd))
        stop("current working directory cannot be ascertained")
    on.exit({
        setwd(wd)
        options(op)
    })

    setwd(vigns$dir)

    ## FIXME: should this recurse into subdirs?
    origfiles <- list.files(all.files = TRUE)

    ## Note, as from 2.13.0, only this case
    have.makefile <- "Makefile" %in% origfiles
    WINDOWS <- .Platform$OS.type == "windows"

    file.create(".build.timestamp")

    loadVignetteBuilder(vigns$pkgdir)
    outputs <- NULL
    sourceList <- list()
    startdir <- getwd()
    for(i in seq_along(vigns$docs)) {
        if (!dobuild[i]) next
        file <- vigns$docs[i]
        file <- basename(file)
        name <- vigns$names[i]
    	engine <- vignetteEngine(vigns$engine[i])

        output <- tryCatch({
            engine$weave(file, quiet = quiet)
            setwd(startdir)
            find_vignette_product(name, by = "weave", engine = engine)
        }, error = function(e) {
            stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",
                 file, conditionMessage(e)), domain = NA, call. = FALSE)
        })

        ## This can fail if run in a directory whose path contains spaces.
        if(!have.makefile && vignette_is_tex(output)) {
            texi2pdf(file = output, clean = FALSE, quiet = quiet)
            output <- find_vignette_product(name, by = "texi2pdf", engine = engine)
        }
        outputs[file] <- output

        if (tangle) {  # This is set for all engines as of 3.0.2
            output <- tryCatch({
                engine$tangle(file, quiet = quiet)
                setwd(startdir)
                find_vignette_product(name, by = "tangle", main = FALSE, engine = engine)
            }, error = function(e) {
                stop(gettextf("tangling vignette '%s' failed with diagnostics:\n%s",
                     file, conditionMessage(e)), domain = NA, call. = FALSE)
            })
            sourceList[[file]] <- output
        }
    }

    if(have.makefile) {
        if (WINDOWS) {
            ## Some people have *assumed* that R_HOME uses / in Makefiles
            ## Spaces in paths might still cause trouble.
            rhome <- chartr("\\", "/", R.home())
            Sys.setenv(R_HOME = rhome)
        }
    	make <- Sys.getenv("MAKE", "make")
        if(!nzchar(make)) make <- "make"
        yy <- system(make)
        if(yy > 0) stop("running 'make' failed")
        ## See if Makefile has a clean: target, and if so run it.
        if(clean &&
           any(grepl("^clean:", readLines("Makefile", warn = FALSE))))
            system(paste(make, "clean"))
    } else {
        ## Badly-written vignettes open a pdf() device on Rplots.pdf and
        ## fail to close it.
        graphics.off()

        keep <- c(outputs, sourceList)
        if(clean) {
            f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), keep)
            newer <- file_test("-nt", f, ".build.timestamp")
            ## some packages, e.g. SOAR, create directories
            unlink(f[newer], recursive = TRUE)
        }
        f <- setdiff(list.files(all.files = TRUE, no.. = TRUE),
                     c(keep, origfiles))
        f <- f[file_test("-f", f)]
        file.remove(f)
    }

    # Assert
    stopifnot(length(outputs) == length(vigns$docs[dobuild]))

    vigns$outputs <- rep("", length(vigns$names))
    names(vigns$outputs) <- vigns$docs
    vigns$outputs[names(outputs)] <- outputs
    vigns$sources <- sourceList

    if(file.exists(".build.timestamp")) file.remove(".build.timestamp")
    ## Might have been in origfiles ...

    invisible(vigns)
}

### * .getVignetteEncoding

getVignetteEncoding <-  function(file, ...)
{
    lines <- readLines(file, warn = FALSE)
    .getVignetteEncoding(lines, ...)
}

.getVignetteEncoding <- function(lines, convert = FALSE)
{
    ## Look for input enc lines using inputenc or inputenx
    ## Note, multiple encodings are excluded.

    poss <-
        grep("^[[:space:]]*\\\\usepackage\\[([[:alnum:]]+)\\]\\{inputen[cx]\\}",
             lines, useBytes = TRUE)
    ## Check it is in the preamble
    start <- grep("^[[:space:]]*\\\\begin\\{document\\}",
                  lines, useBytes = TRUE)
    if(length(start)) poss <- poss[poss < start[1L]]
    if(!length(poss)) {
        asc <- iconv(lines, "latin1", "ASCII")
        ind <- is.na(asc) | asc != lines
        if(any(ind)) return("non-ASCII")
        return("") # or "ASCII"
    }
    poss <- lines[poss[1L]]
    res <- gsub("^[[:space:]]*\\\\usepackage\\[([[:alnum:]]+)\\].*", "\\1",
                poss) # This line should be ASCII.
    if (convert) {
        ## see Rd2latex.R.
        ## Currently utf8, utf8x, latin1, latin9 and ansinew are in use.
        switch(res,
               "utf8" =, "utf8x" = "UTF-8",
               "latin1" =, "iso-8859-1" = "latin1",
               "latin2" =, "iso-8859-2" = "latin2",
               "latin9" =, "iso-8859-15" = "latin-9", # only form known to GNU libiconv
               "latin10" =, "iso-8859-16" = "latin10",
               "cyrillic" =, "iso-8859-5" =  "ISO-8859-5", # inputenx
               "koi8-r" =  "KOI8-R", # inputenx
               "arabic" = "ISO-8859-6", # Not clear next 3 are known to latex
               "greek" =, "iso-8859-7" = "ISO-8859-7",
               "hebrew" =, "iso-8859-8" = "ISO-8859-8",
               "ansinew" = "CP1252",
               "applemac" = "macroman",
               ## assume these only get used on Windows
               "cp1250" = "CP1250",
               "cp1252" = "CP1252",
               "cp1257" = "CP1257",
               "unknown")
    } else res
}

### * .build_vignette_index

.get_vignette_metadata <-
function(lines, tag)
{
    meta_RE <- paste("[[:space:]]*%+[[:space:]]*\\\\Vignette", tag,
                     "\\{([^}]*)\\}", sep = "")
    meta <- grep(meta_RE, lines, value = TRUE, useBytes = TRUE)
    .strip_whitespace(gsub(meta_RE, "\\1", meta))
}

vignetteInfo <-
function(file)
{
    lines <- readLines(file, warn = FALSE)

    ## <FIXME>
    ## Can only proceed with lines which are valid in the current locale.
    ## Unfortunately, vignette encodings are a mess: package encodings
    ## might apply, but be overridden by \inputencoding commands.
    ## For now, assume that vignette metadata occur in all ASCII lines.
    ## (Could also iconv() using sub = "byte".)
    lines[is.na(nchar(lines, "c", TRUE))] <- ""
    ## </FIXME>

    ## \VignetteIndexEntry
    title <- c(.get_vignette_metadata(lines, "IndexEntry"), "")[1L]
    ## \VignetteDepends
    depends <- .get_vignette_metadata(lines, "Depends")
    if(length(depends))
        depends <- unlist(strsplit(depends[1L], ", *"))
    ## \VignetteKeyword and old-style \VignetteKeywords
    keywords <- .get_vignette_metadata(lines, "Keywords")
    keywords <- if(!length(keywords)) {
        ## No old-style \VignetteKeywords entries found.
        .get_vignette_metadata(lines, "Keyword")
    } else unlist(strsplit(keywords[1L], ", *"))
    ## no point in recording the file path since this is called on
    ## package installation.
    engine <- getVignetteEngine(lines=lines)
    list(file = basename(file), title = title, depends = depends,
         keywords = keywords, engine = engine)
}

## The below builds vignette indices via 'pkgVignettes' objects.
.build_vignette_index <-
function(vigns)
{
    stopifnot(inherits(vigns, "pkgVignettes"))

    files <- vigns$docs
    names <- vigns$names
    dir <- vigns$dir
    sources <- vigns$sources

    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)

    nvigns <- length(files)
    if(nvigns == 0L) {
        out <- data.frame(File = character(),
                          Title = character(),
                          PDF = character(),
			  R = character(),
                          stringsAsFactors = FALSE)
        out$Depends <- list()
        out$Keywords <- list()
        return(out)
    }

    # Check for duplicated vignette names
    if (any(dups <- duplicated(names))) {
    	dupname <- names[dups][1L]
    	dup <- basename(files[dups][1L])
    	orig <- basename(files[ names == dupname ][1L])
    	stop(gettextf("In '%s' vignettes '%s' and '%s' have the same vignette name",
    		      basename(dirname(dir)), orig, dup),
             domain = NA)
    }

    # Read vignette annotation from vignette source files
    contents <- vector("list", length = nvigns * 5L)
    dim(contents) <- c(nvigns, 5L)
    for(i in seq_along(files))
        contents[i, ] <- vignetteInfo(files[i])
    colnames(contents) <- c("File", "Title", "Depends", "Keywords", "Engine")

    ## This is to cover a temporary package installation
    ## by 'R CMD build' (via 'R CMD INSTALL -l <lib>)
    ## which in case vignettes have not been built.
    outputs <- vigns$outputs
    if (!is.null(outputs)) {
        outputs <- basename(outputs)
    } else {
        outputs <- character(nvigns)
    }

    out <- data.frame(File = unlist(contents[, "File"]),
                      Title = unlist(contents[, "Title"]),
                      PDF = outputs,	# Not necessarily PDF, but name it that for back compatibility
		      R = "",		# May or may not be present
                      row.names = NULL, # avoid trying to compute row
                                        # names
                      stringsAsFactors = FALSE)
    # Optional
    for (i in seq_along(sources))
	if (length(s <- sources[[i]]))
	    out$R[which(names(sources)[i] == files)] <- basename(s[1L])
    out$Depends <- contents[, "Depends"]
    out$Keywords <- contents[, "Keywords"]

    stopifnot(NROW(out) == nvigns)

    out
}

### * .check_vignette_index

.check_vignette_index <-
function(vignetteDir, pkgdir = ".")
{
    dir <- file.path(pkgdir, vignetteDir)
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)

    subdir <- gsub(pkgdir, "", dir, fixed=TRUE)
    vigns <- pkgVignettes(dir = pkgdir, subdirs = subdir)

    vignetteIndex <- .build_vignette_index(vigns)
    badEntries <-
        vignetteIndex[grep("^[[:space:]]*$", vignetteIndex[, "Title"]),
                      "File"]
    class(badEntries) <- "check_vignette_index"
    badEntries
}

print.check_vignette_index <-
function(x, ...)
{
    if(length(x)) {
        writeLines(paste("Vignettes with missing or empty",
                         "\\VignetteIndexEntry:"))
        print(basename(unclass(x)), ...)
    }
    invisible(x)
}


### * .writeVignetteHtmlIndex

## NB SamplerCompare has a .Rnw file which produces no R code.
.writeVignetteHtmlIndex <-
function(pkg, con, vignetteIndex = NULL)
{
    ## FIXME: in principle we could need to set an encoding here
    html <- c(HTMLheader("Vignettes and other documentation"),
              paste0("<h2>Vignettes from package '", pkg,"'</h2>"))

    if(NROW(vignetteIndex) == 0L) { ## NROW(NULL) = 0
        html <-
            c(html,
              "The package contains no vignette meta-information.")
    } else {
    	vignetteIndex <- cbind(Package = pkg, as.matrix(vignetteIndex[,
                               c("File", "Title", "PDF", "R")]))
        html <- c(html, makeVignetteTable(vignetteIndex, depth = 3L))
    }
    otherfiles <- list.files(system.file("doc", package = pkg))
    if(NROW(vignetteIndex))
        otherfiles <- setdiff(otherfiles,
                              c(vignetteIndex[, c("PDF", "File", "R")], "index.html"))
    if (length(otherfiles)) {
    	otherfiles <- ifelse(file.info(system.file(file.path("doc", otherfiles), package=pkg))$isdir,
			     paste0(otherfiles, "/"),
			     otherfiles)
	urls <- paste0('<a href="', otherfiles, '">', otherfiles, '</a>')
        html <- c(html, '<h2>Other files in the <span class="samp">doc</span> directory</h2>',
                  '<table width="100%">',
		  '<col width="24%">',
		  '<col width="50%">',
		  '<col width="24%">',
                  paste0('<tr><td></td><td><span class="samp">',
                         iconv(urls, "", "UTF-8"), "</span></td></tr>"),
                  "</dl>")
    }
    html <- c(html, "</body></html>")
    writeLines(html, con=con)
}

vignetteDepends <-
function(vignette, recursive = TRUE, reduce = TRUE,
         local = TRUE, lib.loc = NULL)
{
    if (length(vignette) != 1L)
        stop("argument 'vignette' must be of length 1")
    if (!nzchar(vignette)) return(invisible()) # lets examples work.
    if (!file.exists(vignette))
        stop(gettextf("file '%s' not found", vignette),
             domain = NA)

    vigDeps <- vignetteInfo(vignette)$depends

    depMtrx <- getVigDepMtrx(vigDeps)
    instPkgs <- utils::installed.packages(lib.loc=lib.loc)
    getDepList(depMtrx, instPkgs, recursive, local, reduce,
               lib.loc)
}

getVigDepMtrx <-
function(vigDeps)
{
    ## Taken almost directly out of 'package.dependencies'
    if (length(vigDeps)) {
        z <- unlist(strsplit(vigDeps, ",", fixed=TRUE))
        z <- sub("^[[:space:]]*(.*)", "\\1", z)
        z <- sub("(.*)[[:space:]]*$", "\\1", z)
        pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
        depMtrx <- cbind(sub(pat, "\\1", z),
                         sub(pat, "\\2", z),
                         NA)
        noversion <- depMtrx[, 1L] == depMtrx[, 2L]
        depMtrx[noversion, 2L] <- NA
        pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
        depMtrx[!noversion, 2:3] <-
            c(sub(pat, "\\1", depMtrx[!noversion, 2L]),
              sub(pat, "\\2", depMtrx[!noversion, 2L]))
        depMtrx
    }
    else
        NA
}

### * .run_one_vignette
### helper for R CMD check

.run_one_vignette <-
function(vig_name, docDir, encoding = "", pkgdir)
{
    ## The idea about encodings here is that Stangle reads the
    ## file, converts on read and outputs in the current encoding.
    ## Then source() can assume the current encoding.
    td <- tempfile()
    dir.create(td)
    file.copy(docDir, td, recursive = TRUE)
    setwd(file.path(td, basename(docDir)))

    subdir <- gsub(pkgdir, "", docDir, fixed=TRUE)
    vigns <- pkgVignettes(dir=pkgdir, subdirs=subdir)
    if (is.null(vigns)) {
       cat("\n  When running vignette ", sQuote(vig_name), ":\n", sep="")
       stop("No vignettes available", call. = FALSE, domain = NA)
    }

    i <- which(basename(vigns$docs) == vig_name)
    if (length(i) == 0L) {
       cat("\n  When running vignette ", sQuote(vig_name), ":\n", sep="")
       stop("No such vignette ", sQuote(vig_name), call. = FALSE, domain = NA)
    }
    stopifnot(length(i) == 1L)

    loadVignetteBuilder(pkgdir)
    file <- vigns$docs[i]
    file <- basename(file)
    name <- vigns$names[i]
    engine <- vignetteEngine(vigns$engine[i])

    output <- tryCatch({
        engine$tangle(file, quiet = TRUE, encoding = encoding)
        find_vignette_product(name, by = "tangle", engine = engine)
    }, error = function(e) {
        cat("\n  When tangling ", sQuote(file), ":\n", sep="")
        stop(conditionMessage(e), call. = FALSE, domain = NA)
    })

    if(length(output) == 1L) {
        res <- tryCatch({
            source(output, echo = TRUE)
        }, error = function(e) {
            cat("\n  When sourcing ", sQuote(output), ":\n", sep="")
            stop(conditionMessage(e), call. = FALSE, domain = NA)
        })
    }

    cat("\n *** Run successfully completed ***\n")
}

vignetteEngine <- local({
    registry <- new.env(parent = emptyenv())

    engineKey <- function(name, package) {
        key <- strsplit(name, split = "::", fixed = TRUE)[[1L]]
        if (length(key) == 1L) {
            key[2L] <- key[1L]
            key[1L] <- package
        } else if (length(key) != 2L) {
            stop("Unsupported engine name ", sQuote(name))
        }
        key
    }

    getEngine <- function(name, package) {
        if (missing(name)) {
            result <- as.list(registry)
            if (length(result) > 0L && !is.null(package)) {
               package <- unique(package)
               pkgs <- sapply(result, function(engine) engine$package)
               keep <- is.element(pkgs, package)
               if (!any(keep)) {
                   stop("None of packages ", paste(sQuote(package), collapse = ", "), " have registered vignette engines")
               }
               result <- result[keep]
               pkgs <- pkgs[keep]
               if (length(package) > 1L) {
                 result <- result[order(match(pkgs, package))]
               }
            }
        } else {
            result <- NULL
            if (is.null(package)) {
                if (name == "Sweave") {
                    key <- engineKey(name, package = "utils")
                } else {
                    key <- engineKey(name)
                }
                name <- paste(key, collapse = "::")
                result <- registry[[name]]
                if (is.null(result))
                    stop("Vignette engine ", sQuote(name), " is not registered")
            } else {
                for (pkg in package) {
                    key <- engineKey(name, pkg)
                    nameT <- paste(key, collapse = "::")
                    result <- registry[[nameT]]
                    if (!is.null(result))
                        break
                }
                if (is.null(result))
                    stop("Vignette engine ", sQuote(name), " is not registered by any of the packages ", paste(sQuote(package), collapse = ", "))
            }

            if (!is.null(package) && !is.element(result$package, package))
                stop("Vignette engine ", sQuote(name), " is not registered by any of the packages ", paste(sQuote(package), collapse = ", "))
        }
        result
    }

    setEngine <- function(name, package, pattern, weave, tangle) {
        key <- engineKey(name, package)
        if (!is.null(package) && key[1L] != package)
            stop("Engine name ", sQuote(name), " and package ", sQuote(package), " do not match")


        rname <- paste(key, collapse = "::")
        if (is.null(weave)) {
            result <- NULL
            if (exists(rname, envir = registry))
                rm(list = rname, envir = registry)
        } else {
            if (!is.function(weave) && is.na(weave)) {
                if (missing(tangle))
                    tangle <- NA
            } else {
                if (!is.function(weave))
                    stop("Argument ", sQuote("weave"), " must be a function and not ", sQuote(class(weave)[1L]))
                if (!is.function(tangle))
                    stop("Argument ", sQuote("tangle"), " must be a function and not ", sQuote(class(tangle)[1L]))
            }
            if (is.null(pattern))
                pattern <- "[.][rRsS](nw|tex)$"
            else if (!is.character(pattern))
                stop("Argument ", sQuote("pattern"), " must be a character vector or NULL and not ", sQuote(class(pattern)[1L]))

            result <- list(name = key[2L], package = key[1L], pattern = pattern, weave = weave, tangle = tangle)
            assign(rname, result, registry)
        }

        result
    }

    setEngine(name = "Sweave", package = "utils", pattern = NULL,
              weave = function(...) utils::Sweave(...),
              tangle = function(...) utils::Stangle(...))


    function(name, weave, tangle, pattern = NULL, package = NULL) {
        if (missing(weave)) { # we're getting the engine
            getEngine(name, package)
        } else { # we're setting a new engine
            if (is.element(name, c("Sweave", "utils::Sweave"))) {
                stop("Cannot change the ", sQuote("Sweave"), " engine or use an engine of that name")
            }
            if (missing(package))
                package <- utils::packageName(parent.frame())
            result <- setEngine(name, package, pattern = pattern, weave = weave, tangle = tangle)
            invisible(result)
        }
    }
})

loadVignetteBuilder <-
function(pkgdir, mustwork = TRUE)
{
    pkgs <- .get_package_metadata(pkgdir)["VignetteBuilder"]
    if (is.na(pkgs))
        pkgs <- NULL
    else if (length(pkgs) > 0L) {
        pkgs <- unlist(strsplit(pkgs, ","))
        pkgs <- gsub('[[:space:]]', '', pkgs)
    }
    pkgs <- unique(c(pkgs, "utils"))

    for (pkg in pkgs) {
        res <- try(loadNamespace(pkg), silent = TRUE)
        if (mustwork && inherits(res, "try-error"))
            stop(gettextf("vignette builder '%s' not found", pkg), domain = NA)
    }
    pkgs
}

# This gets the info for installed packages

getVignetteInfo <- function(package = NULL, lib.loc = NULL, all = TRUE)
{
    if (is.null(package)) {
        package <- .packages(all.available = all, lib.loc)
        ## allow for misnamed dirs
        paths <- find.package(package, lib.loc, quiet = TRUE)
    } else paths <- find.package(package, lib.loc)

    ## Find the directories with a 'doc' subdirectory *possibly*
    ## containing vignettes.

    paths <- paths[file_test("-d", file.path(paths, "doc"))]

    empty <- cbind(Package = character(0),
                   Dir = character(0),
                   Topic = character(0),
                   File = character(0),
                   Title = character(0),
                   R = character(0),
                   PDF = character(0))

    getVinfo <- function(dir) {
        entries <- NULL
        if (file.exists(INDEX <- file.path(dir, "Meta", "vignette.rds")))
            entries <- readRDS(INDEX)
        if (NROW(entries) > 0) {
            # FIXME:  this test is unnecessary?
            if (is.null(entries$R)) R <- rep("", NROW(entries))
            else R <- entries$R
            file <- basename(entries$File)
            pdf <- entries$PDF
            topic <- file_path_sans_ext(ifelse(R == "", ifelse(pdf == "", file, pdf), R))
            cbind(Package = basename(dir),
                  Dir = dir,
                  Topic = topic,
                  File = file,
                  Title = entries$Title,
                  R = R,
                  PDF = pdf)[order(entries$Title), , drop=FALSE]
        }
        else empty
    }

    if (length(paths))
    	do.call(rbind, lapply(paths, getVinfo))
    else
    	empty
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
