#  File src/library/tools/R/Vignettes.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

vignette_is_tex <- function(file, ...) {
    (regexpr("[.]tex$", file, ignore.case = TRUE) != -1L)
}

# Infers the vignette type (PDF or HTML) from the filename of the
# final vignette product.
vignette_type <- function(file) {
    ext <- tolower(file_ext(file))
    type <- c(pdf="PDF", html="HTML")[ext]
    if (is.na(type))
        stop(gettextf("Vignette product %s does not have a known filename extension (%s)",
                      sQuote(file), paste(sQuote(names(type)), collapse=", ")),
             domain = NA)
    unname(type)
}

# Locates the vignette weave, tangle and texi2pdf product(s) based on the
# vignette name.   All such products must have the name as their filename
# prefix (i.e. "^<name>").
# For weave, final = TRUE will look for <name>.pdf and <name>.html, whereas
# with final = FALSE it also looks for <name>.tex (if <name>.pdf is also
# found, it will be returned).  For tangle, main = TRUE will look <name>.R,
# whereas main = FALSE will look for <name><anything>*.R.
# For texipdf, <name>.pdf is located.
find_vignette_product <-
    function(name, by = c("weave", "tangle", "texi2pdf"),
             final = FALSE, main = TRUE, dir = ".", engine, ...)
{
    stopifnot(length(name) == 1L, dir.exists(dir))
    by <- match.arg(by)
    exts <- ## (lower case here):
	switch(by,
	       "weave" = if (final) c("pdf", "html") else c("pdf", "html", "tex"),
	       "tangle" = c("r", "s"),
	       "texi2pdf" = "pdf")

    exts <- c(exts, toupper(exts))
    pattern1 <- sprintf("^%s[.](%s)$", name, paste(exts, collapse = "|"))
    output0 <- list.files(path = dir, all.files = FALSE, full.names = FALSE,
                          no.. = TRUE)
    output0 <- output0[file_test("-f", file.path(dir, output0))]
    output <- grep(pattern1, output0, value = TRUE)
    # If main is FALSE, we want to find all other files with related
    # names.  We make sure that the main file is in position 1.
    # FIXME: we should check a timestamp or something to see that
    #	      these were produced by tangling for the "name" vignette,
    #	      they aren't just coincidentally similar names.
    if (!main) {
	pattern2 <- sprintf("^%s.*[.](%s)$", name, paste(exts, collapse = "|"))
	output2 <- grep(pattern2, output0, value = TRUE)
	output <- c(output, setdiff(output2, output))
    }

    if (by == "weave") {
        if (length(output) == 0L)
            stop(gettextf("Failed to locate the %s output file (by engine %s) for vignette with name %s. The following files exist in directory %s: %s",
                          sQuote(by),
                          sQuote(sprintf("%s::%s", engine$package, engine$name)),
                          sQuote(name), sQuote(dir),
                          paste(sQuote(output0), collapse=", ")),
                 domain = NA)
        if (length(output) > 2L || (final && length(output) > 1L))
            stop(gettextf("Located more than one %s output file (by engine %s) for vignette with name %s: %s", sQuote(by),
                          sQuote(sprintf("%s::%s", engine$package, engine$name)),
                          sQuote(name), paste(sQuote(output), collapse=", ")),
                 domain  = NA)
	# If weave produced a TeX and then a PDF without cleaning out
	# the TeX, consider the newer one (PDF wins a tie) as the weave product
	if (length(output) == 2L) {
	    idxs <- match(tolower(file_ext(output)), exts)
	    output <- output[order(idxs)]
	    if (file_test("-nt", output[2L], output[1L])) output <- output[2L]
	    else output <- output[1L]
        }
    } else if (by == "tangle") {
        if (main)
            stopifnot(length(output) <= 1L)
    } else if (by == "texi2pdf") {
        if (length(output) == 0L)
            stop(gettextf("Failed to locate the %s output file (by engine %s) for vignette with name %s. The following files exist in directory %s: %s",
                          sQuote(by),
                          sQuote(sprintf("%s::%s", engine$package, engine$name)),
                          sQuote(name), sQuote(dir),
                          paste(sQuote(output0), collapse=", ")),
                 domain = NA)
        if (length(output) > 1L)
            stop(gettextf("Located more than one %s output file (by engine %s) for vignette with name %s: %s",
                          sQuote(by),
                          sQuote(sprintf("%s::%s", engine$package, engine$name)),
                          sQuote(name), paste(sQuote(output), collapse=", ")),
                 domain = NA)
    }

    ## return :
    if (length(output) > 0L) {
        if (dir == ".")
            basename(output)
        else
            file.path(dir, output)
    } ## else NULL
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
	enc <- vigns$encodings[i]
        if (enc == "non-ASCII")
            stop(gettextf("Vignette '%s' is non-ASCII but has no declared encoding", name),
                 domain = NA)
        if(tangle) {
            message("  Running ", sQuote(file))
            .eval_with_capture({
                result$tangle[[file]] <- tryCatch({
                    engine$tangle(file, quiet = TRUE, encoding = enc)
                    setwd(startdir) # in case a vignette changes the working dir
                    find_vignette_product(name, by = "tangle", main = FALSE, engine = engine)
                }, error = function(e) e)
            })
        }
        if(weave) {
            setwd(startdir) # in case a vignette changes the working dir then errored out
            .eval_with_capture({
                result$weave[[file]] <- tryCatch({
                    engine$weave(file, quiet = TRUE, encoding = enc)
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
                     stop(gettextf("Vignette %s overwrites the following %s output by vignette %s: %s",
                                   sQuote(basename(names(resultsT)[j])),
                                   sQuote(name),
                                   sQuote(basename(names(resultsT)[i])),
                                   paste(basename(bad), collapse=", ")),
                          domain = NA)
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
        if("Makefile" %notin% list.files(vigns$dir)) {
            ## <NOTE>
            ## This used to run texi2pdf on *all* vignettes, including
            ## the ones already known from the above to give trouble.
            ## In addition, texi2pdf errors were not caught, so that in
            ## particular the results of the previous QC analysis were
            ## *not* returned in case of such errors ...
            ## Hence, let us
            ## * Only run texi2pdf() on previously unproblematic vignettes
            ## * Catch texi2pdf() errors similar to the above.
            ## * Do *not* immediately show texi2pdf() output as part of
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

### A vector of 'subdirs' is allowed for historical reasons but the
### first which exists is used.

pkgVignettes <-
function(package, dir, subdirs = NULL, lib.loc = NULL, output = FALSE,
         source = FALSE, check = FALSE)
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
    if(!dir.exists(dir))
	stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else {
	dir <- file_path_as_absolute(dir)
	if (is.null(subdirs))
	    subdirs <- if (missing(package)) "vignettes" else "doc"
	for (subdir in subdirs) {
	    docdir <- file.path(dir, subdir)
	    if(dir.exists(docdir))
		break
	}
    }

    if(!dir.exists(docdir)) return(NULL)

    # Locate all vignette files
    buildPkgs <- loadVignetteBuilder(dir, mustwork = FALSE)
    engineList <- vignetteEngine(package = buildPkgs)

    docs <- names <- engines <- patterns <- character()
    allFiles <- list.files(docdir, all.files = FALSE, full.names = TRUE)
    exclude <- inRbuildignore(sub(paste0(dir, "/"), "", allFiles, fixed = TRUE), dir)
    allFiles <- allFiles[!exclude]
    
    matchedPattern <- rep.int(FALSE, length(allFiles))
    msg <- character()
    if (length(allFiles) > 0L) {
        for (name in names(engineList)) {
            engine <- engineList[[name]]
            for (pattern in engine$pattern) {
                idxs <- grep(pattern, allFiles)
		matchedPattern[idxs] <- TRUE
		keep <- vapply(allFiles[idxs], function(.d.)
			       engineMatches(name, getVignetteEngine(.d.)), NA)
		if (any(keep)) {
		    idxs <- idxs[keep]
                    if (is.function(engine$weave)) {
                        docsT <- allFiles[idxs]
                        docs <- c(docs, docsT)
                        names <- c(names, gsub(pattern, "", basename(docsT)))
			engines	 <- c(engines,	rep.int(name,	 length(idxs)))
			patterns <- c(patterns, rep.int(pattern, length(idxs)))
                    }
		    matchedPattern <- matchedPattern[-idxs]
                    allFiles <- allFiles[-idxs]
                    if (length(allFiles) == 0L)
                        break
                }
            }
        }
	if (check && any(matchedPattern)) {
            files <- substring(allFiles[matchedPattern], nchar(dir) + 2)
            msg <- c("Files named as vignettes but with no recognized vignette engine:",
                     paste("  ", sQuote(files)),
                     "(Is a VignetteBuilder field missing?)")
        }
    }

    # Assert
    stopifnot(length(names)    == length(docs),
	      length(engines)  == length(docs),
	      length(patterns) == length(docs), !anyDuplicated(docs))

    defaultEncoding <- .get_package_metadata(dir)["Encoding"]
    encodings <- vapply(docs, getVignetteEncoding, "", default = defaultEncoding)

    z <- list(docs=docs, names=names, engines=engines, patterns=patterns, encodings = encodings,
	      dir = docdir, pkgdir = dir, msg = msg)

    if (output) {
        outputs <- character(length(docs))
        for (i in seq_along(docs)) {
            file <- docs[i]
            name <- names[i]
            outputI <- find_vignette_product(name, by = "weave", dir = docdir, engine = engine)
            outputs[i] <- outputI
        }
        z$outputs <- outputs
    }

    if (source) {
        sources <- list()
        for (i in seq_along(docs)) {
            file <- docs[i]
            name <- names[i]
            sourcesI <- find_vignette_product(name, by = "tangle", main = FALSE, dir = docdir, engine = engine)
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
    function(package, dir, lib.loc = NULL, quiet = TRUE, clean = TRUE,
             tangle = FALSE)
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc,
                          check = TRUE)
    if(is.null(vigns)) return(invisible())
    if(length(vigns$msg))
        warning(paste(vigns$msg, collapse = "\n"), domain = NA)

    ## Check that duplicated vignette names do not exist, e.g.
    ## 'vig' and 'vig' from 'vig.Rnw' and 'vig.Snw'.
    dups <- duplicated(vigns$names)
    if (any(dups)) {
        names <- unique(vigns$names[dups])
        docs <- sort(basename(vigns$docs[vigns$names %in% names]))
        stop(gettextf("Detected vignette source files (%s) with shared names (%s) and therefore risking overwriting each others output files",
                      paste(sQuote(docs), collapse = ", "),
                      paste(sQuote(names), collapse = ", ")),
             domain = NA)
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
        file <- basename(vigns$docs[i])
        name <- vigns$names[i]
    	engine <- vignetteEngine(vigns$engines[i])
        enc <- vigns$encodings[i]
        if (enc == "non-ASCII")
            stop(gettextf("Vignette '%s' is non-ASCII but has no declared encoding",
                 file), domain = NA, call. = FALSE)

        output <- tryCatch({
            ## FIXME: run this in a separate process
            engine$weave(file, quiet = quiet, encoding = enc)
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
        outputs <- c(outputs, output)

        if (tangle) {  # This is set for all engines as of 3.0.2
            output <- tryCatch({
                ## FIXME: run this in a separate process
                engine$tangle(file, quiet = quiet, encoding = enc)
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
	   any(startsWith(readLines("Makefile", warn = FALSE), "clean:")))
            system(paste(make, "clean"))
    } else {
        ## Badly-written vignettes open a pdf() device on Rplots.pdf and
        ## fail to close it.
        grDevices::graphics.off()

        keep <- c(outputs, unlist(sourceList))
        if(clean) {
            f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), keep)
            newer <- file_test("-nt", f, ".build.timestamp")
            ## some packages, e.g. SOAR, create directories
            unlink(f[newer], recursive = TRUE)
            f <- setdiff(list.files(all.files = TRUE, no.. = TRUE),
                         c(keep, origfiles))
            f <- f[file_test("-f", f)]
            file.remove(f)
        }
    }

    # Assert
    stopifnot(length(outputs) == length(vigns$docs))

    vigns$outputs <- outputs
    vigns$sources <- sourceList

    if(file.exists(".build.timestamp")) file.remove(".build.timestamp")
    ## Might have been in origfiles ...

    invisible(vigns)
}

### * buildVignette
###
### Run a weave and/or tangle on one vignette and try to
### remove all temporary files that were created.
### Also called from 'R CMD Sweave' via .Sweave() in ../../utils/R/Sweave.R
buildVignette <-
    function(file, dir = ".", weave = TRUE, latex = TRUE, tangle = TRUE,
             quiet = TRUE, clean = TRUE, keep = character(),
             engine = NULL, buildPkg = NULL,
	     encoding = getVignetteEncoding(file), ...)
{
    if (!file_test("-f", file))
	stop(gettextf("file '%s' not found", file), domain = NA)
    if (!dir.exists(dir))
	stop(gettextf("directory '%s' does not exist", dir), domain = NA)

    if (!is.null(buildPkg))
	for (pkg in buildPkg)
	    suppressPackageStartupMessages(loadNamespace(pkg))

    if (is.null(engine))
    # Infer vignette engine from vignette content
	engine <- getVignetteEngine(file)

    # Get the vignette engine
    if (is.character(engine))
	engine <- vignetteEngine(engine, package = buildPkg)

    # Infer the vignette name
    names <- sapply(engine$pattern, FUN = sub, "", file)
    name <- basename(names[(names != file)][1L])

    # A non-matching filename?
    if (is.na(name))
	stop(gettextf("vignette filename '%s' does not match any of the '%s' filename patterns",
		file, paste(engine$package, engine$name, sep="::")),
		domain = NA)

    if (encoding == "non-ASCII")
    	stop(gettextf("Vignette '%s' is non-ASCII but has no declared encoding", name))

    # Set output directory temporarily
    file <- file_path_as_absolute(file)
    olddir <- setwd(dir)
    if (!is.null(olddir)) on.exit(setwd(olddir))

    ## # Record existing files
    ## origfiles <- list.files(all.files = TRUE)
    if (is.na(clean) || clean) {
	file.create(".build.timestamp")
    }

    tdir <- getwd()# if 'dir' was relative, resetting to tdir will work
    output <- NULL

    # Weave
    final <- if (weave) {
	engine$weave(file, quiet = quiet, encoding = encoding, ...)
	setwd(tdir)  # In case weave/vignette changed it
	output <- find_vignette_product(name, by = "weave", engine = engine)

	# Compile TeX to PDF?
	if(latex && vignette_is_tex(output)) {
	    texi2pdf(file = output, clean = FALSE, quiet = quiet)
	    find_vignette_product(name, by = "texi2pdf", engine = engine)
	} else
	    output
    } # else NULL

    # Tangle
    sources <- if (tangle) {
	engine$tangle(file, quiet = quiet, encoding = encoding, ...)
	setwd(tdir)  # In case tangle changed it
	find_vignette_product(name, by = "tangle", main = FALSE, engine = engine)
    } # else NULL

    ## Cleanup newly created files unless those in 'keep'
    keep <- c(sources, final, keep)
    if (is.na(clean)) {  # Use NA to signal we want .tex (or .md) files kept.
	keep <- c(keep, output)
	clean <- TRUE
    }
    if (clean) {
	f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), keep)
	newer <- file_test("-nt", f, ".build.timestamp")
	## some packages create directories
	unlink(f[newer], recursive = TRUE)
    }
    ### huh?  2nd round of cleaning even if  clean is FALSE ??
    ##     f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), c(keep, origfiles))
    ##     f <- f[file_test("-f", f)]
    ##     file.remove(f)
    ## #}

    if((is.na(clean) || clean) && file.exists(".build.timestamp")) {
        file.remove(".build.timestamp")
    }

    unique(keep)
}

### * getVignetteEncoding

getVignetteEncoding <-  function(file, ...)
{
    lines <- readLines(file, warn = FALSE)
    .getVignetteEncoding(lines, ...)
}

.getVignetteEncoding <- function(lines, default = NA)
{
    res <- .get_vignette_metadata(lines, "Encoding")[1L]

    if(is.na(res)) {
        poss <- grep("^[[:space:]]*%+[[:space:]]*\\\\SweaveUTF8[[:space:]]*$", lines, useBytes = TRUE)
        if (length(poss))
	    "UTF-8"
        else {
            ## Look for input enc lines using inputenc or inputenx
            ## Note, multiple encodings are excluded.
            poss <-
                grep("^[[:space:]]*\\\\usepackage\\[([[:alnum:]]+)\\]\\{inputen[cx]\\}",
                     lines, useBytes = TRUE)
            ## Check it is in the preamble
            start <- grep("^[[:space:]]*\\\\begin\\{document\\}",
                          lines, useBytes = TRUE)
            if(length(start))
                poss <- poss[poss < start[1L]]
            if(length(poss)) {
        	poss <- lines[poss[1L]]
        	res <- gsub("^[[:space:]]*\\\\usepackage\\[([[:alnum:]]+)\\].*", "\\1",
                            poss)               # This line should be ASCII.
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
	    } else if (!is.na(default)) {
		default
            } else { # Nothing else has indicated an encoding, maybe it's just ASCII
                asc <- iconv(lines, "latin1", "ASCII")
		if(anyNA(asc) || any(asc != lines)) "non-ASCII" else "" # or "ASCII"
            }
        }
    } else
	res
}

### * .build_vignette_index

.get_vignette_metadata <-
function(lines, tag)
{
    ## <FIXME>
    ## Why don't we anchor this to the beginning of a line?
    meta_RE <- paste0("[[:space:]]*%+[[:space:]]*\\\\Vignette",
                      tag, "\\{([^}]*(\\{[^}]*\\})*[^}]*)\\}.*")
    ## </FIXME>
    meta <- grep(meta_RE, lines, value = TRUE, useBytes = TRUE)
    trimws(gsub(meta_RE, "\\1", meta))
}

vignetteInfo <- function(file)
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

## builds vignette indices from 'vigns', a pkgVignettes() result
.build_vignette_index <- function(vigns)
{
    stopifnot(inherits(vigns, "pkgVignettes"))

    files <- vigns$docs
    names <- vigns$names
    dir <- vigns$dir
    sources <- vigns$sources

    if(!dir.exists(dir))
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
    outputs <- if(!is.null(outputs)) basename(outputs) else character(nvigns)

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
    if(!dir.exists(dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)

    subdir <- gsub(pkgdir, "", dir, fixed=TRUE)
    vigns <- pkgVignettes(dir = pkgdir, subdirs = subdir)

    vignetteIndex <- .build_vignette_index(vigns)
    badEntries <-
        vignetteIndex[grep("^[[:space:]]*$", vignetteIndex[, "Title"]), "File"]
    class(badEntries) <- "check_vignette_index"
    badEntries
}

print.check_vignette_index <-
function(x, ...)
{
    if(length(x)) {
        writeLines(c("Vignettes with missing or empty \\VignetteIndexEntry:",
                     paste0("  ", basename(unclass(x)))))
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
              paste0("<h2>Vignettes from package '", pkg,"'</h2>"),
              if(NROW(vignetteIndex) == 0L) ## NROW(NULL) = 0
                  "The package contains no vignette meta-information."
              else {
                  vignetteIndex <- cbind(Package = pkg,
                                         as.matrix(vignetteIndex[, c("File", "Title", "PDF", "R")]))
                  makeVignetteTable(vignetteIndex, depth = 3L)
              })
    otherfiles <- list.files(system.file("doc", package = pkg))
    if(NROW(vignetteIndex))
        otherfiles <- setdiff(otherfiles,
                              c(vignetteIndex[, c("PDF", "File", "R")], "index.html"))
    if (length(otherfiles)) {
    	otherfiles <- ifelse(dir.exists(system.file(file.path("doc", otherfiles), package = pkg)),
			     paste0(otherfiles, "/"),
			     otherfiles)
	urls <- paste0('<a href="', otherfiles, '">', otherfiles, '</a>')
        html <- c(html, '<h2>Other files in the <span class="samp">doc</span> directory</h2>',
                  '<table width="100%">',
		  '<col style="width: 24%;" />',
		  '<col style="width: 50%;" />',
		  '<col style="width: 24%;" />',
                  paste0('<tr><td></td><td><span class="samp">',
                         iconv(urls, "", "UTF-8"), "</span></td></tr>"),
                  "</table>")
    }
    html <- c(html, "</body></html>")
    writeLines(html, con=con)
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
    file <- basename(vigns$docs[i])
    name <- vigns$names[i]
    engine <- vignetteEngine(vigns$engines[i])

    output <- tryCatch({
        engine$tangle(file, quiet = TRUE, encoding = encoding)
        find_vignette_product(name, by = "tangle", engine = engine)
    }, error = function(e) {
        cat("\n  When tangling ", sQuote(file), ":\n", sep="")
        stop(conditionMessage(e), call. = FALSE, domain = NA)
    })

    if(length(output) == 1L) {
        tryCatch({
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
	    if (missing(package))
		stop("Vignette engine package not specified", call.=FALSE)
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
                   stop(gettextf("None of packages %s have registered vignette engines",
                                 paste(sQuote(package), collapse = ", ")),
                        domain = NA)
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
		suppressPackageStartupMessages(loadNamespace(key[1]))
                name <- paste(key, collapse = "::")
                result <- registry[[name]]
                if (is.null(result))
                    stop(gettextf("Vignette engine %s is not registered",
                                  sQuote(name)), domain = NA)
            } else {
                for (pkg in package) {
                    key <- engineKey(name, pkg)
		    try(suppressPackageStartupMessages(loadNamespace(key[1])),
                        silent = TRUE)
                    nameT <- paste(key, collapse = "::")
                    result <- registry[[nameT]]
                    if (!is.null(result))
                        break
                }
                if (is.null(result))
                    stop(gettextf("Vignette engine %s is not registered by any of the packages %s",
                                  sQuote(name),
                                  paste(sQuote(package), collapse = ", ")),
                         domain = NA)
            }

            if (!is.null(package) && !is.element(result$package, package))
                stop(gettextf("Vignette engine %s is not registered by any of the packages %s",
                              sQuote(name),
                              paste(sQuote(package), collapse = ", ")),
                     domain = NA)
        }
        result
    }

    setEngine <- function(name, package, pattern, weave, tangle,
                          aspell = list()) {
        key <- engineKey(name, package)
        if (!is.null(package) && key[1L] != package)
            stop(gettextf("Engine name %s and package %s do not match",
                          sQuote(name), sQuote(package)), domain = NA)


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
                    stop(gettextf("Argument %s must be a function and not %s",
                                  sQuote("weave"), sQuote(class(weave)[1L])),
                         domain = NA)
                if (!is.function(tangle))
                    stop(gettextf("Argument %s must be a function and not %s",
                                  sQuote("tangle"), sQuote(class(tangle)[1L])),
                         domain = NA)
            }
            if (is.null(pattern))
                pattern <- "[.][rRsS](nw|tex)$"
            else if (!is.character(pattern))
                stop(gettextf("Argument %s must be a character vector or NULL and not %s",
                              sQuote("pattern"), sQuote(class(pattern)[1L])),
                     domain = NA)

            result <-
                list(name = key[2L], package = key[1L], pattern = pattern,
                     weave = weave, tangle = tangle, aspell = aspell)
            assign(rname, result, registry)
        }

        result
    }

    setEngine(name = "Sweave", package = "utils", pattern = NULL,
              weave = function(...) utils::Sweave(...),
              tangle = function(...) utils::Stangle(...),
              aspell = list(filter = "Sweave", control = "-t"))

    function(name, weave, tangle, pattern = NULL, package = NULL,
             aspell = list()) {
        if (missing(weave)) { # we're getting the engine
            getEngine(name, package)
        } else { # we're setting a new engine
            if (is.element(name, c("Sweave", "utils::Sweave"))) {
                stop(gettextf("Cannot change the %s engine or use an engine of that name",
                              sQuote("Sweave")), domain = NA)
            }
            if (missing(package))
                package <- utils::packageName(parent.frame())
            result <-
                setEngine(name, package, pattern = pattern,
                          weave = weave, tangle = tangle, aspell = aspell)
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
    else if (length(pkgs)) {
        pkgs <- unlist(strsplit(pkgs, ","))
        pkgs <- gsub('[[:space:]]', '', pkgs)
    }
    pkgs <- unique(c(pkgs, "utils"))

    for (pkg in pkgs) {
	res <- tryCatch(suppressPackageStartupMessages(loadNamespace(pkg)),
                        error = identity)
	if (mustwork && inherits(res, "error"))
            stop(gettextf("vignette builder '%s' not found", pkg), domain = NA)
    }
    pkgs
}

# This gets the info for installed packages

getVignetteInfo <- function(package = NULL, lib.loc = NULL, all = TRUE)
{
    paths <-
        if (is.null(package)) {
            package <- .packages(all.available = all, lib.loc)
            ## allow for misnamed dirs
            find.package(package, lib.loc, quiet = TRUE)
        } else
            find.package(package, lib.loc)

    ## Find the directories with a 'doc' subdirectory *possibly*
    ## containing vignettes.

    paths <- paths[dir.exists(file.path(paths, "doc"))]

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
            R <- if (is.null(entries$R)) rep("", NROW(entries)) else entries$R
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
