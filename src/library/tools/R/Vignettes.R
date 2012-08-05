#  File src/library/tools/R/Vignettes.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
        tmpd <- tempfile("Sweave")
        if(!dir.create(tmpd))
            stop("unable to create temp directory ", tmpd)
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

    startdir <- getwd()
    for(f in vigns$docs) {
        if(tangle)
            .eval_with_capture(tryCatch(utils::Stangle(f, quiet = TRUE),
                                        error = function(e)
                                        result$tangle[[f]] <<-
                                        conditionMessage(e)))
        if(weave)
            .eval_with_capture(tryCatch(utils::Sweave(f, quiet = TRUE),
                                        error = function(e)
                                        result$weave[[f]] <<-
                                        conditionMessage(e)))
        setwd(startdir) # in case a vignette changes the working dir
    }

    if(tangle) {
        ## Tangling can create several source files if splitting is on,
        ## and these can be .R or .S (at least).  However, there is
        ## no guarantee that running them in alphabetical order in a
        ## session will work -- with named chunks it normally will not.
        cwd <- getwd()
        if (is.null(cwd))
            stop("current working directory cannot be ascertained")
        sources <- list_files_with_exts(cwd, c("r", "s", "R", "S"))
        sources <- sources[file_test("-nt", sources, ".check.timestamp")]
        for(f in sources) {
            .eval_with_capture(tryCatch(source(f),
                                        error = function(e)
                                        result$source[[f]] <<-
                                        conditionMessage(e)))
            setwd(startdir)
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
            bad_vignettes <- as.character(names(unlist(result)))
            bad_vignettes <- file_path_sans_ext(basename(bad_vignettes))
            for(f in vigns$docs) {
                bf <- file_path_sans_ext(basename(f))
                if(bf %in% bad_vignettes) break
                bft <- paste0(bf, ".tex")
                .eval_with_capture(tryCatch(texi2pdf(file = bft, clean = FALSE,
                                                     quiet = TRUE),
                                            error = function(e)
                                            result$latex[[f]] <<-
                                            conditionMessage(e)))
            }
        }
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

### * pkgVignettes
###
### Get an object of class pkgVignettes which contains a list of Sweave
### files and the name of the directory which contains them.

pkgVignettes <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        docdir <- file.path(find.package(package, lib.loc), "doc")
        ## Using package installed in @code{dir} ...
    } else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir), domain = NA)
        else {
            docdir <- file.path(file_path_as_absolute(dir), "vignettes")
            if(!file_test("-d", docdir))
                docdir <- file.path(file_path_as_absolute(dir), "inst", "doc")
        }
    }

    if(!file_test("-d", docdir)) return(NULL)

    docs <- list_files_with_type(docdir, "vignette")

    z <- list(docs=docs, dir=docdir)
    class(z) <- "pkgVignettes"
    z
}

### * buildVignettes
###
### Run a weave and pdflatex on all vignettes of a package and try to
### remove all temporary files that were created.

buildVignettes <-
function(package, dir, lib.loc = NULL, quiet = TRUE, clean = TRUE)
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc)
    if(is.null(vigns)) return(invisible())

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

    pdfs <- character()
    startdir <- getwd()
    for(f in vigns$docs) {
        f <- basename(f)
        bf <- file_path_sans_ext(f)
        bft <- paste0(bf, ".tex")
        pdfs <- c(pdfs, paste0(bf, ".pdf"))

        tryCatch(utils::Sweave(f, quiet = quiet),
                 error = function(e) {
                     stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",
                                   f, conditionMessage(e)),
                          domain = NA, call. = FALSE)
                 })
        setwd(startdir)
        ## This can fail if run in a directory whose path contains spaces.
        if(!have.makefile)
            texi2pdf(file = bft, clean = FALSE, quiet = quiet)
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
        if(clean) {
            f <- list.files(all.files = TRUE) %w/o% c(".", "..", pdfs)
            newer <- file_test("-nt", f, ".build.timestamp")
            ## some packages, e.g. SOAR, create directories
            unlink(f[newer], recursive = TRUE)
        }
        f <- list.files(all.files = TRUE)
        file.remove(f %w/o% c(".", "..", pdfs, origfiles))
    }

    if(file.exists(".build.timestamp")) file.remove(".build.timestamp")
    ## Might have been in origfiles ...

    invisible(NULL)
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
    list(file = basename(file), title = title, depends = depends,
         keywords = keywords)
}

.build_vignette_index <-
function(vignetteDir)
{
    if(!file_test("-d", vignetteDir))
        stop(gettextf("directory '%s' does not exist", vignetteDir),
             domain = NA)

    vignetteFiles <-
        path.expand(list_files_with_type(vignetteDir, "vignette"))

    if(!length(vignetteFiles)) {
        out <- data.frame(File = character(),
                          Title = character(),
                          PDF = character(),
                          stringsAsFactors = FALSE)
        out$Depends <- list()
        out$Keywords <- list()
        return(out)
    }

    contents <- vector("list", length = length(vignetteFiles) * 4L)
    dim(contents) <- c(length(vignetteFiles), 4L)
    for(i in seq_along(vignetteFiles))
        contents[i, ] <- vignetteInfo(vignetteFiles[i])
    colnames(contents) <- c("File", "Title", "Depends", "Keywords")

    ## (Note that paste(character(0L), ".pdf") does not do what we want.)
    vignettePDFs <- sub("$", ".pdf", file_path_sans_ext(vignetteFiles))

    vignetteTitles <- unlist(contents[, "Title"])

    vignettePDFs[!file_test("-f", vignettePDFs)] <- ""
    vignettePDFs <- basename(vignettePDFs)

    out <- data.frame(File = unlist(contents[, "File"]),
                      Title = vignetteTitles,
                      PDF = vignettePDFs,
                      row.names = NULL, # avoid trying to compute row
                                        # names
                      stringsAsFactors = FALSE)

    if (any(dups <- duplicated(names <- file_path_sans_ext(out$File)))) {
    	dup <- out$File[dups][1]
    	dupname <- names[dups][1]
    	orig <- out$File[ names == dupname ][1]
    	stop(gettextf("In '%s' vignettes '%s' and '%s' have the same vignette name",
    		      basename(dirname(vignetteDir)), orig, dup),
             domain = NA)
    }
    out$Depends <- contents[, "Depends"]
    out$Keywords <- contents[, "Keywords"]
    out
}

### * .check_vignette_index

.check_vignette_index <-
function(vignetteDir)
{
    if(!file_test("-d", vignetteDir))
        stop(gettextf("directory '%s' does not exist", vignetteDir),
             domain = NA)
    vignetteIndex <- .build_vignette_index(vignetteDir)
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
        print(basename(file_path_sans_ext(unclass(x))), ...)
    }
    invisible(x)
}


### * .writeVignetteHtmlIndex

.writeVignetteHtmlIndex <-
function(pkg, con, vignetteIndex = NULL)
{
    ## FIXME: in principle we could need to set an encoding here
    html <- c(HTMLheader("Vignettes"),
              paste0("<h2>Vignettes from package '", pkg,"'</h2>"))

    if(is.null(vignetteIndex) || nrow(vignetteIndex) == 0L) {
        html <-
            c(html,
              "Sorry, the package contains no vignette meta-information nor an index.",
              'Please browse the <a href=".">directory</a>.')
    } else {
    	vignetteIndex <- cbind(Package=pkg, as.matrix(vignetteIndex[,c("File", "Title", "PDF", "R")]))
        html <- c(html, makeVignetteTable(vignetteIndex, depth=3))
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
    function(vig_name, docDir, encoding = "")
{
    ## The idea about encodings here is that Stangle reads the
    ## file, converts on read and outputs in the current encoding.
    ## Then source() can assume the current encoding.
    td <- tempfile()
    dir.create(td)
    file.copy(docDir, td, recursive = TRUE)
    setwd(file.path(td, basename(docDir)))
    result <- NULL
    tryCatch(utils::Stangle(vig_name, quiet = TRUE, encoding = encoding),
             error = function(e) result <<- conditionMessage(e))
    if(length(result)) {
        cat("\n  When tangling ", sQuote(vig_name), ":\n", sep="")
        stop(result, call. = FALSE, domain = NA)
    }
    f <- sub("\\.[RrSs](nw|tex)$", ".R", vig_name)
    tryCatch(source(f, echo = TRUE),
             error = function(e) result <<- conditionMessage(e))
    if(length(result)) {
        cat("\n  When sourcing ", sQuote(f), ":\n", sep="")
        stop(result, call. = FALSE, domain = NA)
    }
    cat("\n *** Run successfully completed ***\n")
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
