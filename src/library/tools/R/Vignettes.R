#  File src/library/tools/R/Vignettes.R
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
        ## Tangling can create several source files if splitting is on.
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
        if(!("makefile" %in% tolower(list.files(vigns$dir)))) {
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
                bft <- paste(bf, ".tex", sep = "")
                .eval_with_capture(tryCatch(texi2dvi(file = bft, pdf = TRUE,
                                                     clean = FALSE,
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

    mycat(x$weave,  "*** Weave Errors ***")
    mycat(x$tangle, "*** Tangle Errors ***")
    mycat(x$source, "*** Source Errors ***")
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
        docdir <- file.path(.find.package(package, lib.loc), "doc")
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            docdir <- file.path(file_path_as_absolute(dir), "inst",
                                "doc")
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

    wd <- getwd()
    if (is.null(wd))
        stop("current working directory cannot be ascertained")
    on.exit(setwd(wd))
    setwd(vigns$dir)

    ## should this recurse into subdirs?
    origfiles <- list.files(all.files = TRUE)
    have.makefile <- "makefile" %in% tolower(origfiles)
    file.create(".build.timestamp")

    pdfs <- character()
    startdir <- getwd()
    for(f in vigns$docs) {
        f <- basename(f)
        bf <- file_path_sans_ext(f)
        bft <- paste(bf, ".tex", sep = "")
        pdfs <- c(pdfs, paste(bf, ".pdf", sep = ""))

        tryCatch(utils::Sweave(f, quiet = quiet),
                 error = function(e) {
                     stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s",
                                   f, conditionMessage(e)),
                          domain = NA, call. = FALSE)
                 })
        setwd(startdir)
        if(!have.makefile)
            texi2dvi(file = bft, pdf = TRUE, clean = FALSE, quiet = quiet)
    }

    if(have.makefile) {
    	make <- Sys.getenv("MAKE")
        if(!nzchar(make)) make <- "make"
        yy <- system(make)
        if(make == "" || yy > 0) stop("running 'make' failed")
    } else {
        if(clean) {
            f <- list.files(all.files = TRUE) %w/o% c(".", "..", pdfs)
            newer <- file_test("-nt", f, ".build.timestamp")
            file.remove(f[newer])
        }
        f <- list.files(all.files = TRUE)
        file.remove(f %w/o% c(".", "..", pdfs, origfiles))
    }

    if(file.exists(".build.timestamp")) file.remove(".build.timestamp")
    ## Might have been in origfiles ...

    invisible(NULL)
}

### * .build_vignette_index

.get_vignette_metadata <-
function(lines, tag)
{
    meta_RE <- paste("[[:space:]]*%+[[:space:]]*\\\\Vignette", tag,
                     "\\{([^}]*)\\}", sep = "")
    meta <- grep(meta_RE, lines, value = TRUE)
    .strip_whitespace(gsub(meta_RE, "\\1", meta))
}

vignetteInfo <-
function(file)
{
    lines <- readLines(file, warn = FALSE)

    ## <FIXME>
    ## Can only proceed with lines with are valid in the current locale.
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
    }
    else
        unlist(strsplit(keywords[1L], ", *"))
    list(file = file, title = title, depends = depends,
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
    html <- c('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">',
              paste("<html><head><title>R:", pkg, "vignettes</title>"),
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../R.css\">",
              "</head><body>",
              paste("<h2>Vignettes of package", pkg,"</h2>"))

    if(is.null(vignetteIndex) || nrow(vignetteIndex) == 0L) {
        html <- c(html, "Sorry, the package contains no vignette meta-information or index.",
                  "Please browse the <a href=\".\">directory</a>.")
    }
    else{
        html <- c(html, "<dl>")
        for(k in seq_len(nrow(vignetteIndex))){
            html <- c(html,
                      paste("<dt><a href=\"", vignetteIndex[k, "PDF"], "\">",
                            vignetteIndex[k, "PDF"], "</a>:", sep=""),
                      paste("<dd>", vignetteIndex[k, "Title"]))
        }
        html <- c(html, "</dl>")
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


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
