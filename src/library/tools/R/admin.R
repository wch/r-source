### * .installPackageDescription

.installPackageDescription <-
function(dir, outDir)
{
    ## Function for taking the DESCRIPTION package meta-information,
    ## at least partially checking it, and installing it with the
    ## 'Built:' fields added.  Note that from 1.7.0 on, packages without
    ## compiled code are not marked as being from any platform.
    dfile <- file.path(dir, "DESCRIPTION")
    if(!fileTest("-f", dfile))
        stop(paste("file", sQuote(dfile), "does not exist"))
    db <- try(read.dcf(dfile)[1, ])
    if(inherits(db, "try-error"))
        stop(paste("file", sQuote(dfile), "is not in valid DCF format"))
    ## Check for fields needed for what follows.
    ## <FIXME>
    ## In fact, more fields are 'required' as per R CMD check.
    ## Eventually we should have the same tests here.
    ## Maybe have .checkDescription() for this?
    ## Should also include the above, of course.
    requiredFields <- c("Package", "Title", "Description")
    if(any(i <- which(is.na(match(requiredFields, names(db)))))) {
        stop(paste("required fields missing from DESCRIPTION:",
                   paste(requiredFields[i], collapse = " ")))
    }
    ## </FIXME>
    OS <- Sys.getenv("R_OSTYPE")
    OStype <- if(nchar(OS) && OS == "windows")
        "i386-pc-mingw32"
    else
        R.version$platform
    writeLines(c(formatDL(names(db), db, style = "list"),
                 paste("Built: R ",
                       paste(R.version[c("major", "minor")],
                             collapse = "."),
                       "; ",
                       if(fileTest("-d", file.path(dir, "src"))) OStype
                       else "",
                       "; ",
                       ## Prefer date in ISO 8601 format.
                       ## Could also use
                       ##   format(Sys.time(), "%a %b %d %X %Y")
                       Sys.time(),
                       "; ",
                       .OStype(),
                       sep = "")),
               file.path(outDir, "DESCRIPTION"))
    invisible()
}

### * .installPackageCodeFiles

.installPackageCodeFiles <-
function(dir, outDir)
{
    if(!fileTest("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    dir <- filePathAsAbsolute(dir)

    ## Attempt to set the LC_COLLATE locale to 'C' to turn off locale
    ## specific sorting.
    curLocale <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", curLocale), add = TRUE)
    ## (Guaranteed to work as per the Sys,setlocale() docs.)
    lccollate <- "C"
    if(Sys.setlocale("LC_COLLATE", lccollate) != lccollate) {
        ## <NOTE>
        ## I don't think we can give an error here.
        ## It may be the case that Sys.setlocale() fails because the "OS
        ## reports request cannot be honored" (src/main/platform.c), in
        ## which case we should still proceed ...
        warning("cannot turn off locale-specific sorting via LC_COLLATE")
        ## </NOTE>
    }

    ## We definitely need a valid DESCRIPTION file.
    db <- try(read.dcf(file.path(dir, "DESCRIPTION"))[1, ],
              silent = TRUE)
    if(inherits(db, "try-error"))
        stop(paste("package directory", sQuote(dir),
                   "has no valid DESCRIPTION file"))
    codeDir <- file.path(dir, "R")
    if(!fileTest("-d", codeDir)) return(invisible())

    codeFiles <- listFilesWithType(codeDir, "code", full.names = FALSE)

    collationField <-
        c(paste("Collate", .OStype(), sep = "."), "Collate")
    if(any(i <- collationField %in% names(db))) {
        ## We have a Collate specification in the DESCRIPTION file:
        ## currently, file paths relative to codeDir, separated by
        ## white space, possibly quoted.  Note that we could have
        ## newlines in DCF entries but do not allow them in file names,
        ## hence we gsub() them out.
        collationField <- collationField[i][1]
        codeFilesInCspec <-
            scan(textConnection(gsub("\n", " ", db[collationField])),
                 what = character(), strip.white = TRUE, quiet = TRUE)
        ## Duplicated entries in the collaction spec?
        badFiles <-
            unique(codeFilesInCspec[duplicated(codeFilesInCspec)])
        if(length(badFiles)) {
            out <- paste("\nduplicated files in",
                         sQuote(collationField),
                         "field:")
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out)
        }
        ## See which files are listed in the collation spec but don't
        ## exist.
        badFiles <- codeFilesInCspec[! codeFilesInCspec %in% codeFiles]
        if(length(badFiles)) {
            out <- paste("\nfiles in ", sQuote(collationField),
                         " field missing from ", sQuote(codeDir),
                         ":",
                         sep = "")
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out)
        }
        ## See which files exist but are missing from the collation
        ## spec.  Note that we do not want the collation spec to use
        ## only a subset of the available code files.
        badFiles <- codeFiles[! codeFiles %in% codeFilesInCspec]
        if(length(badFiles)) {
            out <- paste("\nfiles in", sQuote(codeDir),
                         "missing from", sQuote(collationField),
                         "field:")
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out)
        }
        ## Everything's groovy ...
        codeFiles <- codeFilesInCspec
    }

    codeFiles <- file.path(codeDir, codeFiles)

    if(!fileTest("-d", outDir) && !dir.create(outDir))
        stop("cannot open directory", sQuote(outDir))
    outCodeDir <- file.path(outDir, "R")
    if(!fileTest("-d", outCodeDir) && !dir.create(outCodeDir))
        stop("cannot open directory", sQuote(outCodeDir))
    outFile <- file.path(outCodeDir, db["Package"])
    ## <NOTE>
    ## It may be safer to do
    ##   writeLines(sapply(codeFiles, readLines), outFile)
    ## instead, but this would be much slower ...
    if(!file.create(outFile)) stop("unable to create ", outFile)
    writeLines(paste(".packageName <- \"", db["Package"], "\"", sep=""),
               outFile)
    if(!all(file.append(outFile, codeFiles)))
        stop("unable to write code files")
    ## </NOTE>

    invisible()
}


### * .installPackageIndices

.installPackageIndices <-
function(dir, outDir)
{
    if(!fileTest("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    if(!fileTest("-d", outDir))
        stop(paste("directory", sQuote(outDir), "does not exist"))

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this, and do not build it.
    if(fileTest("-f", file.path(dir, "INDEX")))
        if(!file.copy(file.path(dir, "INDEX"),
                      file.path(outDir, "INDEX"),
                      overwrite = TRUE))
            stop("unable to copy INDEX to ", file.path(outDir, "INDEX"))

    outMetaDir <- file.path(outDir, "Meta")
    if(!fileTest("-d", outMetaDir) && !dir.create(outMetaDir))
         stop("cannot open directory", sQuote(outMetaDir))
    .installPackageRdIndices(dir, outDir)
    .installPackageVignetteIndex(dir, outDir)
    .installPackageDemoIndex(dir, outDir)
    invisible()
}

### * .installPackageRdIndices

.installPackageRdIndices <-
function(dir, outDir)
{
    dir <- filePathAsAbsolute(dir)
    docsDir <- file.path(dir, "man")
    if(!fileTest("-d", docsDir)) return(invisible())

    dataDir <- file.path(dir, "data")
    packageName <- basename(dir)

    indices <- c(file.path("Meta", "Rd.rds"),
                 file.path("Meta", "hsearch.rds"),
                 "CONTENTS", "INDEX")
    upToDate <- fileTest("-nt", file.path(outDir, indices), docsDir)
    if(fileTest("-d", dataDir)) {
        ## Note that the data index is computed from both the package's
        ## Rd files and the data sets actually available.
        upToDate <-
            c(upToDate,
              fileTest("-nt",
                        file.path(outDir, "Meta", "data.rds"),
                        c(dataDir, docsDir)))
    }
    if(all(upToDate)) return(invisible())

    contents <- Rdcontents(listFilesWithType(docsDir, "docs"))

    .writeContentsRDS(contents, file.path(outDir, "Meta", "Rd.rds"))

    .saveRDS(.buildHsearchIndex(contents, packageName, outDir),
             file.path(outDir, "Meta", "hsearch.rds"))

    .writeContentsDCF(contents, packageName,
                      file.path(outDir, "CONTENTS"))

    ## If there is no @file{INDEX} file in the package sources, we
    ## build one.
    ## <FIXME>
    ## Maybe also save this in RDS format then?
    if(!fileTest("-f", file.path(dir, "INDEX")))
        writeLines(formatDL(.buildRdIndex(contents)),
                   file.path(outDir, "INDEX"))
    ## </FIXME>

    if(fileTest("-d", dataDir)) {
        .saveRDS(.buildDataIndex(dataDir, contents),
                 file.path(outDir, "Meta", "data.rds"))
    }
    invisible()
}

### * .installPackageVignetteIndex

.installPackageVignetteIndex <-
function(dir, outDir)
{
    dir <- filePathAsAbsolute(dir)
    vignetteDir <- file.path(dir, "inst", "doc")
    ## Create a vignette index only if the vignette dir exists.
    if(!fileTest("-d", vignetteDir))
        return(invisible())

    packageName <- basename(dir)
    outVignetteDir <- file.path(outDir, "doc")
    if(!fileTest("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop("cannot open directory", sQuote(outVignetteDir))

    ## If there is an HTML index in the @file{inst/doc} subdirectory of
    ## the package source directory (@code{dir}), we do not overwrite it
    ## (similar to top-level @file{INDEX} files).  Installation already
    ## copies/d this over.
    hasHtmlIndex <- fileTest("-f", file.path(vignetteDir, "index.html"))
    htmlIndex <- file.path(outDir, "doc", "index.html")

    ## Write dummy HTML index if no vignettes are found and exit.
    if(!length(listFilesWithType(vignetteDir, "vignette"))) {
        if(!hasHtmlIndex)
            .writeVignetteHtmlIndex(packageName, htmlIndex)
        return(invisible())
    }

    vignetteIndex <- .buildVignetteIndex(vignetteDir)
    ## For base package vignettes there is no PDF in @file{vignetteDir}
    ## but there might/should be one in @file{outVignetteDir}.
    if(NROW(vignetteIndex) > 0) {
        vignettePDFs <-
            sub("$", ".pdf",
                basename(filePathSansExt(vignetteIndex$File)))
        ind <- fileTest("-f", file.path(outVignetteDir, vignettePDFs))
        vignetteIndex$PDF[ind] <- vignettePDFs[ind]
    }
    if(!hasHtmlIndex)
        .writeVignetteHtmlIndex(packageName, htmlIndex, vignetteIndex)

    .saveRDS(vignetteIndex,
             file = file.path(outDir, "Meta", "vignette.rds"))

    invisible()
}

### * .installPackageVignettes

.installPackageVignettes <-
function(dir, outDir)
{
    dir <- filePathAsAbsolute(dir)
    vignetteDir <- file.path(dir, "inst", "doc")
    if(!fileTest("-d", vignetteDir))
        return(invisible())
    vignetteFiles <- listFilesWithType(vignetteDir, "vignette")
    if(!length(vignetteFiles))
        return(invisible())

    outDir <- filePathAsAbsolute(outDir)
    outVignetteDir <- file.path(outDir, "doc")
    if(!fileTest("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop("cannot open directory", sQuote(outVignetteDir))
    ## For the time being, assume that no PDFs are available in
    ## vignetteDir.
    vignettePDFs <-
        file.path(outVignetteDir,
                  sub("$", ".pdf",
                      basename(filePathSansExt(vignetteFiles))))
    upToDate <- fileTest("-nt", vignettePDFs, vignetteFiles)
    if(all(upToDate))
        return(invisible())

    ## For the time being, the primary use of this function is to
    ## install (and build) vignettes in base packages.  Hence, we build
    ## in a subdir of the current directory rather than a temp dir: this
    ## allows inspection of problems and automatic cleanup via Make.
    cwd <- getwd()
    buildDir <- file.path(cwd, ".vignettes")
    if(!fileTest("-d", buildDir) && !dir.create(buildDir))
        stop(paste("cannot create directory", sQuote(buildDir)))
    on.exit(setwd(cwd))
    setwd(buildDir)

    ## Argh.  We need to ensure that vignetteDir is in TEXINPUTS and
    ## BIBINPUTS.
    envSep <- if(.Platform$OS.type == "windows") ";" else ":"
    ## (Yes, it would be nice to have envPath() similar to file.path().)
    texinputs <- Sys.getenv("TEXINPUTS")
    bibinputs <- Sys.getenv("BIBINPUTS")
    on.exit(Sys.putenv(TEXINPUTS = texinputs, BIBINPUTS = bibinputs),
            add = TRUE)
    Sys.putenv(TEXINPUTS = paste(vignetteDir, Sys.getenv("TEXINPUTS"),
               sep = envSep),
               BIBINPUTS = paste(vignetteDir, Sys.getenv("BIBINPUTS"),
               sep = envSep))

    for(srcfile in vignetteFiles[!upToDate]) {
        base <- basename(filePathSansExt(srcfile))
        texfile <- paste(base, ".tex", sep = "")
        yy <- try(Sweave(srcfile, pdf = TRUE, eps = FALSE, quiet =
                         TRUE))
        if(inherits(yy, "try-error"))
            stop(yy)
        ## In case of an error, do not clean up: should we point to
        ## buildDir for possible inspection of results/problems?
        if(.Platform$OS.type == "windows") {
            ## may not have texi2dvi
            res <- system(paste("pdflatex", texfile))
            if(res) stop(paste("unable to run pdflatex on", sQuote(texfile)))
            if(length(grep("\\bibdata",
                           readLines(paste(base, ".aux", sep = ""))))) {
                res <- system(paste("bibtex", base))
                if(res) stop(paste("unable to run bibtex on", sQuote(base)))
                res <- system(paste("pdflatex", texfile))
                if(res) stop(paste("unable to run pdflatex on", sQuote(texfile)))
            }
            res <- system(paste("pdflatex", texfile))
            if(res) stop(paste("unable to run pdflatex on", sQuote(texfile)))
        } else
            texi2dvi(texfile, pdf = TRUE, quiet = TRUE)
        pdffile <-
            paste(basename(filePathSansExt(srcfile)), ".pdf", sep = "")
        if(!file.exists(pdffile))
            stop(paste("file", sQuote(pdffile), "was not created"))
        if(!file.copy(pdffile, outVignetteDir, overwrite = TRUE))
            stop(paste("cannot copy", sQuote(pdffile), "to",
                       sQuote(outVignetteDir)))
    }
    unlink(buildDir, recursive = TRUE)
    invisible()
}


### * .installPackageDemoIndex

.installPackageDemoIndex <-
function(dir, outDir)
{
    demoDir <- file.path(dir, "demo")
    if(!fileTest("-d", demoDir)) return(invisible())
    demoIndex <- .buildDemoIndex(demoDir)
    .saveRDS(demoIndex,
             file = file.path(outDir, "Meta", "demo.rds"))
    invisible()
}

### * .installPackageNamespaceInfo

.installPackageNamespaceInfo <-
function(dir, outDir)
{
    dir <- filePathAsAbsolute(dir)
    nsFile <- file.path(dir, "NAMESPACE")
    if(!fileTest("-f", nsFile)) return(invisible())
    nsInfoFilePath <- file.path(outDir, "Meta", "nsInfo.rds")
    if(fileTest("-nt", nsInfoFilePath, nsFile)) return(invisible())
    nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
    outMetaDir <- file.path(outDir, "Meta")
    if(!fileTest("-d", outMetaDir) && !dir.create(outMetaDir))
        stop("cannot open directory", sQuote(outMetaDir))
    .saveRDS(nsInfo, nsInfoFilePath)
    invisible()
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
