### * .installPackageDescription

.installPackageDescription <-
function(dir, outDir)
{
    ## Function for taking the DESCRIPTION package meta-information,
    ## at least partially checking it, and installing it with the
    ## 'Built:' fields added.  Note that from 1.7.0 on, packages without
    ## compiled code are not marked as being from any platform.
    dfile <- file.path(dir, "DESCRIPTION")
    if(!.fileTest("-f", dfile))
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
        stop(paste("Required fields missing from DESCRIPTION:",
                   paste(requiredFields[i], collapse = " ")))
    }
    ## </FIXME>
    writeLines(c(formatDL(names(db), db, style = "list"),
                 paste("Built: R ",
                       paste(R.version[c("major", "minor")],
                             collapse = "."),
                       "; ",
                       if(.fileTest("-d", file.path(dir, "src")))
                           R.version$platform
                       else
                           "",
                       "; ",
                       ## Prefer date in ISO 8601 format.
                       ## Could also use
                       ##   format(Sys.time(), "%a %b %d %X %Y")
                       Sys.time(),
                       sep = "")),
               file.path(outDir, "DESCRIPTION"))
    invisible(NULL)
}

### * .installPackageIndices

.installPackageIndices <-
function(dir, outDir)
{
    if(!.fileTest("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    ## <FIXME>
    ## Should we do any checking on @code{outDir}?
    ## </FIXME>
    
    .installPackageRdIndices(dir, outDir)
    .installPackageVignetteIndex(dir, outDir)
    .installPackageDemoIndex(dir, outDir)
}

### * .installPackageRdIndices

.installPackageRdIndices <-
function(dir, outDir)
{
    dir <- .convertFilePathToAbsolute(dir)
    docsDir <- file.path(dir, "man")
    if(!.fileTest("-d", docsDir))
        stop(paste("directory", sQuote(dir),
                   "does not contain Rd sources"))
    
    dataDir <- file.path(dir, "data")
    packageName <- basename(dir)
    
    indices <- c("CONTENTS.rda", "CONTENTS", "INDEX")
    upToDate <- .fileTest("-nt", file.path(outDir, indices), docsDir)
    if(.fileTest("-d", dataDir)) {
        ## Note that the data index is computed from both the package's
        ## Rd files and the data sets actually available.
        upToDate <-
            c(upToDate,
              .fileTest("-nt",
                        file.path(outDir, "data", "00Index.dcf"),
                        c(dataDir, docsDir)))
    }
    if(all(upToDate)) return()

    contents <- Rdcontents(.listFilesWithType(docsDir, "docs"))

    .writeContentsRDA(contents, file.path(outDir, "CONTENTS.rda"))

    .writeContentsDCF(contents, packageName,
                      file.path(outDir, "CONTENTS"))

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this (in @code{R CMD INSTALL}), and do not build it.
    ## <FIXME>
    ## Should we install here instead?
    if(!.fileTest("-f", file.path(dir, "INDEX")))
        writeLines(formatDL(.buildRdIndex(contents)),
                   file.path(outDir, "INDEX"))
    ## </FIXME>

    if(.fileTest("-d", dataDir)) {
        outDataDir <- file.path(outDir, "data")
        if(!.fileTest("-d", outDataDir)) dir.create(outDataDir)
        writeLines(formatDL(.buildDataIndex(dataDir, contents),
                            style = "list"),
                   file.path(outDataDir, "00Index.dcf"))
    }
    
}

### * .installPackageVignetteIndex

.installPackageVignetteIndex <-
function(dir, outDir)
{
    vignetteDir <- file.path(dir, "inst", "doc")
    if(!.fileTest("-d", vignetteDir)) return()
    vignetteFiles <- .listFilesWithType(vignetteDir, "vignette")
    vignetteIndex <- .buildVignetteIndex(vignetteFiles)
    outVignetteDir <- file.path(outDir, "doc")
    if(!.fileTest("-d", outVignetteDir)) dir.create(outVignetteDir)
    writeLines(formatDL(vignetteIndex, style = "list"),
               file.path(outVignetteDir, "00Index.dcf"))
}

### * .installPackageDemoIndex

.installPackageDemoIndex <-
function(dir, outDir)
{
    demoDir <- file.path(dir, "demo")
    if(!.fileTest("-d", demoDir)) return()
    demoIndex <- .buildDemoIndex(demoDir)
    outDemoDir <- file.path(outDir, "demo")
    if(!.fileTest("-d", outDemoDir)) dir.create(outDemoDir)
    writeLines(formatDL(demoIndex, style = "list"),
               file.path(outDemoDir, "00Index.dcf"))
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
