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

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this, and do not build it.
    if(.fileTest("-f", file.path(dir, "INDEX")))
        file.copy(file.path(dir, "INDEX"),
                  file.path(outDir, "INDEX"),
                  overwrite = TRUE)

    outMetaDir <- file.path(outDir, "Meta")
    if(!.fileTest("-d", outMetaDir)) dir.create(outMetaDir)

    .installPackageRdIndices(dir, outDir)
    .installPackageVignetteIndex(dir, outDir)
    .installPackageDemoIndex(dir, outDir)
    invisible(NULL)
}

### * .installPackageRdIndices

.installPackageRdIndices <-
function(dir, outDir)
{
    dir <- .convertFilePathToAbsolute(dir)
    docsDir <- file.path(dir, "man")
    if(!.fileTest("-d", docsDir)) return()

    dataDir <- file.path(dir, "data")
    packageName <- basename(dir)

    indices <- c(file.path("Meta", "Rd.rds"), "CONTENTS", "INDEX")
    upToDate <- .fileTest("-nt", file.path(outDir, indices), docsDir)
    if(.fileTest("-d", dataDir)) {
        ## Note that the data index is computed from both the package's
        ## Rd files and the data sets actually available.
        upToDate <-
            c(upToDate,
              .fileTest("-nt",
                        file.path(outDir, "Meta", "data.rds"),
                        c(dataDir, docsDir)))
    }
    if(all(upToDate)) return()

    contents <- Rdcontents(.listFilesWithType(docsDir, "docs"))

    .writeContentsRDS(contents, file.path(outDir, "Meta", "Rd.rds"))

    .writeContentsDCF(contents, packageName,
                      file.path(outDir, "CONTENTS"))

    ## If there is no @file{INDEX} file in the package sources, we
    ## build one.
    ## <FIXME>
    ## Maybe also save this in RDS format then?
    if(!.fileTest("-f", file.path(dir, "INDEX")))
        writeLines(formatDL(.buildRdIndex(contents)),
                   file.path(outDir, "INDEX"))
    ## </FIXME>

    if(.fileTest("-d", dataDir)) {
        .saveRDS(.buildDataIndex(dataDir, contents),
                 file.path(outDir, "Meta", "data.rds"))
    }
    invisible(NULL)
}

### * .installPackageVignetteIndex

.installPackageVignetteIndex <-
function(dir, outDir)
{
    vignetteDir <- file.path(dir, "inst", "doc")
    ## Create a vignette index only if the vignette dir exists and
    ## really contains vignettes.
    if(!.fileTest("-d", vignetteDir)) return()
    if(!length(.listFilesWithType(vignetteDir, "vignette"))) return()

    vignetteIndex <- .buildVignetteIndex(vignetteDir)

    .saveRDS(vignetteIndex,
             file = file.path(outDir, "Meta", "vignette.rds"))

    ## <FIXME>
    ## Compatibility code for BioC vignette tools.
    ## Remove eventually ...
    outVignetteDir <- file.path(outDir, "doc")
    if(!.fileTest("-d", outVignetteDir)) dir.create(outVignetteDir)
    vignetteIndex <-
        vignetteIndex[vignetteIndex$PDF != "", c("PDF", "Title")]
    writeLines(formatDL(vignetteIndex, style = "list"),
               file.path(outVignetteDir, "00Index.dcf"))
    ## </FIXME>
    invisible(NULL)
}

### * .installPackageDemoIndex

.installPackageDemoIndex <-
function(dir, outDir)
{
    demoDir <- file.path(dir, "demo")
    if(!.fileTest("-d", demoDir)) return()
    demoIndex <- .buildDemoIndex(demoDir)
    .saveRDS(demoIndex,
             file = file.path(outDir, "Meta", "demo.rds"))
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
