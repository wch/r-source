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

    packageName <- basename(dir)
    
    indices <- c("CONTENTS.rda", "CONTENTS", "INDEX")
    if(.fileTest("-d", file.path(dir, "data")))
        indices <- c(indices, file.path("data", "00Index.dcf"))
    if(all(.fileTest("-nt", file.path(outDir, indices), docsDir)))
        return()

    docsExts <- .makeFileExts("docs")
    docsFiles <- .listFilesWithExts(docsDir, docsExts)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        docsFiles <- c(docsFiles,
                       .listFilesWithExts(docsOSDir, docsExts))

    contents <- Rdcontents(docsFiles)

    ## * CONTENTS.rda

    ## Change e.g. when we use a different format for the contents db
    ## (e.g., with aliases and keywords not collapsed).
    version <- "0.1"
    save(list = c("contents", "version"),
         file = file.path(outDir, "CONTENTS.rda"))

    ## * CONTENTS
    
    ## <NOTE>
    ## Same code as in @code{Rd2contents()}.
    ## In the future, we will create all indices at install time using
    ## @code{.installPackageIndices()}, and @code{Rd2contents()} as the
    ## R version of the Perl script @file{Rd2contents.pl} will no longer
    ## be needed.
    ## </NOTE>

    ## <FIXME>
    ## This has 'html' hard-wired.
    ## Note that slashes etc. should be fine for URLs.
    URLs <- paste("../../../library/",
                  packageName,
                  "/html/",
                  basename(gsub("\\.[[:alpha:]]+$", "",
                                contents[ , "File"])),
                  ".html",
                  sep = "")
    ## </FIXME>
                  
    cat(paste(c("Entry:", "Aliases:", "Keywords:", "Description:",
                "URL:"),
              t(cbind(contents[, c("Name", "Aliases", "Keywords",
                                   "Title"), drop = FALSE],
                      URLs))),
        sep = c("\n", "\n", "\n", "\n", "\n\n"),
        file = file.path(outDir, "CONTENTS"))

    ## * INDEX
    
    ## <NOTE>
    ## Code similar to @code{Rdindex()} ...
    if(!.fileTest("-f", file.path(dir, "INDEX")))
        writeLines(formatDL(contents[ , c("Name", "Title"), drop = FALSE]),
                   file.path(outDir, "INDEX"))
    ## </NOTE>

    ## * data/00Index.dcf

    ## <NOTE>
    ## Code similar to @code{Rdindex()} ...
    if(.fileTest("-d", file.path(dir, "data"))) {
        outDataDir <- file.path(outDir, "data")
        if(!.fileTest("-d", outDataDir)) dir.create(outDataDir)
        ind <- (contents[ , "Keywords"] == "datasets" |
                contents[ , "Type"] == "data")
        contents <- contents[ind, c("Name", "Title"), drop = FALSE]
        writeLines(formatDL(contents, style = "list"),
                   file.path(outDataDir, "00Index.dcf"))
    }
    ## </NOTE>
}    

### * .installPackageVignetteIndex

.installPackageVignetteIndex <-
function(dir, outDir)
{
    vignetteDir <- file.path(dir, "inst", "doc")
    if(!.fileTest("-d", vignetteDir)) return()
    vignetteExts <- .makeFileExts("vignette")
    vignetteFiles <- .listFilesWithExts(vignetteDir, vignetteExts)
    vignetteIndexEntryRE <-
        "[[:space:]]*%+[[:space:]]\\\\VignetteIndexEntry\{([^}]*)\}"
    vignetteTitles <-
        sapply(vignetteFiles,
               function(file) {
                   lines <- grep(vignetteIndexEntryRE, readLines(file),
                                 value = TRUE)
                   lines <- gsub(vignetteIndexEntryRE, "\\1", lines[1])
               })
    vignetteFiles <-
        paste(basename(gsub("\\.[[:alpha:]]+$", "", vignetteFiles)),
              ".pdf", sep = "")
    outVignetteDir <- file.path(outDir, "doc")
    if(!.fileTest("-d", outVignetteDir)) dir.create(outVignetteDir)
    writeLines(formatDL(cbind(vignetteFiles, vignetteTitles),
                        style = "list"),
               file.path(outVignetteDir, "00Index.dcf"))
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
