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
    writeLines(c(formatDL(names(db), db, style = "list"),
                 paste("Built: R ",
                       paste(R.version[c("major", "minor")],
                             collapse = "."),
                       "; ",
                       if(fileTest("-d", file.path(dir, "src")))
                           R.version$platform
                       else
                           "",
                       "; ",
                       ## Prefer date in ISO 8601 format.
                       ## Could also use
                       ##   format(Sys.time(), "%a %b %d %X %Y")
                       Sys.time(),
                       "; ",                       
                       .Platform$OS.type,
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
        c(paste("Collate", .Platform$OS.type, sep = "."), "Collate")
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

    if(!fileTest("-d", outDir)) dir.create(outDir)
    outCodeDir <- file.path(outDir, "R")
    if(!fileTest("-d", outCodeDir)) dir.create(outCodeDir)
    outFile <- file.path(outCodeDir, db["Package"])
    ## <NOTE>
    ## It may be safer to do
    ##   writeLines(sapply(codeFiles, readLines), outFile)
    ## instead, but this would be much slower ...
    file.create(outFile)
    writeLines(paste(".packageName <- \"", db["Package"], "\"", sep=""), outFile)
    file.append(outFile, codeFiles)
    ## </NOTE>

    invisible()
}


### * .installPackageIndices

.installPackageIndices <-
function(dir, outDir)
{
    if(!fileTest("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    ## <FIXME>
    ## Should we do any checking on @code{outDir}?
    ## </FIXME>

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this, and do not build it.
    if(fileTest("-f", file.path(dir, "INDEX")))
        file.copy(file.path(dir, "INDEX"),
                  file.path(outDir, "INDEX"),
                  overwrite = TRUE)

    outMetaDir <- file.path(outDir, "Meta")
    if(!fileTest("-d", outMetaDir)) dir.create(outMetaDir)

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
    vignetteDir <- file.path(dir, "inst", "doc")
    ## Create a vignette index only if the vignette dir exists
    if(!fileTest("-d", vignetteDir))
        return(invisible())

    packageName <- basename(dir)    
    htmlIndex <- file.path(outDir, "doc", "index.html")

    ## write dummy HTML index if no vignettes are found and exit
    if(!length(listFilesWithType(vignetteDir, "vignette"))){
        if(!file.exists(htmlIndex)){
            .writeVignetteHtmlIndex(packageName, htmlIndex)
        }
        return(invisible())
    }

    vignetteIndex <- .buildVignetteIndex(vignetteDir)
    if(!file.exists(htmlIndex)){
        .writeVignetteHtmlIndex(packageName, htmlIndex, vignetteIndex)
    }

    .saveRDS(vignetteIndex,
             file = file.path(outDir, "Meta", "vignette.rds"))

    ## <FIXME>
    ## Compatibility code for BioC vignette tools.
    ## Remove eventually ...
    outVignetteDir <- file.path(outDir, "doc")
    if(!fileTest("-d", outVignetteDir)) dir.create(outVignetteDir)
    vignetteIndex <-
        vignetteIndex[vignetteIndex$PDF != "", c("PDF", "Title")]
    writeLines(formatDL(vignetteIndex, style = "list"),
               file.path(outVignetteDir, "00Index.dcf"))
    ## </FIXME>
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


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
