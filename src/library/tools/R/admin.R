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

.installPackageIndices <-
function(dir, outDir)
{
    if(!.fileTest("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    else {
        dir <- .convertFilePathToAbsolute(dir)
        packageName <- basename(dir)
    }
    docsDir <- file.path(dir, "man")
    if(!.fileTest("-d", docsDir))
        stop(paste("directory", sQuote(dir),
                   "does not contain Rd sources"))
    
    indices <- c("CONTENTS.rda", "CONTENTS", "INDEX")
    if(.fileTest("-d", file.path(dir, "data")))
        indices <- c(indices, file.path("data", "00Index"))
    if(all(.fileTest("-nt", file.path(outDir, indices), docsDir)))
        return()

    docsExts <- c("Rd", "rd")
    docsFiles <- .listFilesWithExts(docsDir, docsExts)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        docsFiles <- c(docsFiles,
                       .listFilesWithExts(docsOSDir, docsExts))

    contents <- Rdcontents(docsFiles)

    ## Change e.g. when we use a different format for the contents db
    ## (e.g., with aliases and keywords not collapsed).
    version <- "0.1"

    save(list = c("contents", "version"),
         file = file.path(outDir, "CONTENTS.rda"))

    ## Same code as in Rd2contents(): in the future, we will create all
    ## indices at install time using .installPackageIndices(), and the
    ## function Rd2contents() as the R version of the Perl script
    ## Rd2contents.pl will no longer be needed.

    ## <FIXME>
    ## This has 'html' hard-wired.
    ## Also what about MacOS Classic?
    URLs <- paste("../../../library/",
                  packageName,
                  "/html/",
                  basename(gsub("\.[Rr]d$", "", contents[ , "File"])),
                  ".html",
                  sep = "")
    ## </FIXME>
                  
    cat(paste(c("Entry:", "Aliases:", "Keywords:", "Description:",
                "URL:"),
              t(cbind(contents[, c("Name", "Aliases", "Keywords",
                                   "Title")],
                      URLs))),
        sep = c("\n", "\n", "\n", "\n", "\n\n"),
        file = file.path(outDir, "CONTENTS"))

    ## <NOTE>
    ## Code similar to Rdindex() ...
    writeLines(formatDL(contents[ , c("Name", "Title"), drop = FALSE]),
               file.path(outDir, "INDEX"))

    if(.fileTest("-d", file.path(dir, "data"))) {
        if(!.fileTest("-d", file.path(outDir, "data")))
            dir.create(file.path(outDir, "data"))
        ind <- (contents[ , "Keywords"] == "datasets" |
                contents[ , "Type"] == "data")
        contents <- contents[ind, , drop = FALSE]
        writeLines(formatDL(contents[ , c("Name", "Title"),
                                     drop = FALSE]),
                   file.path(outDir, "data", "00Index"))
    }
    ## </NOTE>
    
}
