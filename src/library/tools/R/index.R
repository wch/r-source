### Miscellaneous indexing functions.

### * .buildDemoIndex

.buildDemoIndex <-
function(demoDir)
{
    ## <FIXME>
    ## We use both the contents of @file{00Index} (if possible) and the
    ## information which demos are actually available to build the real
    ## demo index.
    ## This ensures that demo() really lists all *available* demos, even
    ## if some might be 'undocumented', i.e., without index information.
    ## Use .checkDemoIndex() to check whether available demo code and
    ## docs are in sync.
    ## We should really do the same for data sets: i.e., have a function
    ## .buildDataIndex() which uses .buildRdIndex(type = "data") to
    ## build an index with information about available data sets.
    ## </FIXME>
    if(!.fileTest("-d", demoDir))
        stop(paste("directory", sQuote(demoDir), "does not exist"))
    demoFiles <- .listFilesWithType(demoDir, "demo")
    demoTopics <- basename(gsub("\\.[[:alpha:]]+$", "", demoFiles))
    demoIndex <- cbind(demoTopics, "")
    if(.fileTest("-f", index <- file.path(demoDir, "00Index"))) {
        demoEntries <- try(read.00Index(index))
        if(inherits(demoEntries, "try-error"))
            warning(paste("cannot read index information in file",
                          sQuote(index)))
        idx <- match(demoTopics, demoEntries[ , 1], 0)
        demoIndex[which(idx != 0), 2] <- demoEntries[idx, 2]
    }
    demoIndex
}

### * .checkDemoIndex

.checkDemoIndex <-
function(demoDir)
{
    if(!.fileTest("-d", demoDir))
        stop(paste("directory", sQuote(demoDir), "does not exist"))
    infoFromBuild <- .buildDemoIndex(demoDir)
    infoFromIndex <- try(read.00Index(file.path(demoDir, "00Index")))
    if(inherits(infoFromIndex, "try-error"))
        stop(paste("cannot read index information in file",
                   sQuote(file.path(demoDir, "00Index"))))
    badEntries <-
        list(missingFromIndex =
             infoFromBuild[grep("^[[:space:]]*$",
                                infoFromBuild[ , 2]),
                           1],
             missingFromDemos =
             infoFromIndex[!infoFromIndex[ , 1]
                           %in% infoFromBuild[ , 1],
                           1])
    class(badEntries) <- "checkDemoIndex"
    badEntries
}

print.checkDemoIndex <-
function(x, ...)
{
    if(length(x$missingFromIndex) > 0) {
        writeLines("Demos with missing or empty index information:")
        print(x$missingFromIndex)
    }
    if(length(x$missingFromDemos) > 0) {
        writeLines("Demo index entries without corresponding demo:")
        print(x$missingFromDemos)
    }
    invisible(x)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
