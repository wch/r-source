### Miscellaneous indexing functions.

## <NOTE>
## Currently indices are represented as 2-column character matrices.
## To 'merge' indices in the sense of using the values from index B for
## all keys in index A also present in index B, we currently use
##   idx <- match(indA[ , 1], indB[ , 1], 0)
##   indA[which(idx != 0), 2] <- indB[idx, 2]
## which could be abstracted into a function .mergeIndexEntries().
## </NOTE>

### * .buildDataIndex

.buildDataIndex <-
function(dataDir, contents)
{
    ## Build an index with information about all available data sets.
    ## See .buildDemoIndex() for an explanation of what we do here.

    ## <NOTE>
    ## We could also have an interface like
    ##   .buildDataIndex(dir, contents = NULL)
    ## where @code{dir} is the path to a package's root source dir and
    ## contents is Rdcontents(.listFilesWithType(file.path(dir, "man"),
    ## "docs")).
    ## </NOTE>

    if(!.fileTest("-d", dataDir))
        stop(paste("directory", sQuote(dataDir), "does not exist"))
    dataFiles <- .listFilesWithType(dataDir, "data")
    dataTopics <- unique(basename(.filePathSansExt(dataFiles)))
    if(!length(dataTopics)) return(matrix("", 0, 2))
    dataIndex <- cbind(dataTopics, "")
    ## <FIXME>
    ## Remove this for 1.8.
    ## Compatibility code for transition from old-style to new-style
    ## indexing.  If we have @file{data/00Index}, use it when computing
    ## the data index, but let the Rd entries override the index ones.
    if(.fileTest("-f", INDEX <- file.path(dataDir, "00Index"))) {
        dataEntries <- try(read.00Index(INDEX))
        if(inherits(dataEntries, "try-error"))
            warning(paste("cannot read index information in file",
                          sQuote(INDEX)))
        idx <- match(dataTopics, dataEntries[ , 1], 0)
        dataIndex[which(idx != 0), 2] <- dataEntries[idx, 2]
    }
    ## </FIXME>
    ## NROW(contents) might be 0
    if (NROW(contents)) {
        aliasIndices <-
            rep(1 : NROW(contents), sapply(contents$Aliases, length))
        idx <- match(dataTopics, unlist(contents$Aliases), 0)
        dataIndex[which(idx != 0), 2] <-
            contents[aliasIndices[idx], "Title"]
    }
    dimnames(dataIndex) <- NULL
    dataIndex
}

### * .buildDemoIndex

.buildDemoIndex <-
function(demoDir)
{
    ## Build an index with information about all available demos.

    ## <NOTE>
    ## We use both the contents of @file{00Index} (if possible) and the
    ## information which demos are actually available to build the real
    ## demo index.
    ## This ensures that demo() really lists all *available* demos, even
    ## if some might be 'undocumented', i.e., without index information.
    ## Use .checkDemoIndex() to check whether available demo code and
    ## docs are in sync.
    ## </NOTE>

    if(!.fileTest("-d", demoDir))
        stop(paste("directory", sQuote(demoDir), "does not exist"))
    demoFiles <- .listFilesWithType(demoDir, "demo")
    demoTopics <- unique(basename(.filePathSansExt(demoFiles)))
    if(!length(demoTopics)) return(matrix("", 0, 2))
    demoIndex <- cbind(demoTopics, "")
    if(.fileTest("-f", INDEX <- file.path(demoDir, "00Index"))) {
        demoEntries <- try(read.00Index(INDEX))
        if(inherits(demoEntries, "try-error"))
            warning(paste("cannot read index information in file",
                          sQuote(INDEX)))
        idx <- match(demoTopics, demoEntries[ , 1], 0)
        demoIndex[which(idx != 0), 2] <- demoEntries[idx, 2]
    }
    dimnames(demoIndex) <- NULL
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
