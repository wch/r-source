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
    ## contents is Rdcontents(listFilesWithType(file.path(dir, "man"),
    ## "docs")).
    ## </NOTE>

    if(!fileTest("-d", dataDir))
        stop(paste("directory", sQuote(dataDir), "does not exist"))
    dataFiles <- listFilesWithType(dataDir, "data")
    ## <FIXME> to avoid name clashes CO2 is stored as zCO2.R
    dataTopics <- unique(basename(filePathSansExt(dataFiles)))
    dataTopics[dataTopics == "zCO2"] <- "CO2"
    if(!length(dataTopics)) return(matrix("", 0, 2))
    dataTopics <- sort(dataTopics)
    dataIndex <- cbind(dataTopics, "")
    ## Note that NROW(contents) might be 0.
    if(NROW(contents)) {
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

    if(!fileTest("-d", demoDir))
        stop(paste("directory", sQuote(demoDir), "does not exist"))
    demoFiles <- listFilesWithType(demoDir, "demo")
    demoTopics <- unique(basename(filePathSansExt(demoFiles)))
    if(!length(demoTopics)) return(matrix("", 0, 2))
    demoIndex <- cbind(demoTopics, "")
    if(fileTest("-f", INDEX <- file.path(demoDir, "00Index"))) {
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
    if(!fileTest("-d", demoDir))
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

### * .buildHsearchIndex

.buildHsearchIndex <-
function(contents, packageName, libDir)
{
    ## Build an index of the Rd contents in 'contents', of a package
    ## named 'packageName' (to be) installed in 'libDir', in a form
    ## useful for help.search().

    dbAliases <- dbConcepts <- dbKeywords <- matrix(character(), nc = 3)

    if((nr <- NROW(contents)) > 0) {
        ## IDs are used for indexing the Rd objects in the help.search
        ## db.
        IDs <- seq(length = nr)
        if(!is.data.frame(contents)) {
            colnames(contents) <-
                c("Name", "Aliases", "Title", "Keywords")
            base <- contents[, c("Name", "Title"), drop = FALSE]
            ## If the contents db is not a data frame, then it has the
            ## aliases collapsed.  Split again as we need the first
            ## alias as the help topic to indicate for matching Rd
            ## objects.
            aliases <- strsplit(contents[, "Aliases"], " +")
            ## Don't do this for keywords though, as these might be
            ## non-standard (and hence contain white space ...).
        }
        else {
            base <-
                as.matrix(contents[, c("Name", "Title")])
            aliases <- contents[, "Aliases"]
        }
        keywords <- contents[, "Keywords"]
        ## We create 3 character matrices (cannot use data frames for
        ## efficiency reasons): 'dbBase' holds all character string
        ## data, and 'dbAliases' and 'dbKeywords' hold character vector
        ## data in a 3-column character matrix format with entry, ID of
        ## the Rd object the entry comes from, and the package the
        ## object comes from.  The latter is useful when subscripting
        ## the help.search db according to package.
        dbBase <- cbind(packageName, libDir, IDs, base,
                        topic = sapply(aliases, "[", 1))
        ## If there are no aliases at all, cbind() below would give
        ## matrix(packageName, nc = 1).  (Of course, Rd objects without
        ## aliases are useless ...)
        if(length(tmp <- unlist(aliases)) > 0)
            dbAliases <-
                cbind(tmp, rep.int(IDs, sapply(aliases, length)),
                      packageName)
        ## And similarly if there are no keywords at all.
        if(length(tmp <- unlist(keywords)) > 0)
            dbKeywords <-
                cbind(tmp, rep.int(IDs, sapply(keywords, length)),
                      packageName)
        ## Finally, concepts are a feature added in R 1.8 ...
        if("Concepts" %in% colnames(contents)) {
            concepts <- contents[, "Concepts"]
            if(length(tmp <- unlist(concepts)) > 0)
                dbConcepts <-
                    cbind(tmp, rep.int(IDs, sapply(concepts, length)),
                          packageName)
        }
    }
    else {
        dbBase <- matrix(character(), nc = 6)
    }

    colnames(dbBase) <-
        c("Package", "LibPath", "ID", "name", "title", "topic")
    colnames(dbAliases) <-
        c("Aliases", "ID", "Package")
    colnames(dbKeywords) <-
        c("Keywords", "ID", "Package")
    colnames(dbConcepts) <-
        c("Concepts", "ID", "Package")

    list(dbBase, dbAliases, dbKeywords, dbConcepts)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
