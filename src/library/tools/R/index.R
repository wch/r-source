#  File src/library/tools/R/index.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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
#  https://www.R-project.org/Licenses/

### Miscellaneous indexing functions.

## <NOTE>
## Currently indices are represented as 2-column character matrices.
## To 'merge' indices in the sense of using the values from index B for
## all keys in index A also present in index B, we currently use
##   idx <- match(indA[ , 1L], indB[ , 1L], 0L)
##   indA[which(idx != 0L), 2L] <- indB[idx, 2L]
## which could be abstracted into a function .mergeIndexEntries().
## </NOTE>

### * .build_data_index

.build_data_index <-
function(dataDir, contents)
{
    ## Build an index with information about all available data sets.
    ## See .build_demo_index() for an explanation of what we do here.

    ## <NOTE>
    ## We could also have an interface like
    ##   .build_data_index(dir, contents = NULL)
    ## where @code{dir} is the path to a package's root source dir and
    ## contents is
    ##    Rd_contents(list_files_with_type(file.path(dir, "man"),
    ##                                     "docs")).
    ## </NOTE>

    if(!dir.exists(dataDir))
        stop(gettextf("directory '%s' does not exist", dataDir),
             domain = NA)
    ## dataFiles <- list_files_with_type(dataDir, "data")
    dataTopics <- list_data_in_pkg(dataDir=dataDir)
    if(!length(dataTopics)) return(matrix("", 0L, 2L))
    names(dataTopics) <- paste0(names(dataTopics), "/")
    datasets <- unlist(dataTopics)
    ## it is possible to have topics that create no object:
    ## BioC's makecdfenv did.
    if(!length(datasets)) return(matrix("", 0L, 2L))
    names(datasets) <- sub("/[^/]*$", "", names(datasets))
    datasets <- sort(datasets)
    dataIndex <- cbind(datasets, "")
    ## Note that NROW(contents) might be 0.
    if(length(datasets) && NROW(contents)) {
        aliasIndices <-
            rep(1 : NROW(contents), lengths(contents$Aliases))
        idx <- match(datasets, unlist(contents$Aliases), 0L)
        dataIndex[which(idx != 0L), 2L] <-
            contents[aliasIndices[idx], "Title"]
    }
    if(length(datasets))
        dataIndex[, 1L] <-
            as.vector(ifelse(datasets == names(datasets), datasets,
                             paste0(datasets, " (", names(datasets), ")")))
    dimnames(dataIndex) <- NULL
    dataIndex
}

### * .build_demo_index

.build_demo_index <-
function(demoDir)
{
    ## Build an index with information about all available demos.

    ## <NOTE>
    ## We use both the contents of @file{00Index} (if possible) and the
    ## information which demos are actually available to build the real
    ## demo index.
    ## This ensures that demo() really lists all *available* demos, even
    ## if some might be 'undocumented', i.e., without index information.
    ## Use .check_demo_index() to check whether available demo code and
    ## docs are in sync.
    ## </NOTE>

    if(!dir.exists(demoDir))
        stop(gettextf("directory '%s' does not exist", demoDir),
             domain = NA)
    demoFiles <- list_files_with_type(demoDir, "demo")
    demoTopics <- unique(basename(file_path_sans_ext(demoFiles)))
    if(!length(demoTopics)) return(matrix("", 0L, 2L))
    demoIndex <- cbind(demoTopics, "")
    if(file_test("-f", INDEX <- file.path(demoDir, "00Index"))) {
        demoEntries <- tryCatch(read.00Index(INDEX), error = identity)
        if(inherits(demoEntries, "error"))
            warning(gettextf("cannot read index information in file '%s'",
                             INDEX),
                    domain = NA)
        else {
            idx <- match(demoTopics, demoEntries[ , 1L], 0L)
            demoIndex[which(idx != 0L), 2L] <- demoEntries[idx, 2L]
        }
    }
    dimnames(demoIndex) <- NULL
    demoIndex
}

### * .check_demo_index

.check_demo_index <-
function(demoDir)
{
    if(!dir.exists(demoDir))
        stop(gettextf("directory '%s' does not exist", demoDir),
             domain = NA)
    info_from_build <- .build_demo_index(demoDir)
    info_from_index <-
        tryCatch(read.00Index(file.path(demoDir, "00Index")),
                 error = function(e)
                 stop(gettextf("cannot read index information in file '%s'",
                               file.path(demoDir, "00Index")),
                      domain = NA))
    bad_entries <-
        list(missing_from_index =
             info_from_build[grep("^[[:space:]]*$",
                                  info_from_build[ , 2L]),
                             1L],
             missing_from_demos =
             info_from_index[!info_from_index[ , 1L]
                             %in% info_from_build[ , 1L],
                             1L])
    class(bad_entries) <- "check_demo_index"
    bad_entries
}

print.check_demo_index <-
function(x, ...)
{
    if(length(bad <- x$missing_from_index)) {
        writeLines(c("Demos with missing or empty index information:",
                     paste(" ", bad)))
    }
    if(length(bad <- x$missing_from_demos)) {
        writeLines(c("Demo index entries without corresponding demo:",
                     paste(" ", bad)))
    }
    invisible(x)
}

### * .build_hsearch_index

.build_hsearch_index <-
function(contents, packageName, defaultEncoding = NULL)
{
    ## Build an index of the Rd contents in 'contents', of a package
    ## named 'packageName' in a form useful for help.search().
    ## As from 2.3.0 the installation directory is no longer recorded,
    ## but the format is kept for back-compatibility.

    dbAliases <- dbConcepts <- dbKeywords <-
        matrix(character(), ncol = 3L)

    if((nr <- NROW(contents)) > 0L) {
        ## IDs are used for indexing the Rd objects in the help.search
        ## db.
        IDs <- seq_len(nr)
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
            encoding <- NULL
        }
        else {
            base <- as.matrix(contents[, c("Name", "Title")])
            aliases <- contents[, "Aliases"]
            encoding <- contents$Encoding # may not be there ...
        }
        if(is.null(encoding))
            encoding <- character(length = nr)
        if(!is.null(defaultEncoding))
            encoding[!nzchar(encoding)] <- defaultEncoding
        keywords <- contents[, "Keywords"]
        ## We create 4 character matrices (cannot use data frames for
        ## efficiency reasons): 'dbBase' holds all character string
        ## data; 'dbAliases', 'dbConcepts' and 'dbKeywords' hold
        ## character vector data in a 3-column character matrix format
        ## with entry, ID of the Rd object the entry comes from, and the
        ## package the object comes from.  The latter is useful when
        ## subscripting the help.search db according to package.
        dbBase <- cbind(packageName, "", IDs, base,
                        topic = sapply(aliases, "[", 1L), encoding)
        ## If there are no aliases at all, cbind() below would give
        ## matrix(packageName, ncol = 1L).  (Of course, Rd objects
        ## without aliases are useless ...)
        if(length(tmp <- unlist(aliases)))
            dbAliases <-
                cbind(tmp, rep.int(IDs, lengths(aliases)),
                      packageName)
        ## And similarly if there are no keywords at all.
        if(length(tmp <- unlist(keywords)))
            dbKeywords <-
                cbind(tmp, rep.int(IDs, lengths(keywords)),
                      packageName)
        ## Finally, concepts are a feature added in R 1.8 ...
        if("Concepts" %in% colnames(contents)) {
            concepts <- contents[, "Concepts"]
            if(length(tmp <- unlist(concepts)))
                dbConcepts <-
                    cbind(tmp, rep.int(IDs, lengths(concepts)),
                          packageName)
        }
    }
    else
        dbBase <- matrix(character(), ncol = 7L)

    colnames(dbBase) <- hsearch_index_colnames$Base
    colnames(dbAliases) <- hsearch_index_colnames$Aliases
    colnames(dbKeywords) <- hsearch_index_colnames$Keywords
    colnames(dbConcepts) <- hsearch_index_colnames$Concepts

    list(dbBase, dbAliases, dbKeywords, dbConcepts)
}

hsearch_index_colnames <-
    list(Base = c("Package", "LibPath", "ID", "Name", "Title", "Topic",
         "Encoding"),
         Aliases = c("Alias", "ID", "Package"),
         Keywords = c("Keyword", "ID", "Package"),
         Concepts = c("Concept", "ID", "Package"))

### * .build_links_index

.build_links_index <-
function(contents, package)
{
    if(length(contents)) {
        aliases <- contents$Aliases
        lens <- lengths(aliases)
        files <- sub("\\.[Rr]d$", "\\.html", contents$File)
        structure(file.path("../..", package, "html", rep.int(files, lens)),
                  names = unlist(aliases))
    } else character()
}


### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
