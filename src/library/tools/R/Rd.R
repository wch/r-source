### * Rdpp

Rdpp <-
function(lines)
{
    if(!is.character(lines))
        stop(paste("argument", sQuote(lines),
                   "must be a character vector"))

    ppLineIndices <- grep("^#(endif|ifn?def[[:space:]]+[[:alnum:]]+)",
                          lines)
    ## <NOTE>
    ## This is based on the Perl code in R::Rdtools::Rdpp().
    ## What should we do with #ifn?def lines not matching the above?
    ## </NOTE>
    nOfPpLines <- length(ppLineIndices)
    if(nOfPpLines == 0) return(lines)
    OS <- .Platform$OS.type
    ppLines <- lines[ppLineIndices]

    ## Record the preprocessor line type: starts of conditionals with
    ## TRUE/FALSE according to whether they increase the skip level or
    ## not, and NA for ends of conditionals.
    ppTypes <- rep(NA, nOfPpLines)
    if(any(i <- grep("^#ifdef", ppLines))) {
        ppTypes[i] <- gsub("^#ifdef[[:space:]]+([[:alnum:]]+)",
                           "\\1", ppLines[i]) != OS
    }
    if(any(i <- grep("^#ifndef", ppLines))) {
        ppTypes[i] <- gsub("^#ifndef[[:space:]]+([[:alnum:]]+)",
                           "\\1", ppLines[i]) == OS
    }

    ## Looks stupid, but ... we need a loop to determine the skip list
    ## to deal with nested conditionals.
    skipList <- integer(0)
    skipLevel <- 0
    skipIndices <- ppLineIndices
    for(i in seq(along = ppTypes)) {
        if(!is.na(skip <- ppTypes[i])) {
            if(skipLevel == 0 && skip > 0) {
                skipStart <- ppLineIndices[i]
                skipLevel <- 1
            }
            else
                skipLevel <- skipLevel + skip
            skipList <- c(skip, skipList) # push
        }
        else {
            if(skipLevel == 1 && skipList[1] > 0) {
                skipIndices <- c(skipIndices,
                                 seq(from = skipStart,
                                     to = ppLineIndices[i]))
                skipLevel <- 0
            }
            else
                skipLevel <- skipLevel - skipList[1]
            skipList <- skipList[-1]    # pop
        }
    }

    lines[-skipIndices]
}

### * .stripRdComments

.stripRdComments <-
function(lines)
{
    gsub("([^\\])((\\\\)*)%.*", "\\1\\2", lines)
}

### * Rdinfo

Rdinfo <-
function(file)
{
    ## <NOTE>
    ## This is based on the Perl code in R::Rd::info().
    ## It seems that matches for aliases and keywords are only single
    ## line.  Hence, as we get the lines from @code{Rdpp()}, we get
    ## aliases and keywords directly from them before collapsing them to
    ## one string (which also allows us to avoid looping as in the Perl
    ## code).
    ## </NOTE>

    if(is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop(paste("argument", sQuote(file),
                   "must be a character string or connection"))

    lines <- Rdpp(readLines(file))

    ## <NOTE>
    ## Compatibility code for R::Rdtools::Rdpp(): strip comment lines.
    commentLineIndices <- grep("^[[:space:]]*\%", lines)
    if(length(commentLineIndices) > 0)
        lines <- lines[-commentLineIndices]
    ## </NOTE>
    
    aliasesRegExp <-
        "^[[:space:]]*\\\\alias{[[:space:]]*(.*)[[:space:]]*}.*"
    aliases <- grep(aliasesRegExp, lines, value = TRUE)
    aliases <- gsub(aliasesRegExp, "\\1", aliases)
    aliases <- gsub("\\\\%", "%", aliases)
    
    keywordsRegExp <-
        "^[[:space:]]*\\\\keyword{[[:space:]]*(.*)[[:space:]]*}.*"
    keywords <- grep(keywordsRegExp, lines, value = TRUE)
    keywords <- gsub(keywordsRegExp, "\\1", keywords)
    keywords <- gsub("\\\\%", "%", keywords)
    
    ## <FIXME>
    ## docType ... 
    RdTypeRegExp <-
        "^[[:space:]]*\\\\docType{[[:space:]]*(.*)[[:space:]]*}.*"
    RdType <- grep(RdTypeRegExp, lines, value = TRUE)
    ## Could be none or more than one ... argh.
    RdType <- c(gsub(RdTypeRegExp, "\\1", RdType), "")[1]
    ## </FIXME>
    
    txt <- paste(lines, collapse = "\n")

    start <- regexpr("\\\\name{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1)
        stop(paste("missing/empty \\name field in",
                   summary(file)$description))
    RdName <- gsub("[[:space:]]*", " ",
                   substr(txt,
                          start + 6,
                          start + attr(start, "match.length") - 2))

    start <- regexpr("\\\\title{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1)
        stop(paste("missing/empty \\title field in",
                   summary(file)$description))
    RdTitle <- gsub("[[:space:]]*", " ",
                    substr(txt,
                           start + 7,
                           start + attr(start, "match.length") - 2))
    
    list(name = RdName, type = RdType, title = RdTitle,
         aliases = aliases, keywords = keywords)
}

### * Rdcontents

Rdcontents <-
function(RdFiles)
{
    ## Compute contents db from Rd files.

    RdFiles <- path.expand(RdFiles[.fileTest("-f", RdFiles)])

    if(length(RdFiles) == 0)
        return(data.frame(File = I(character(0)),
                          Name = I(character(0)),
                          Type = I(character(0)),
                          Title = I(character(0)),
                          Aliases = I(list()),
                          Keywords = I(list())))
    
    contents <- vector("list", length(RdFiles) * 5)
    dim(contents) <- c(length(RdFiles), 5)
    for(i in seq(along = RdFiles)) {
        contents[i, ] <- Rdinfo(RdFiles[i])
    }
    colnames(contents) <-
        c("Name", "Type", "Title", "Aliases", "Keywords")

    ## Although R-exts says about the Rd title slot that
    ## <QUOTE>
    ##   This should be capitalized, not end in a period, and not use
    ##   any markup (which would cause problems for hypertext search).
    ## </QUOTE>
    ## some Rd files have LaTeX-style markup, including
    ## * LaTeX-style single and double quotation
    ## * Medium and punctuation dashes
    ## * Escaped ampersand.
    ## Hence we try getting rid of these ...
    title <- unlist(contents[ , "Title"])
    title <- gsub("\(``\|''\)", "\"", title)
    title <- gsub("`", "'", title)
    title <- gsub("\([[:alnum:]]\)--\([[:alnum:]]\)", "\\1-\\2", title)
    title <- gsub("\\\\\&", "&", title)
    title <- gsub("---", "--", title)
    ## Also remove leading and trailing whitespace.
    title <- sub("^[[:space:]]*", "", title)
    title <- sub("[[:space:]]*$", "", title)

    data.frame(File = I(basename(RdFiles)),
               Name = I(unlist(contents[ , "Name"])),
               Type = I(unlist(contents[ , "Type"])),
               Title = I(title),
               Aliases = I(contents[ , "Aliases"]),
               Keywords = I(contents[ , "Keywords"]))
}

### * .writeContentsRDS

.writeContentsRDS <-
function(contents, outFile)
{
    ## Save Rd contents db to @file{outFile}.
    
    ## <NOTE>
    ## To deal with possible changes in the format of the contents db
    ## in the future, use a version attribute and/or a formal class.
    .saveRDS(contents, file = outFile)
    ## </NOTE>
}

### * .writeContentsDCF

.writeContentsDCF <-
function(contents, packageName, outFile)
{
    ## Write a @file{CONTENTS} DCF file from an Rd contents db.
    ## Note that these files currently have @samp{URL:} entries which
    ## contain the package name, whereas @code{Rdcontents()} works on
    ## collections of Rd files which do not necessarily all come from
    ## the same package ...

    ## If the contents is 'empty', return immediately.  (Otherwise,
    ## e.g. URLs would not be right ...)
    if(!NROW(contents)) return()

    ## <FIXME>
    ## This has 'html' hard-wired.
    ## Note that slashes etc. should be fine for URLs.
    URLs <- paste("../../../library/", packageName, "/html/",
                  .filePathSansExt(contents[ , "File"]),
                  ".html",
                  sep = "")
    ## </FIXME>

    if(is.data.frame(contents))
        contents <-
            cbind(contents$Name,
                  sapply(contents$Aliases, paste, collapse = " "),
                  sapply(contents$Keywords, paste, collapse = " "),
                  contents$Title)
    else
        contents <-
            contents[, c("Name", "Aliases", "Keywords", "Title"),
                     drop = FALSE]
                  
    cat(paste(c("Entry:", "Aliases:", "Keywords:", "Description:",
                "URL:"),
              t(cbind(contents, URLs))),
        sep = c("\n", "\n", "\n", "\n", "\n\n"),
        file = outFile)
}

### * .buildRdIndex

.buildRdIndex <-
function(contents, type = NULL)
{
    ## Build an Rd 'index' containing Rd names and titles, maybe
    ## subscripted according to the Rd type (\docType).
    
    keywords <- contents[ , "Keywords"]
    
    if(!is.null(type)) {
        idx <- contents[ , "Type"] %in% type
        ## Argh.  Ideally we only want to subscript according to
        ## \docType.  Maybe for 2.0 ...
        if(type == "data")
            idx <- idx | keywords == "datasets"
        ## (Note: we really only want the Rd objects which have
        ## 'datasets' as their *only* keyword.)
        contents <- contents[idx, , drop = FALSE]
        keywords <- keywords[idx]
    }
    
    ## Drop all Rd objects marked as 'internal' from the index.
    idx <- is.na(sapply(keywords, function(x) match("internal", x)))
    
    contents[idx, c("Name", "Title"), drop = FALSE]
}

### * Rdindex

Rdindex <-
function(RdFiles, outFile = "", type = NULL,
         width = 0.9 * getOption("width"), indent = NULL)
{
    ## Create @file{INDEX} or @file{data/00Index} style files from Rd
    ## files.
    ##
    ## R version of @code{R CMD Rdindex}.
    ##
    ## <NOTE>
    ## It is not a good idea to rewrite @code{R CMD Rdindex} as a shell
    ## wrapper to @code{Rdindex()} because passing a long list of Rd
    ## files as arguments can be quite messy.  Wait for this until we
    ## have R scripts.
    ## </NOTE>

    if((length(RdFiles) == 1) && .fileTest("-d", RdFiles)) {
        ## Compatibility code for the @code{R CMD Rdindex} interface.
        docsDir <- RdFiles
        if(.fileTest("-d", file.path(docsDir, "man")))
            docsDir <- file.path(docsDir, "man")
        RdFiles <- .listFilesWithType(docsDir, "docs")
    }

    if(outFile == "")
        outFile <- stdout()
    else if(is.character(outFile)) {
        outFile <- file(outFile, "w")
        on.exit(close(outFile))
    }
    if(!inherits(outFile, "connection"))
        stop(paste("argument", sQuote("outFile"),
                   "must be a character string or connection"))

    index <- .buildRdIndex(Rdcontents(RdFiles), type = type)
            
    writeLines(formatDL(index, width = width, indent = indent),
               outFile)
}

### * Rd2contents

Rd2contents <-
function(dir, outFile = "")
{
    ## <NOTE>
    ## This is based on the Perl code in R_HOME/share/Rd2contents.pl.
    ## </NOTE>

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

    if(outFile == "")
        outFile <- stdout()
    else if(is.character(outFile)) {
        outFile <- file(outFile, "w")
        on.exit(close(outFile))
    }
    if(!inherits(outFile, "connection"))
        stop(paste("argument", sQuote("outFile"),
                   "must be a character string or connection"))

    contents <- Rdcontents(.listFilesWithType(docsDir, "docs"))

    .writeContentsDCF(contents, packageName, outFile)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
