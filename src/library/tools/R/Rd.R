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
    
    aliasesRegExp <- "^\\\\alias{[[:space:]]*(.*)[[:space:]]*}.*"
    aliases <- grep(aliasesRegExp, lines, value = TRUE)
    aliases <- gsub(aliasesRegExp, "\\1", aliases)
    aliases <- gsub("\\\\%", "%", aliases)
    
    keywordsRegExp <- "^\\\\keyword{[[:space:]]*(.*)[[:space:]]*}.*"
    keywords <- grep(keywordsRegExp, lines, value = TRUE)
    keywords <- gsub(keywordsRegExp, "\\1", keywords)
    keywords <- gsub("\\\\%", "%", keywords)
    
    ## <FIXME>
    ## docType ... 
    RdTypeRegExp <- "^\\\\docType{[[:space:]]*(.*)[[:space:]]*}.*"
    RdType <- grep(RdTypeRegExp, lines, value = TRUE)
    ## Could be none or more than one ... argh.
    RdType <- c(gsub(RdTypeRegExp, "\\1", RdType), "")[1]
    ## </FIXME>
    
    txt <- paste(lines, collapse = "\n")

    start <- regexpr("\\\\name{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1)
       stop("missing/empty \\name field")
    RdName <- gsub("[[:space:]]*", " ",
                   substr(txt,
                          start + 6,
                          start + attr(start, "match.length") - 2))

    start <- regexpr("\\\\title{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1)
        stop("missing/empty \\title field")
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

    RdFiles <- RdFiles[.fileTest("-f", RdFiles)]
    contents <- matrix("", nr = length(RdFiles), nc = 6)
    for(i in seq(along = RdFiles)) {
        contents[i, ] <-
            c(RdFiles[i],
              sapply(Rdinfo(RdFiles[i]), paste, collapse = " "))
    }
    colnames(contents) <-
        c("File", "Name", "Type", "Title", "Aliases", "Keywords")

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
    title <- contents[ , "Title"]
    title <- gsub("\(``\|''\)", "\"", title)
    title <- gsub("`", "'", title)
    title <- gsub("\([[:alnum:]]\)--\([[:alnum:]]\)", "\\1-\\2", title)
    title <- gsub("\\\\\&", "&", title)
    title <- gsub("---", "--", title)
    contents[ , "Title"] <- title

    contents
}

### * Rdindex

Rdindex <-
function(RdFiles, outFile = "", type = NULL,
         width = 0.9 * getOption("width"), indent = NULL)
{
    ## Create @file{INDEX} or @file{data/00Index} files from Rd files.

    if((length(RdFiles) == 1) && .fileTest("-d", RdFiles)) {
        ## Compatibility code for the @code{R CMD Rdindex} interface.
        ## <NOTE>
        ## It is not a good idea to rewrite @code{R CMD Rdindex} as a
        ## shell wrapper to @code{Rdindex()} because passing a long list
        ## of Rd files as arguments can be quite messy.  Wait for this
        ## until we have R scripts.
        ## </NOTE>
        docsDir <- RdFiles
        if(.fileTest("-d", file.path(docsDir, "man")))
            docsDir <- file.path(docsDir, "man")
        docsExts <- .makeFileExts("docs")
        RdFiles <- .listFilesWithExts(docsDir, docsExts)
        docsOSDir <- file.path(docsDir, .Platform$OS)
        if(.fileTest("-d", docsOSDir))
            RdFiles <- c(RdFiles,
                         .listFilesWithExts(docsOSDir, docsExts))
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

    contents <- Rdcontents(RdFiles)
    
    if(!is.null(type)) {
        ind <- contents[ , "Type"] %in% type
        ## Argh.  Ideally we only want to subscript according to
        ## \docType.  Maybe for 2.0 ...
        if(type == "data")
            ind <- ind | contents[ , "Keywords"] == "datasets"
        contents <- contents[ind, , drop = FALSE]
    }
            
    writeLines(formatDL(contents[ , c("Name", "Title"), drop = FALSE],
                        width = width, indent = indent),
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

    docsExts <- .makeFileExts("docs")
    docsFiles <- .listFilesWithExts(docsDir, docsExts)
    if(file.exists(docsOSDir <- file.path(docsDir, .Platform$OS)))
        docsFiles <- c(docsFiles,
                       .listFilesWithExts(docsOSDir, docsExts))

    contents <- Rdcontents(docsFiles)

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
        file = outFile)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
