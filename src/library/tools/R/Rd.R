### * Rdpp

Rdpp <-
function(lines)
{
    ## Preprocess lines with Rd markup according to .Platform$OS.type.

    if(!is.character(lines))
        stop(paste("argument", sQuote(lines),
                   "must be a character vector"))

    ## Strip Rd comments first.
    lines <- .stripRdComments(lines)

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
        ppTypes[i] <- gsub("^#ifdef[[:space:]]+([[:alnum:]]+).*",
                           "\\1", ppLines[i]) != OS
    }
    if(any(i <- grep("^#ifndef", ppLines))) {
        ppTypes[i] <- gsub("^#ifndef[[:space:]]+([[:alnum:]]+).*",
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
    gsub("(^|[^\\])((\\\\\\\\)*)%.*", "\\1\\2", lines)
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

    aliases <- .getRdMetaDataFromRdLines(lines, "alias")
    concepts <- .getRdMetaDataFromRdLines(lines, "concept")
    keywords <- .getRdMetaDataFromRdLines(lines, "keyword")

    ## Could be none or more than one ... argh.
    RdType <- c(.getRdMetaDataFromRdLines(lines, "docType"), "")[1]

    txt <- paste(lines, collapse = "\n")

    RdName <- .getRdName(txt)
    if(!length(RdName))
        stop(paste("missing/empty \\name field in ",
                   sQuote(summary(file)$description), "\n",
                   "Rd files must have a non-empty \\name.\n",
                   "See chapter ", sQuote("Writing R documentation"),
                   " in manual ", sQuote("Writing R Extensions"),
                   ".", sep = ""))

    RdTitle <- .getRdTitle(txt)
    if(!length(RdTitle))
        stop(paste("missing/empty \\title field in ",
                   sQuote(summary(file)$description), "\n",
                   "Rd files must have a non-empty \\title.\n",
                   "See chapter ", sQuote("Writing R documentation"),
                   " in manual ", sQuote("Writing R Extensions"),
                   ".", sep = ""))

    list(name = RdName, type = RdType, title = RdTitle,
         aliases = aliases, concepts = concepts, keywords = keywords)
}

### * Rdcontents

Rdcontents <-
function(RdFiles)
{
    ## Compute contents db from Rd files.

    RdFiles <- path.expand(RdFiles[fileTest("-f", RdFiles)])

    if(length(RdFiles) == 0)
        return(data.frame(File = I(character(0)),
                          Name = I(character(0)),
                          Type = I(character(0)),
                          Title = I(character(0)),
                          Aliases = I(list()),
                          Concepts = I(list()),
                          Keywords = I(list())))

    contents <- vector("list", length(RdFiles) * 6)
    dim(contents) <- c(length(RdFiles), 6)
    for(i in seq(along = RdFiles)) {
        contents[i, ] <- Rdinfo(RdFiles[i])
    }
    colnames(contents) <-
        c("Name", "Type", "Title", "Aliases", "Concepts", "Keywords")

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
               Concepts = I(contents[ , "Concepts"]),
               Keywords = I(contents[ , "Keywords"]),
               row.names = NULL)  # avoid trying to compute row names
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
                  filePathSansExt(contents[ , "File"]),
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
    ## R version of defunct @code{R CMD Rdindex} (now removed).

    if((length(RdFiles) == 1) && fileTest("-d", RdFiles)) {
        ## Compatibility code for the former @code{R CMD Rdindex}
        ## interface.
        docsDir <- RdFiles
        if(fileTest("-d", file.path(docsDir, "man")))
            docsDir <- file.path(docsDir, "man")
        RdFiles <- listFilesWithType(docsDir, "docs")
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

    if(!fileTest("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    else {
        dir <- filePathAsAbsolute(dir)
        packageName <- basename(dir)
    }
    docsDir <- file.path(dir, "man")
    if(!fileTest("-d", docsDir))
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

    contents <- Rdcontents(listFilesWithType(docsDir, "docs"))

    .writeContentsDCF(contents, packageName, outFile)
}

### * Rddb

Rddb <-
function(package, dir, lib.loc = NULL)
{
    ## Build an Rd 'data base' from an installed package or the unpacked
    ## package sources as a list containing the 'raw' R documentation
    ## objects obtained via readLines().

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop(paste("argument", sQuote("package"),
                       "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd objects"))
        docsFiles <- listFilesWithType(docsDir, "docs")
        db <- list()
        for(f in docsFiles) {
            lines <- readLines(f)
            eofPos <- grep("\\eof$", lines)
            db <- c(db, split(lines,
                              rep(seq(along = eofPos),
                                  times = diff(c(0, eofPos)))))
        }
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- filePathAsAbsolute(dir)
        docsDir <- file.path(dir, "man")
        if(!fileTest("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        docsFiles <- listFilesWithType(docsDir, "docs")
        db <- lapply(docsFiles, readLines)

    }

    db

}

### * getRdSection

getRdSection <-
function(txt, type, predefined = TRUE)
{
    ## Extract Rd section(s) 'type' from (preprocessed) Rd markup in the
    ## character string 'txt'.  Use 'predefined = FALSE' for dealing
    ## with user-defined sections.

    ## <NOTE>
    ## This is *not* vectorized.  As we try extracting *all* top-level
    ## sections of the given type, computations on a single character
    ## string can result in a character vector of arbitray length.
    ## Hence, a vectorized version would return its results similar to
    ## e.g. strsplit(), i.e., a list of character vectors.  Worth the
    ## effort?
    ## </FIXME>

    out <- character()
    if(length(txt) != 1)
        stop("'txt' must be a character string")
    pattern <- paste("(^|\n)[[:space:]]*\\\\",
                     ifelse(predefined, type,
                            paste("section{", type, "}",
                                  sep = "")),
                     "{",
                     sep = "")
    while((pos <- regexpr(pattern, txt)) != -1) {
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        pos <- delimMatch(txt)
        if(pos == -1) {
            if((type == "alias") && predefined) {
                ## \alias entries seem to be special (Paren.Rd).
                ## The regexp below feels wrong, but is based on what is
                ## used in Perl's R::Rdlists::build_index(), sort of.
                pos <- regexpr("{([^\n]*)}(\n|$)", txt)
            }
            if(pos == -1)
                stop(paste("unterminated section", sQuote(type)))
            else {
                out <- c(out, sub("{([^\n]*)}(\n|$).*", "\\1", txt))
                txt <- substring(txt, pos + attr(pos, "match.length"))
                next
            }

        }
        out <- c(out,
                 substring(txt,
                           pos + 1,
                           pos + attr(pos, "match.length") - 2))
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    out
}

### * getRdItems

getRdItems <-
function(txt)
{
    ## Extract names of Rd \item{}{} markup in the character string
    ## 'txt'.
    out <- character()
    if(length(txt) != 1)
        stop("'txt' must be a character string")
    pattern <- "(^|\n)[[:space:]]*\\\\item{"
    while((pos <- regexpr(pattern, txt)) != -1) {
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        if((pos <- delimMatch(txt)) == -1)
            stop("unmatched \\item name")
        out <- c(out,
                 substring(txt,
                           pos + 1,
                           pos + attr(pos, "match.length") - 2))
        txt <- substring(txt, pos + attr(pos, "match.length"))
        ## The next character should really be a '{'.  Let's be nice
        ## and tolerate whitespace in between ...
        if((pos <- regexpr("^[[:space:]]*{", txt)) == -1)
            stop(paste("no \\item description for item",
                       sQuote(out[length(out)])))
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        if((pos <- delimMatch(txt)) == -1)
            stop("unmatched \\item description")
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    out
}

### * .getRdMetaDataFromRdLines

.getRdMetaDataFromRdLines <-
function(lines, kind) {
    pattern <- paste("^[[:space:]]*\\\\", kind,
                     "{[[:space:]]*(.*)[[:space:]]*}.*", sep = "")
    lines <- grep(pattern, lines, value = TRUE)
    lines <- sub(pattern, "\\1", lines)
    lines <- gsub("\\\\%", "%", lines)
    lines
}

### * .getRdArgumentNames

.getRdArgumentNames <-
function(txt)
{
    txt <- getRdSection(txt, "arguments")
    txt <- unlist(sapply(txt, getRdItems))
    if(!length(txt)) return(character())
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- sub("^[[:space:]]*", "", txt)
    txt <- sub("[[:space:]]*$", "", txt)
    txt
}

### * .getRdName

.getRdName <-
function(txt)
{
    start <- regexpr("\\\\name{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1) return(character())
    RdName <- gsub("[[:space:]]*", " ",
                   substr(txt,
                          start + 6,
                          start + attr(start, "match.length") - 2))
    RdName
}

### * .getRdTitle

.getRdTitle <-
function(txt)
{
    start <- regexpr("\\\\title{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1) return(character())
    RdTitle <- gsub("[[:space:]]*", " ",
                    substr(txt,
                           start + 7,
                           start + attr(start, "match.length") - 2))
    RdTitle
}

### * .getRdExampleCode

.getRdExampleCode <-
function(txt)
{
    txt <- getRdSection(txt, "examples")
    if(length(txt) != 1) return(character())
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\%", "%", txt)

    ## Now try removing \dontrun{}.
    ## Simple version of R::Rdconv::undefine_command().
    out <- character()
    pattern <- "\\\\dontrun\\{"
    while((pos <- regexpr(pattern, txt)) != -1) {
        out <- c(out, substring(txt, 1, pos - 1))
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        if((pos <- delimMatch(txt)) == -1)
            stop("unclosed \\dontrun")
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    txt <- paste(c(out, txt), collapse = "")
    ## Now try removing \dontshow{} and \testonly{}.
    ## Simple version of R::Rdconv::replace_command().
    out <- character()
    pattern <- "\\\\(testonly|dontshow)\\{"
    while((pos <- regexpr(pattern, txt)) != -1) {
        out <- c(out, substring(txt, 1, pos - 1))
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        if((pos <- delimMatch(txt)) == -1)
            stop("unclosed \\dontshow or \\testonly")
        out <- c(out,
                 substring(txt, 2, pos + attr(pos, "match.length") - 2))
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    paste(c(out, txt), collapse = "")
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
