### * Rdpp

Rdpp <-
function(lines)
{
    ## Preprocess lines with Rd markup according to .Platform$OS.type.

    if(!is.character(lines))
        stop(.wrong_args("lines", "must be a character vector"))

    ## Strip Rd comments first.
    lines <- .strip_Rd_comments(lines)

    ppLineIndices <- grep("^#(endif|ifn?def[[:space:]]+[[:alnum:]]+)",
                          lines)
    ## <NOTE>
    ## This is based on the Perl code in R::Rdtools::Rdpp().
    ## What should we do with #ifn?def lines not matching the above?
    ## </NOTE>
    n_of_pp_lines <- length(ppLineIndices)
    if(n_of_pp_lines == 0) return(lines)

    OS <- .Platform$OS.type
    ppLines <- lines[ppLineIndices]

    ## Record the preprocessor line type: starts of conditionals with
    ## TRUE/FALSE according to whether they increase the skip level or
    ## not, and NA for ends of conditionals.
    ppTypes <- rep(NA, n_of_pp_lines)
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

### * .strip_Rd_comments

.strip_Rd_comments <-
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
        stop(.wrong_args("file",
                         "must be a character string or connection"))

    lines <- Rdpp(.read_Rd_lines_quietly(file))

    aliases <- .get_Rd_metadata_from_Rd_lines(lines, "alias")
    concepts <- .get_Rd_metadata_from_Rd_lines(lines, "concept")
    keywords <- .get_Rd_metadata_from_Rd_lines(lines, "keyword")

    ## Could be none or more than one ... argh.
    Rd_type <-
        c(.get_Rd_metadata_from_Rd_lines(lines, "docType"), "")[1]

    txt <- paste(lines, collapse = "\n")

    Rd_name <- .get_Rd_name(txt)
    if(!length(Rd_name))
        stop(paste("missing/empty \\name field in ",
                   sQuote(summary(file)$description), "\n",
                   "Rd files must have a non-empty \\name.\n",
                   "See chapter ", sQuote("Writing R documentation"),
                   " in manual ", sQuote("Writing R Extensions"),
                   ".", sep = ""))

    Rd_title <- .get_Rd_title(txt)
    if(!length(Rd_title))
        stop(paste("missing/empty \\title field in ",
                   sQuote(summary(file)$description), "\n",
                   "Rd files must have a non-empty \\title.\n",
                   "See chapter ", sQuote("Writing R documentation"),
                   " in manual ", sQuote("Writing R Extensions"),
                   ".", sep = ""))

    list(name = Rd_name, type = Rd_type, title = Rd_title,
         aliases = aliases, concepts = concepts, keywords = keywords)
}

### * Rdcontents

Rdcontents <-
function(RdFiles)
{
    ## Compute contents db from Rd files.

    RdFiles <- path.expand(RdFiles[file_test("-f", RdFiles)])

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

### * .write_contents_as_RDS

.write_contents_as_RDS <-
function(contents, outFile)
{
    ## Save Rd contents db to @file{outFile}.

    ## <NOTE>
    ## To deal with possible changes in the format of the contents db
    ## in the future, use a version attribute and/or a formal class.
    .saveRDS(contents, file = outFile)
    ## </NOTE>
}

### * .write_contents_as_DCF

.write_contents_as_DCF <-
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
                  file_path_sans_ext(contents[ , "File"]),
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

### * .build_Rd_index

.build_Rd_index <-
function(contents, type = NULL)
{
    ## Build an Rd 'index' containing Rd "names" (see below) and titles,
    ## maybe subscripted according to the Rd type (\docType).

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

    index <- contents[idx, c("Name", "Title"), drop = FALSE]
    if(nrow(index)) {
        ## If a \name is not a valid \alias, replace it by the first
        ## alias. 
        aliases <- contents[idx, "Aliases"]
        bad <- which(!mapply("%in%", index[, 1], aliases))
        if(any(bad)) {
            tmp <- sapply(aliases[bad], "[[", 1)
            tmp[is.na(tmp)] <- ""
            index[bad, 1] <- tmp
        }
        ## and sort it by name
        index <- index[sort.list(index[,1]), ]
    }
    index
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

    if((length(RdFiles) == 1) && file_test("-d", RdFiles)) {
        ## Compatibility code for the former @code{R CMD Rdindex}
        ## interface.
        docsDir <- RdFiles
        if(file_test("-d", file.path(docsDir, "man")))
            docsDir <- file.path(docsDir, "man")
        RdFiles <- list_files_with_type(docsDir, "docs")
    }

    if(outFile == "")
        outFile <- stdout()
    else if(is.character(outFile)) {
        outFile <- file(outFile, "w")
        on.exit(close(outFile))
    }
    if(!inherits(outFile, "connection"))
        stop(.wrong_args("outFile",
                         "must be a character string or connection"))

    index <- .build_Rd_index(Rdcontents(RdFiles), type = type)

    writeLines(formatDL(index, width = width, indent = indent),
               outFile)
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
            stop(.wrong_args("package", "must be of length 1"))
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        docsDir <- file.path(dir, "man")
        if(!file_test("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd objects"))
        docsFiles <- list_files_with_type(docsDir, "docs")
        db <- list()
        for(f in docsFiles) {
            lines <- .read_Rd_lines_quietly(f)
            eofPos <- grep("\\eof$", lines)
            db <- c(db, split(lines[-eofPos],
                              rep(seq(along = eofPos),
                                  times = diff(c(0, eofPos)))[-eofPos]))
        }
        ## If this was installed using a recent enough version of R CMD
        ## INSTALL, information on source file names is available, and
        ## we use it for the names of the Rd db.  Otherwise, remove the
        ## artificial names attribute.
        paths <- as.character(sapply(db, "[", 1))
        names(db) <-
            if(length(paths)
               && all(regexpr("^% --- Source file: (.+) ---$", paths)
                      > -1))
                sub("^% --- Source file: (.+) ---$", "\\1", paths)
            else
                NULL
    }
    else {
        if(missing(dir))
            stop(paste("you must specify", sQuote("package"),
                       "or", sQuote("dir")))
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            dir <- file_path_as_absolute(dir)
        docsDir <- file.path(dir, "man")
        if(!file_test("-d", docsDir))
            stop(paste("directory", sQuote(dir),
                       "does not contain Rd sources"))
        docsFiles <- list_files_with_type(docsDir, "docs")
        db <- lapply(docsFiles, .read_Rd_lines_quietly)
        names(db) <- docsFiles
    }

    db

}

### * Rd_parse

Rd_parse <-
function(file, text = NULL)
{
    ## Arguments similar to the ones in parse(), with 'text' a character
    ## vector with the text to parse (elements are treated as if they
    ## were lines of a file).
    if(!is.null(text))
        lines <- Rdpp(text)
    else {
        if(is.character(file)) {
            file <- file(file)
            on.exit(close(file))
        }
        if(!inherits(file, "connection"))
            stop(.wrong_args("file",
                             "must be a character string or connection"))
        lines <- Rdpp(.read_Rd_lines_quietly(file))
    }
    
    ## Get meta data (need to agree on what precisely these are), and
    ## remove the corresponding lines (assuming that these entries are
    ## all one-liners).  We mostly do this because \alias (see Paren.Rd)
    ## has non-standard syntax.
    meta <-
        list(aliases = .get_Rd_metadata_from_Rd_lines(lines, "alias"),
             concepts = .get_Rd_metadata_from_Rd_lines(lines, "concept"),
             keywords = .get_Rd_metadata_from_Rd_lines(lines, "keyword"),
             doc_type = .get_Rd_metadata_from_Rd_lines(lines, "docType"))
    ## (Use the same regexp as in .get_Rd_metadata_from_Rd_lines().)
    i <- grep(paste("^[[:space:]]*\\\\",
                    "(alias|concept|keyword|docType)",
                    "{[[:space:]]*([^}]*[^}[:space:]])[[:space:]]*}.*",
                    sep = ""),
              lines)
    if(any(i)) lines <- lines[-i]
    ## Collapse into one character string.
    txt <- paste(lines, collapse = "\n")
    ## Initialize for extraction loop.
    tag <- ""
    tags <- list()
    rest <- vals <- character()
    ## Note that what we do here is not quite the same as what the code
    ## in R CMD check for checking Rd files does (which e.g. takes all
    ## lines starting with a command tag as top-level).  Also, it is not
    ## clear whether this is what we *really* want (or what Rdconv()
    ## should do).
    ## <NOTE>
    ## We try to catch \non_function{} here, even though it is at least
    ## deprecated.
    ## </NOTE>
    pattern <- "(^|\n)[[:space:]]*\\\\([[:alpha:]]|non_function)+{"
    while((pos <- regexpr(pattern, txt)) != -1) {
        otag <- tag
        start <- substring(txt, 1, pos + attr(pos, "match.length") - 2)
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        pos <- regexpr("\\\\([[:alpha:]]|non_function)+$", start)
        tag <- substring(start, pos + 1)
        start <- substring(start, 1, pos - 1)
        pos <- delimMatch(txt)
        if(pos == -1)
            stop(paste("unterminated section", sQuote(tag)))
        if(tag == "section") {
            tmp <- substring(txt, 2, attr(pos, "match.length") - 1)
            txt <- substring(txt, pos + attr(pos, "match.length"))
            ## Should 'txt' now really start with an open brace?
            if(substring(txt, 1, 1) != "{")
                stop(paste("incomplete section",
                           sQuote(paste("section{", tmp, "}",
                                        sep = ""))))
            pos <- delimMatch(txt)
            if(pos == -1)
                stop(paste("unterminated section",
                           sQuote(paste("section{", tmp, "}",
                                        sep = ""))))
            tag <- c(tag, tmp)
        }
        if(regexpr("^[[:space:]]*(^|\n)[[:space:]]*$", start) == -1) {
            names(start) <- paste(otag, collapse = " ")
            rest <- c(rest, start)
        }
        tags <- c(tags, list(tag))
        vals <- c(vals, substring(txt,
                                  pos + 1,
                                  pos + attr(pos, "match.length") - 2))
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    if(regexpr("^[[:space:]]*(^|\n)[[:space:]]*$", txt) == -1) {
        names(txt) <- paste(tag, collapse = " ")
        rest <- c(rest, txt)
    }
    list(meta = meta,
         data = data.frame(tags = I(tags), vals = I(vals)),
         rest = rest)
}

### * get_Rd_section

get_Rd_section <-
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
        stop(.wrong_args("txt", "must be a character string"))
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

### * get_Rd_items

get_Rd_items <-
function(txt)
{
    ## Extract names of Rd \item{}{} markup in the character string
    ## 'txt'.
    out <- character()
    if(length(txt) != 1)
        stop(.wrong_args("txt", "must be a character string"))
    pattern <- "(^|\n)[[:space:]]*\\\\item{"
    while((pos <- regexpr(pattern, txt)) != -1) {
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        if((pos <- delimMatch(txt)) == -1)
            stop(paste("unmatched \\item name in",
                       sQuote(paste("\\item{",
                                    sub("\n.*$", "", txt),
                                    sep = ""))),
                 call. = FALSE)
        out <- c(out,
                 substring(txt,
                           pos + 1,
                           pos + attr(pos, "match.length") - 2))
        txt <- substring(txt, pos + attr(pos, "match.length"))
        ## The next character should really be a '{'.  Let's be nice
        ## and tolerate whitespace in between ...
        if((pos <- regexpr("^[[:space:]]*{", txt)) == -1)
            stop(paste("no \\item description for item",
                       sQuote(out[length(out)])),
                 call. = FALSE)
        txt <- substring(txt, pos + attr(pos, "match.length") - 1)
        if((pos <- delimMatch(txt)) == -1)
            stop(paste("unmatched \\item description for item",
                       sQuote(out[length(out)])),
                 call. = FALSE)
        txt <- substring(txt, pos + attr(pos, "match.length"))
    }
    out
}

### * .get_Rd_metadata_from_Rd_lines

.get_Rd_metadata_from_Rd_lines <-
function(lines, kind) {
    pattern <- paste("^[[:space:]]*\\\\", kind,
                     "{[[:space:]]*([^}]*[^}[:space:]])[[:space:]]*}.*",
                     sep = "")
    lines <- grep(pattern, lines, value = TRUE)
    lines <- sub(pattern, "\\1", lines)
    lines <- gsub("\\\\%", "%", lines)
    lines
}

### * .get_Rd_argument_names

.get_Rd_argument_names <-
function(txt)
{
    txt <- get_Rd_section(txt, "arguments")
    txt <- unlist(sapply(txt, get_Rd_items))
    if(!length(txt)) return(character())
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- sub("^[[:space:]]*", "", txt)
    txt <- sub("[[:space:]]*$", "", txt)
    txt
}

### * .get_Rd_name

.get_Rd_name <-
function(txt)
{
    start <- regexpr("\\\\name{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1) return(character())
    Rd_name <- gsub("[[:space:]]*", " ",
                    substr(txt,
                           start + 6,
                           start + attr(start, "match.length") - 2))
    Rd_name
}

### * .get_Rd_title

.get_Rd_title <-
function(txt)
{
    start <- regexpr("\\\\title{[[:space:]]*([^\}]+)[[:space:]]*}", txt)
    if(start == -1) return(character())
    Rd_title <- gsub("[[:space:]]*", " ",
                     substr(txt,
                            start + 7,
                            start + attr(start, "match.length") - 2))
    Rd_title
}

### * .get_Rd_example_code

.get_Rd_example_code <-
function(txt)
{
    txt <- get_Rd_section(txt, "examples")
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

### .apply_Rd_filter_to_Rd_db

.apply_Rd_filter_to_Rd_db <-
function(db, FUN, ...)
{
    db <- lapply(db, function(t) try(FUN(t, ...), silent = TRUE))
    idx <- as.logical(sapply(db, inherits, "try-error"))
    if(any(idx)) {
        msg <- "Rd syntax errors found"
        for(i in which(idx))
            msg <- c(msg, 
                     paste("Syntax error in documentation object ",
                           sQuote(names(db)[i]), ":", sep = ""),
                     db[[i]])
        stop(paste(msg, collapse = "\n"), call. = FALSE)
    }
    db
}

### .get_Rd_names_from_Rd_db

.get_Rd_names_from_Rd_db <-
function(db)
{
    Rd_names <- lapply(db, .get_Rd_name)
    ## If the Rd db was obtained from an installed package, we know that
    ## all Rd objects must have a \name entry---otherwise, Rdinfo() and
    ## hence installing the package Rd contents db would have failed.
    ## For Rd dbs created from a package source directory, we now add
    ## the Rd file paths as the names attribute, so that we can point to
    ## the files with missing \name entries.
    idx <- as.numeric(sapply(Rd_names, length)) == 0
    if(any(idx)) {
        Rd_paths <- names(db)
        if(is.null(Rd_paths)) {
            ## This should not happen.
            ## We cannot refer to the bad Rd objects because we do not
            ## know their names, and have no idea which file they came
            ## from ...) 
            stop("cannot deal with Rd objects with missing/empty names")
        }
        else {
            stop(paste("missing/empty \\name field in Rd file(s)",
                       paste(" ", Rd_paths[idx], collapse = "\n"),
                       sep = "\n"),
                 call. = FALSE)
        }
    }
    unlist(Rd_names)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
