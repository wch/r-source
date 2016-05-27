#  File src/library/tools/R/Rd.R
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

### * Rd_info

Rd_info <-
function(file, encoding = "unknown")
{
    ## <FIXME>
    ## This used to work only for a given Rd file.
    ## now only for a parsed Rd object.

    if(inherits(file, "Rd")) {
        Rd <- file
        description <- attr(attr(Rd, "srcref"), "srcfile")$filename
    } else
        stop("Rd object required")

    aliases <- .Rd_get_metadata(Rd, "alias")
    concepts <- .Rd_get_metadata(Rd, "concept")
    keywords <- .Rd_get_metadata(Rd, "keyword")

    ## Could be none or more than one ... argh.
    Rd_type <- .Rd_get_doc_type(Rd)
    encoding <- c(.Rd_get_metadata(Rd, "encoding"), "")[1L]

    Rd_name <- .Rd_get_name(Rd)
    if(!length(Rd_name)) {
        msg <-
            c(gettextf("missing/empty %s field in '%s'",
                       "\\name",
                       description),
              gettextf("Rd files must have a non-empty %s.",
                       "\\name"),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    Rd_title <- .Rd_get_title(Rd)
    if(!nchar(Rd_title)) {
        msg <-
            c(gettextf("missing/empty \\title field in '%s'",
                       description),
              gettext("Rd files must have a non-empty \\title."),
              gettext("See chapter 'Writing R documentation' in manual 'Writing R Extensions'."))
        stop(paste(msg, collapse = "\n"), domain = NA)
    }

    list(name = Rd_name, type = Rd_type, title = Rd_title,
         aliases = aliases, concepts = concepts, keywords = keywords,
         encoding = encoding)
}

### * Rd_contents

Rd_contents <-
function(db)
{
    ## Compute contents db from Rd db.
    ## NB: Encoding is the encoding declared in the file, not
    ## that after parsing.
    if(!length(db)) {
        out <- data.frame(File = character(),
                          Name = character(),
                          Type = character(),
                          Title = character(),
                          Encoding = character(),
                          stringsAsFactors = FALSE)
        out$Aliases <- list()
        out$Concepts <- list()
        out$Keywords <- list()
        return(out)
    }

    entries <- c("Name", "Type", "Title", "Aliases", "Concepts",
                 "Keywords", "Encoding")
    contents <- vector("list", length(db) * length(entries))
    dim(contents) <- c(length(db), length(entries))
    for(i in seq_along(db)) {
        contents[i, ] <- Rd_info(db[[i]])
    }
    colnames(contents) <- entries

    title <- .Rd_format_title(unlist(contents[ , "Title"]))
    out <- data.frame(File = basename(names(db)),
                      Name = unlist(contents[ , "Name"]),
                      Type = unlist(contents[ , "Type"]),
                      Title = title,
                      Encoding = unlist(contents[ , "Encoding"]),
                      row.names = NULL, # avoid trying to compute row
                                        # names
                      stringsAsFactors = FALSE)
    out$Aliases <- contents[ , "Aliases"]
    out$Concepts <- contents[ , "Concepts"]
    out$Keywords <- contents[ , "Keywords"]
    out
}

### * .write_Rd_contents_as_RDS

.write_Rd_contents_as_RDS <-
function(contents, outFile)
{
    ## Save Rd contents db to @file{outFile}.

    ## <NOTE>
    ## To deal with possible changes in the format of the contents db
    ## in the future, use a version attribute and/or a formal class.
    saveRDS(contents, file = outFile, compress = TRUE)
    ## </NOTE>
}

### * .write_Rd_contents_as_DCF

if(FALSE) {
.write_Rd_contents_as_DCF <-
function(contents, packageName, outFile)
{
    ## Write a @file{CONTENTS} DCF file from an Rd contents db.
    ## Note that these files currently have @samp{URL:} entries which
    ## contain the package name, whereas @code{Rd_contents()} works on
    ## collections of Rd files which do not necessarily all come from
    ## the same package ...

    ## If the contents is 'empty', return immediately.  (Otherwise,
    ## e.g. URLs would not be right ...)
    if(!NROW(contents)) return()

    ## <NOTE>
    ## This has 'html' hard-wired.
    ## Note that slashes etc. should be fine for URLs.
    URLs <- paste0("../../../library/", packageName, "/html/",
                   file_path_sans_ext(contents[ , "File"]),
                   ".html")
    ## </NOTE>

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
        bad <- which(!mapply("%in%", index[, 1L], aliases))
        if(any(bad)) {
            ## was [[, but that applies to lists not char vectors
            tmp <- sapply(aliases[bad], "[", 1L)
            tmp[is.na(tmp)] <- ""
            index[bad, 1L] <- tmp
        }
        ## and sort it by name
        index <- index[sort.list(index[, 1L]), ]
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
    ##
    ## called from R CMD build

    if((length(RdFiles) == 1L) && dir.exists(RdFiles)) {
        ## Compatibility code for the former @code{R CMD Rdindex}
        ## interface.
        docsDir <- RdFiles
        if(dir.exists(file.path(docsDir, "man")))
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
        stop("argument 'outFile' must be a character string or connection")

    db <- .build_Rd_db(files = RdFiles, stages="build")
    index <- .build_Rd_index(Rd_contents(db), type = type)
    writeLines(formatDL(index, width = width, indent = indent), outFile)
}

### * Rd_db

Rd_db <-
function(package, dir, lib.loc = NULL)
{
    ## Build an Rd 'data base' from an installed package or the unpacked
    ## package sources as a list containing the parsed Rd objects.

    ## <NOTE>
    ## We actually also process platform conditionals.
    ## If this was to be changed, we could also need to arrange that Rd
    ## objects in *all* platform specific subdirectories are included.
    ## </NOTE>

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        docs_dir <- file.path(dir, "man")
        ## For an installed package, we might have
        ##
        ## 1) pre-2.10.0-style  man/package.Rd.gz
        ## file with suitable concatenated Rd sources,
        ##
        ## 2) help/package.rd[bx]
        ## with a DB of the parsed (and platform processed, see
        ## above) Rd objects.
        db_file <- file.path(dir, "help", package)
        if(file_test("-f", paste(db_file, "rdx", sep = "."))) {
            db <- fetchRdDB(db_file)
            pathfile <- file.path(dir, "help", "paths.rds")
            if(file.exists(pathfile)) {
                paths <- readRDS(pathfile)
                if(!is.null(first <- attr(paths, "first")))
                    paths <- substring(paths, first)
                names(db) <- paths
            }
            return(db)
        }
        db_file <- file.path(docs_dir, sprintf("%s.Rd.gz", package))
        if(file_test("-f", db_file)) {
            lines <- .read_Rd_lines_quietly(db_file)
            eof_pos <-
                grep("^\\\\eof$", lines, perl = TRUE, useBytes = TRUE)
            db <- split(lines[-eof_pos],
                        rep(seq_along(eof_pos),
                            times = diff(c(0, eof_pos)))[-eof_pos])
        } else return(structure(list(), names = character()))

        ## NB: we only get here for pre-2.10.0 installs

        ## If this was installed using a recent enough version of R CMD
        ## INSTALL, information on source file names is available, and
        ## we use it for the names of the Rd db.  Otherwise, remove the
        ## artificial names attribute.
        paths <- as.character(sapply(db, "[", 1L))
        names(db) <-
            if(length(paths)
               && all(grepl("^% --- Source file: (.+) ---$", paths)))
                sub("^% --- Source file: (.+) ---$", "\\1", paths)
            else
                NULL
        ## Determine package encoding.
        encoding <- .get_package_metadata(dir, TRUE)["Encoding"]
        if(is.na(encoding)) encoding <- "unknown"
        db <- suppressWarnings(lapply(db,
                                      prepare_Rd_from_Rd_lines,
                                      encoding = encoding,
                                      defines = .Platform$OS.type,
                                      stages = "install"))
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!dir.exists(dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        built_file <- file.path(dir, "build", "partial.rdb")
        db <- .build_Rd_db(dir,
                           stages = "build",
                           built_file = built_file)
        if(length(db)) {
            first <- nchar(file.path(dir, "man")) + 2L
            names(db) <- substring(names(db), first)
        }
    }

    db

}

prepare_Rd_from_Rd_lines <-
function(x, ...)
{
    con <- textConnection(x, "rt")
    on.exit(close(con))
    prepare_Rd(con, ...)
}

.build_Rd_db <-
function(dir = NULL, files = NULL,
         encoding = "unknown", db_file = NULL,
         stages = c("build", "install"), os = .OStype(), step = 3L,
         built_file = NULL, macros = character())
{
    if(!is.null(dir)) {
        dir <- file_path_as_absolute(dir)
        macros0 <- loadPkgRdMacros(dir)
        man_dir <- file.path(dir, "man")
        if(!dir.exists(man_dir))
            return(structure(list(), names = character()))
        if(is.null(files))
            files <- list_files_with_type(man_dir, "docs", OS_subdirs=os)
        encoding <- .get_package_metadata(dir, FALSE)["Encoding"]
        if(is.na(encoding)) encoding <- "unknown"
    } else if(!is.null(files))
        macros0 <- initialRdMacros()
    else
        stop("you must specify 'dir' or 'files'")

    if(length(macros)) {
        con <- textConnection(macros)
        macros <- loadRdMacros(con, macros0)
        close(con)
    } else {
        macros <- macros0
    }

    .fetch_Rd_object <- function(f) {
        ## This calls parse_Rd if f is a filename
        Rd <- prepare_Rd(f, encoding = encoding,
                         defines = os,
                         stages = stages, warningCalls = FALSE,
                         stage2 = step > 1L, stage3 = step > 2L,
                         macros = macros)
        structure(Rd, prepared = step)
    }

    if(!is.null(db_file) && file_test("-f", db_file)) {
        ## message("updating database of parsed Rd files")
        db <- fetchRdDB(sub("\\.rdx$", "", db_file))
        db_names <- names(db) <-
            readRDS(file.path(dirname(db_file), "paths.rds"))
        ## Files in the db in need of updating:
        indf <- (files %in% db_names) & file_test("-nt", files, db_file)
        ## Also files not in the db:
        indf <- indf | !(files %in% db_names)

        ## Db elements missing from files:
        ind <- !(db_names %in% files) | (db_names %in% files[indf])
	if(any(ind))
            db <- db[!ind]
	files <- files[indf]
    } else
    	db <- list()

    # The built_file is a file of partially processed Rd objects, where build time
    # \Sexprs have been evaluated.  We'll put the object in place of its
    # filename to continue processing.

    names(files) <- files
    if(!is.null(built_file) && file_test("-f", built_file)) {
        basenames <- basename(files)
 	built <- readRDS(built_file)
 	names_built <- names(built)
 	if ("install" %in% stages) {
 	    this_os <- grepl(paste0("^", os, "/"), names_built)
 	    name_only <- basename(names_built[this_os])
 	    built[name_only] <- built[this_os]
 	    some_os <- grepl("/", names(built))
 	    built <- built[!some_os]
 	    names_built <- names(built)
 	}
 	built[!(names_built %in% basenames)] <- NULL
 	if (length(built)) {
 	    which <- match(names(built), basenames)
 	    if (all(file_test("-nt", built_file, files[which]))) {
 	    	files <- as.list(files)
	    	files[which] <- built
	    }
	}
    }

    if(length(files)) {
        ## message("building database of parsed Rd files")
        db1 <- lapply(files, .fetch_Rd_object)
        names(db1) <- names(files)
        db <- c(db, db1)
    }

    db
}

### * Rd_aliases

## Called from undoc and .check_Rd_xrefs
Rd_aliases <-
function(package, dir, lib.loc = NULL)
{
    ## Get the Rd aliases (topics) from an installed package or the
    ## unpacked package sources.

    if(!missing(package)) {
        dir <- find.package(package, lib.loc)
        rds <- file.path(dir, "Meta", "Rd.rds")
        if(file_test("-f", rds)) {
            aliases <- readRDS(rds)$Aliases
            if(length(aliases)) sort(unlist(aliases)) else character()
        } else
            character()
        ## <NOTE>
        ## Alternatively, we could get the aliases from the help index
        ## (and in fact, earlier versions of this code, then part of
        ## undoc(), did so), along the lines of
        ## <CODE>
        ##   help_index <- file.path(dir, "help", "AnIndex")
        ##   all_doc_topics <- if(!file_test("-f", help_index))
        ##       character()
        ##   else
        ##       sort(scan(file = helpIndex, what = list("", ""),
        ##                 sep = "\t", quote = "", quiet = TRUE,
        ##                 na.strings = character())[[1L]])
        ## </CODE>
        ## This gets all topics the same way as index.search() would
        ## find individual ones.
        ## </NOTE>
    }
    else {
        if(dir.exists(file.path(dir, "man"))) {
            db <- Rd_db(dir = dir)
            aliases <- lapply(db, .Rd_get_metadata, "alias")
            if(length(aliases))
                sort(unique(unlist(aliases, use.names = FALSE)))
            else character()
        }
        else
            character()
    }
}

### .build_Rd_xref_db

.build_Rd_xref_db <-
function(package, dir, lib.loc = NULL)
{
    db <- if(!missing(package))
        Rd_db(package, lib.loc = lib.loc)
    else
        Rd_db(dir = dir)
    lapply(db, .Rd_get_xrefs)
}

### * .Rd_get_metadata

.Rd_get_metadata <-
function(x, kind)
{
    x <- x[RdTags(x) == sprintf("\\%s", kind)]
    if(!length(x))
        character()
    else
        unique(trimws(sapply(x, as.character)))
}

### * .Rd_get_section

.Rd_get_section <-
function(x, which, predefined = TRUE)
{
    if(predefined)
        x <- x[RdTags(x) == paste0("\\", which)]
    else {
        ## User-defined sections are parsed into lists of length 2, with
        ## the elements the title and the body, respectively.
        x <- x[RdTags(x) == "\\section"]
        if(length(x)) {
            ind <- sapply(x, function(e) .Rd_get_text(e[[1L]])) == which
            x <- lapply(x[ind], `[[`, 2L)
        }
    }
    if(!length(x)) x else structure(x[[1L]], class = "Rd")
}

### * .Rd_deparse

.Rd_deparse <-
function(x, tag = TRUE)
{
    ## <NOTE>
    ## This should eventually get an option controlling whether to
    ## escape Rd special characters as needed (thus providing valid Rd)
    ## or not.
    ## It might also be useful to have an option for dropping comments.
    ## </NOTE>
    if(!tag)
        attr(x, "Rd_tag") <- "Rd"
    paste(as.character.Rd(x), collapse = "")
}

### * .Rd_drop_comments

.Rd_drop_comments <-
function(x)
    .Rd_drop_nodes_with_tags(x, "COMMENT")

### * .Rd_drop_nodes_with_tags

.Rd_drop_nodes_with_tags <-
function(x, tags)
{
    recurse <- function(e) {
        if(is.list(e))
            structure(lapply(e[is.na(match(RdTags(e), tags))], recurse),
                      Rd_tag = attr(e, "Rd_tag"))
        else
            e
    }
    recurse(x)
}

### * .Rd_get_argument_names

.Rd_get_argument_names <-
function(x)
{
    x <- .Rd_get_section(x, "arguments")
    if(!length(x)) return(character())
    txt <- .Rd_get_item_tags(x)
    txt <- unlist(strsplit(txt, ", *"))
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\_", "_", txt)
    trimws(txt)
}

### * .Rd_get_argument_table

.Rd_get_argument_table <-
function(x)
{
    x <- .Rd_get_section(x, "arguments")
    if(!length(x)) return(matrix(character(), 0L, 2L))
    ## Extract two-arg \item tags at top level ... non-recursive.
    x <- x[RdTags(x) == "\\item"]
    if(!length(x)) return(matrix(character(), 0L, 2L))
    x <- lapply(x[lengths(x) == 2L], sapply, .Rd_deparse)
    matrix(unlist(x), ncol = 2L, byrow = TRUE)
}

### * .Rd_get_item_tags

.Rd_get_item_tags <-
function(x)
{
    ## Extract two-arg \item tags at top level ... non-recursive.
    x <- x[RdTags(x) == "\\item"]
    out <- lapply(x[lengths(x) == 2L],
                  function(e) .Rd_deparse(e[[1L]]))
    as.character(unlist(out))
}

### * .Rd_get_example_code

.Rd_get_example_code <-
function(x)
{
    x <- .Rd_get_section(x, "examples")
    if(!length(x)) return(character())

    ## Need to remove everything inside \dontrun (and drop comments),
    ## and "undefine" \dontshow and \testonly (which is achieved by
    ## changing the Rd tag to "Rd").

    ## <FIXME>
    ## Remove eventually.
    x <- .Rd_drop_comments(x)
    ## </FIXME>

    recurse <- function(e) {
        if(!is.null(tag <- attr(e, "Rd_tag"))
           && tag %in% c("\\dontshow", "\\testonly"))
            attr(e, "Rd_tag") <- "Rd"
        if(is.list(e)) {
            structure(lapply(e[is.na(match(RdTags(e), "\\dontrun"))],
                             recurse),
                      Rd_tag = attr(e, "Rd_tag"))
        }
        else e
    }

    .Rd_deparse(recurse(x), tag = FALSE)
}

### * .Rd_get_methods_description_table

.Rd_get_methods_description_table <-
function(x)
{
    y <- matrix(character(), 0L, 2L)
    x <- .Rd_get_section(x, "Methods", FALSE)
    if(!length(x)) return(y)
    x <- .Rd_get_section(x, "describe")
    if(!length(x)) return(y)
    x <- x[RdTags(x) == "\\item"]
    if(!length(x)) return(y)
    x <- lapply(x[lengths(x) == 2L], sapply, .Rd_deparse)
    matrix(unlist(x), ncol = 2L, byrow = TRUE)
}

### * .Rd_get_doc_type

.Rd_get_doc_type <-
function(x)
{
    c(attr(x, "meta")$docType, .Rd_get_metadata(x, "docType"), "")[1L]
}

### * .Rd_get_name

.Rd_get_name <-
function(x)
{
    x <- .Rd_get_section(x, "name")
    ## The name should really be plain text, so as.character() should be
    ## fine as well ...
    if(length(x))
        trimws(.Rd_deparse(x, tag = FALSE))
    else
        character()
}

### * .Rd_get_title

.Rd_get_title <-
function(x)
{
    title <- .Rd_get_section(x, "title")

    result <- character()
    if(length(title)) {
        result <- .Rd_get_text(title)
        result <- result[nzchar(result)]
    }
    paste(result, collapse=" ")
}

### * .Rd_get_text

# Return display form of text, encoded in UTF-8.  Note that
# textConnection converts to the local encoding, and we convert back,
# so unrepresentable characters will be lost

.Rd_get_text <-
function(x) {
    # Handle easy cases first
    if (is.character(x)) return(c(x))

    # We'd like to use capture.output here, but don't want to depend
    # on utils, so we duplicate some of it
    rval <- NULL
    file <- textConnection("rval", "w", local = TRUE)

    save <- options(useFancyQuotes = FALSE)
    Rdsave <- Rd2txt_options(underline_titles = FALSE)
    sink(file)
    tryCatch(Rd2txt(x, fragment=TRUE),
             finally = {sink()
                        options(save)
                        Rd2txt_options(Rdsave)
                        close(file)})

    if (is.null(rval)) rval <- character()
    else enc2utf8(rval)
}

### * .Rd_get_xrefs

.Rd_get_xrefs <-
function(x)
{
    out <- matrix(character(), nrow = 0L, ncol = 2L)
    recurse <- function(e) {
        tag <- attr(e, "Rd_tag")
        if(identical(tag, "\\link")) {
            val <- if(length(e)) { # mvbutils has empty links
                arg <- as.character(e[[1L]])
                opt <- attr(e, "Rd_option")
                c(arg, if(is.null(opt)) "" else as.character(opt))
            } else c("", "")
            out <<- rbind(out, val)
        } else if(identical(tag, "\\linkS4class")) {
            arg <- as.character(e[[1L]])
            val <- c(arg, sprintf("=%s-class", arg))
            out <<- rbind(out, val)
        }
        if(is.list(e)) lapply(e, recurse)
    }
    lapply(x, recurse)
    dimnames(out) <- list(NULL, c("Target", "Anchor"))
    out
}

### * .Rd_get_names_from_Rd_db

.Rd_get_names_from_Rd_db <-
function(db)
{
    Rd_names <- lapply(db, .Rd_get_name)
    ## If the Rd db was obtained from an installed package, we know that
    ## all Rd objects must have a \name entry---otherwise, Rd_info() and
    ## hence installing the package Rd contents db would have failed.
    ## For Rd dbs created from a package source directory, we now add
    ## the Rd file paths as the names attribute, so that we can point to
    ## the files with missing \name entries.
    idx <- as.integer(lengths(Rd_names)) == 0L
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
            stop(sprintf(ngettext(sum(idx),
                                  "missing/empty \\name field in Rd file\n%s",
                                  "missing/empty \\name field in Rd files\n%s"),
                         paste0("  ", Rd_paths[idx], collapse = "\n")),
                 call. = FALSE, domain = NA)
        }
    }
    unlist(Rd_names)
}

### * .Rd_format_title

.Rd_format_title <-
function(x)
{
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
    x <- gsub("(``|'')", "\"", x)
    x <- gsub("`", "'", x)
    x <- gsub("([[:alnum:]])--([[:alnum:]])", "\\1-\\2", x)
    x <- gsub("\\\\&", "&", x)
    x <- gsub("---", "--", x)
    ## Also remove leading and trailing whitespace.
    trimws(x)
}


### * fetchRdDB

fetchRdDB <-
function(filebase, key = NULL)
{
    fun <- function(db) {
        vals <- db$vals
        vars <- db$vars
        datafile <- db$datafile
        compressed <- db$compressed
        envhook <- db$envhook

        fetch <- function(key)
            lazyLoadDBfetch(vals[key][[1L]], datafile, compressed, envhook)

        if(length(key)) {
            if(! key %in% vars)
                stop(gettextf("No help on %s found in RdDB %s",
                              sQuote(key), sQuote(filebase)),
                     domain = NA)
            fetch(key)
        } else {
            res <- lapply(vars, fetch)
            names(res) <- vars
            res
        }
    }
    res <- lazyLoadDBexec(filebase, fun)
    if (length(key))
        res
    else
        invisible(res)
}

# The macros argument can be TRUE, in which case a new environment is created with an empty parent,
# or the result of a previous call to this function, in which case it becomes the parent,
# or a filename, in which case that file is loaded first, then the new file into a child environment.

# It is not safe to save this environment, as changes to the parser may invalidate its contents.

loadRdMacros <- function(file, macros = TRUE) {
    # New macros are loaded into a clean environment
    if (is.logical(macros) && !macros)
    	stop("'macros' must be TRUE or must specify existing macros")
    Rd <- parse_Rd(file, fragment = TRUE, macros = macros, warningCalls = FALSE)
    for(entry in Rd) {
        bad <- TRUE
	if (is.list(entry)) break
	tag <- attr(entry, "Rd_tag")
	switch(tag,
	    TEXT = if (any(grepl("[^[:space:]]", entry, perl = TRUE, useBytes=TRUE)))
		      break
		   else
		      bad <- FALSE,
	    USERMACRO =,
	    "\\newcommand" =,
	    "\\renewcommand" =,
	    COMMENT = bad <- FALSE,
	    break
	)
    }
    if (bad)
	warning(gettextf("Macro file %s should only contain Rd macro definitions and comments",
	                 file))
    attr(Rd, "macros")
}

initialRdMacros <- function(pkglist = NULL,
                            macros = file.path(R.home("share"), "Rd", "macros", "system.Rd")
                            ) {
    if (length(pkglist)) {
    	others <- trimws(unlist(strsplit(pkglist, ",")))

    	for (p in others) {
    	    if (dir.exists(system.file("help/macros", package = p)))
    	    	macros <- loadPkgRdMacros(system.file(package = p), macros)
    	    else
    	    	warning(gettextf("No Rd macros in package '%s'.", p), call. = FALSE)
        }
    } else if (is.character(macros))
    	macros <- loadRdMacros(file = macros)
    macros
}

loadPkgRdMacros <- function(pkgdir, macros = NULL) {
    ## this does get called on any directory,
    ## e.g. a man directory in package 'diveMove'.
    pkglist <- try(.read_description(file.path(pkgdir, "DESCRIPTION")),
                   silent = TRUE)
    if (inherits(pkglist, "try-error"))
    	pkglist <-  try(.read_description(file.path(pkgdir, "DESCRIPTION.in")),
                        silent = TRUE)
    ## may check for 'macros' subdirectory?
    if (inherits(pkglist, "try-error")) return(macros)

    pkglist <- pkglist["RdMacros"]

    if (is.na(pkglist))
        pkglist <- NULL

    if (is.null(macros))
        macros <- initialRdMacros(pkglist)
    else
        macros <- initialRdMacros(pkglist, macros)

    files <- c(list.files(file.path(pkgdir, "man", "macros"), pattern = "\\.Rd$", full.names = TRUE),
               list.files(file.path(pkgdir, "help", "macros"), pattern = "\\.Rd$", full.names = TRUE))

    for (f in files)
    	macros <- loadRdMacros(f, macros)

    macros
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
