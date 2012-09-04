#  File src/library/utils/R/aspell.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/


aspell <-
function(files, filter, control = list(), encoding = "unknown",
         program = NULL, dictionaries = character())
{
    ## Take the given files and feed them through spell checker in
    ## Ispell pipe mode.

    ## Think about options and more command line options eventually.

    program <- aspell_find_program(program)
    if(is.na(program))
        stop("No suitable spell check program found.")

    ## Be nice.
    if(inherits(files, "Rd"))
        files <- list(files)

    files_are_names <- is.character(files)

    filter_args <- list()
    if(missing(filter) || is.null(filter)) {
        filter <- if(!files_are_names) {
            function(ifile, encoding) {
                if(inherits(ifile, "srcfile"))
                    readLines(ifile$filename, encoding = encoding,
                              warn = FALSE)
                else if(inherits(ifile, "connection"))
                    readLines(ifile, encoding = encoding)
                else {
                    ## What should this do with encodings?
                    as.character(ifile)
                }
            }
        }
        else NULL
    }
    else if(is.character(filter)) {
        ## Look up filter in aspell filter db.
        filter_name <- filter[1L]
        filter <- aspell_filter_db[[filter_name]]
        ## Warn if the filter was not found in the db.
        if(is.null(filter))
            warning(gettextf("Filter '%s' is not available.",
                             filter_name),
                    domain = NA)
    }
    else if(is.list(filter)) {
        ## Support
        ##   list("Rd", drop = "\\references"
        ## at least for now.
        filter_name <- filter[[1L]][1L]
        filter_args <- filter[-1L]
        filter <- aspell_filter_db[[filter_name]]
        ## Warn if the filter was not found in the db.
        if(is.null(filter))
            warning(gettextf("Filter '%s' is not available.",
                             filter_name),
                    domain = NA)
    }
    else if(!is.function(filter))
        stop("Invalid 'filter' argument.")

    encoding <- rep(encoding, length.out = length(files))

    verbose <- getOption("verbose")

    db <- data.frame(Original = character(), File = character(),
                     Line = integer(), Column = integer(),
                     stringsAsFactors = FALSE)
    db$Suggestions <- list()

    tfile <- tempfile("aspell")
    on.exit(unlink(tfile))

    if(length(dictionaries)) {
        paths <- aspell_find_dictionaries(dictionaries)
        ind <- paths == ""
        if(any(ind)) {
            warning(gettextf("The following dictionaries were not found:\n%s",
                             paste(sprintf("  %s", dictionaries[ind]),
                                   collapse = "\n")),
                    domain = NA)
            paths <- paths[!ind]
        }
        if(length(paths)) {
            words <- unlist(lapply(paths, readRDS), use.names = FALSE)
            personal <- tempfile("aspell_personal")
            on.exit(unlink(personal), add = TRUE)
            ## <FIXME>
            ## How can we get the right language set (if needed)?
            ## Maybe aspell() needs an additional 'language' arg?
            aspell_write_personal_dictionary_file(words, personal,
                                                  program = program)
            ## </FIXME>
            control <- c(control, "-p", shQuote(personal))
        }
    }

    ## No special expansion of control argument for now.
    control <- paste(as.character(control), collapse = " ")

    fnames <- names(files)
    files <- as.list(files)

    for (i in seq_along(files)) {

        file <- files[[i]]
        if(files_are_names)
            fname <- file
        else {
            ## Try srcfiles and srcrefs ...
            fname <- if(inherits(file, "srcfile"))
                file$filename
            else
                attr(attr(file, "srcref"), "srcfile")$filename
            ## As a last resort, try the names of the files argument.
            if(is.null(fname))
                fname <- fnames[i]
            ## If unknown ...
            if(is.null(fname))
                fname <- "<unknown>"
        }

        enc <- encoding[i]

        if(verbose)
            message(gettextf("Processing file %s", fname),
                    domain = NA)

        lines <- if(is.null(filter))
            readLines(file, encoding = enc)
        else {
            ## Assume that filter takes an input file (and additional
            ## arguments) and return a character vector.
            do.call(filter, c(list(file, encoding = enc), filter_args))
        }

        ## Need to escape all lines with carets to ensure Aspell handles
        ## them as data: the Aspell docs say
        ##   It is recommended that programmatic interfaces prefix every
        ##   data line with an uparrow to protect themselves against
        ##   future changes in Aspell.
        writeLines(paste0("^", lines), tfile)
        ## Note that this re-encodes character strings with marked
        ## encodings to the current encoding (which is definitely fine
        ## if this is UTF-8 and Aspell was compiled with full UTF-8
        ## support).  Alternatively, we could try using something along
        ## the lines of
        ##   writeLines(paste0("^", lines), tfile,
        ##              useBytes = TRUE)
        ## ## Pass encoding info to Aspell in case we know it.
        ## if(!is.null(filter))  {
        ##     enc <- unique(Encoding(lines))
        ##     enc <- enc[enc != "unknown"]
        ##     if(length(enc) != 1L)
        ##         enc <- "unknown"
        ## }
        ## ## But only if we know how to tell Aspell about it.
        ## enc <- switch(enc,
        ##               "UTF-8" = "utf-8",
        ##               "latin1" = "iso-8859-1",
        ##               "unknown")
	## cmd <- sprintf("aspell pipe %s < %s",
        ##                if(enc == "unknown") control
        ##                else sprintf("%s --encoding=%s", control, enc),
        ##                tfile)

        cmd <- sprintf("%s -a %s < %s", shQuote(program), control, tfile)

	out <- tools:::.shell_with_capture(cmd)

	if(out$status != 0L)
	    stop(gettextf("Running aspell failed with diagnostics:\n%s",
			  paste(out$stderr, collapse = "\n")),
                 domain = NA)

	## Hopefully everything worked ok.
	lines <- out$stdout[-1L]
	pos <- cumsum(lines == "") + 1L

	## Format is as follows.
	## First line is a header.
	## Blank lines separate the results for each line.
	## Results for the word on each line are given as follows.
	## * If the word was found in the main dictionary, or your personal
	##   dictionary, then the line contains only a `*'.
	## * If the word is not in the dictionary, but there are
	##   suggestions, then the line contains an `&', a space, the
	##   misspelled word, a space, the number of near misses, the number
	##   of characters between the beginning of the line and the
	##   beginning of the misspelled word, a colon, another space, and a
	##   list of the suggestions separated by commas and spaces.
	## * If the word does not appear in the dictionary, and there are no
	##   suggestions, then the line contains a `#', a space, the
	##   misspelled word, a space, and the character offset from the
	##   beginning of the line.
	## This can be summarized as follows:
	##   OK: *
	##   Suggestions: & original count offset: miss, miss, ...
	##   None: # original offset

	## Look at words not in dictionary with suggestions.

	ind <- grepl("^&", lines)
	if(any(ind)) {
	    info <- strsplit(lines[ind], ": ", fixed = TRUE)
	    one <- strsplit(sapply(info, `[`, 1L), " ",  fixed = TRUE)
	    two <- strsplit(sapply(info, `[`, 2L), ", ", fixed = TRUE)
	    db1 <- data.frame(Original =
			      as.character(sapply(one, `[`, 2L)),
			      File = fname,
			      Line = pos[ind],
			      Column =
			      as.integer(sapply(one, `[`, 4L)),
			      stringsAsFactors = FALSE)
	    db1$Suggestions <- two
	    db <- rbind(db, db1)
	}
	## Looks at words not in dictionary with no suggestions.
	ind <- grepl("^#", lines)
	if(any(ind)) {
	    one <- strsplit(lines[ind], " ", fixed = TRUE)
	    db1 <- data.frame(Original =
			      as.character(sapply(one, `[`, 2L)),
			      File = fname,
			      Line = pos[ind],
			      Column =
			      as.integer(sapply(one, `[`, 3L)),
			      stringsAsFactors = FALSE)
	    db1$Suggestions <- vector("list", length(one))
	    db <- rbind(db, db1)
	}
    }

    class(db) <- c("aspell", "data.frame")
    db
}

print.aspell <-
function(x, sort = TRUE, verbose = FALSE, indent = 2L, ...)
{
    ## A very simple printer ...
    if(!(nr <- nrow(x))) return(invisible(x))

    if (sort)
    	x <- x[order(x$Original, x$File, x$Line, x$Column), ]

    if (verbose)
    	out <-
    	    sprintf("%sWord: %s (%s:%d:%d)\n%s",
    	            c("", rep.int("\n", nr - 1L)),
    	            x$Original, x$File, x$Line, x$Column,
    	            formatDL(rep.int("Suggestions", nr),
    	                     sapply(x$Suggestions, paste, collapse = " "),
    	                     style = "list"))
    else {
        s <- split(sprintf("%s:%d:%d", x$File, x$Line, x$Column),
                   x$Original)
        sep <- sprintf("\n%s",
                       paste(rep.int(" ", indent), collapse = ""))
        out <- paste(names(s),
                     sapply(s, paste, collapse = sep),
                     sep = sep, collapse = "\n\n")
    }
    writeLines(out)
    invisible(x)
}

summary.aspell <-
function(object, ...)
{
    words <- sort(unique(object$Original))
    if(length(words)) {
        writeLines("Possibly mis-spelled words:")
        print(words)
    }
    invisible(words)
}

aspell_filter_db <- new.env(hash = FALSE) # small
aspell_filter_db$Rd <- tools::RdTextFilter
aspell_filter_db$Sweave <- tools::SweaveTeXFilter

aspell_find_program <-
function(program = NULL)
{
    check <- !is.null(program) || !is.null(names(program))
    if(is.null(program))
        program <- getOption("aspell_program")
    if(is.null(program))
        program <- c("aspell", "hunspell", "ispell")
    program <- Filter(nzchar, Sys.which(program))[1L]
    if(!is.na(program) && check) {
        out <- c(system(sprintf("%s -v", program),
                        intern = TRUE), "")[1L]
        if(grepl("really Aspell", out))
            names(program) <- "aspell"
        else if(grepl("really Hunspell", out))
            names(program) <- "hunspell"
        else if(grepl("International Ispell", out))
            names(program) <- "ispell"
        else
            names(program) <- NA_character_
    }
    program
}

aspell_find_dictionaries <-
function(dictionaries)
{
    paths <- as.character(dictionaries)
    if(!(n <- length(paths))) return(NULL)

    ## For now, all dictionary files should be .rds files.
    ind <- !grepl("\\.rds$", paths)
    if(any(ind))
        paths[ind] <- sprintf("%s.rds", paths[ind])

    ## Dictionaries with no path separators are looked for in the R
    ## system dictionary directory.
    pos <- grep(.Platform$file.sep, paths, fixed = TRUE)
    ## (Equivalently, could check where paths == basename(paths).)
    if(length(pos) < n)
        paths[-pos] <-
            file.path(R.home("share"), "dictionaries", paths[-pos])
    if(length(pos))
        paths[pos] <- path.expand(paths[pos])

    paths[!file_test("-f", paths)] <- ""

    paths
}

### Utilities.

aspell_inspect_context <-
function(x)
{
    x <- split(x, x$File)
    y <- Map(function(f, x) {
        lines <- readLines(f, warn = FALSE)[x$Line]
        cbind(f,
              x$Line,
              substring(lines, 1L, x$Column - 1L),
              x$Original,
              substring(lines, x$Column + nchar(x$Original)))
    },
             names(x), x)
    y <- data.frame(do.call(rbind, y), stringsAsFactors = FALSE)
    names(y) <- c("File", "Line", "Left", "Original", "Right")
    class(y) <- c("aspell_inspect_context", "data.frame")
    y
}

print.aspell_inspect_context <-
function(x, ...)
{
    s <- split(x, x$File)
    nms <- names(s)
    for(i in seq_along(s)) {
        e <- s[[i]]
        writeLines(c(sprintf("File '%s':", nms[i]),
                     sprintf("  Line %s: \"%s\", \"%s\", \"%s\"",
                             format(e$Line),
                             gsub("\"", "\\\"", e$Left),
                             e$Original,
                             gsub("\"", "\\\"", e$Right)),
                     ""))
    }
    invisible(x)
}


## For spell-checking the R manuals:

## This can really only be done with Aspell as the other checkers have
## no texinfo mode.

aspell_control_R_manuals <-
    list(aspell =
         c("--master=en_US",
           "--add-extra-dicts=en_GB",
           "--mode=texinfo",
           "--add-texinfo-ignore=acronym",
           "--add-texinfo-ignore=deftypefun",
           "--add-texinfo-ignore=deftypefunx",
           "--add-texinfo-ignore=findex",
           "--add-texinfo-ignore=enindex",
           "--add-texinfo-ignore=include",
           "--add-texinfo-ignore=ifclear",
           "--add-texinfo-ignore=ifset",
           "--add-texinfo-ignore=math",
           "--add-texinfo-ignore=macro",
           "--add-texinfo-ignore=multitable",
           "--add-texinfo-ignore=node",
           "--add-texinfo-ignore=pkg",
           "--add-texinfo-ignore=printindex",
           "--add-texinfo-ignore=set",
           "--add-texinfo-ignore=vindex",
           "--add-texinfo-ignore-env=menu",
           "--add-texinfo-ignore=CRANpkg"
           ),
         hunspell =
         c("-d en_US,en_GB"))

aspell_R_manuals <-
function(which = NULL, dir = NULL, program = NULL)
{
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    ## Allow specifying 'R-exts' and alikes, or full paths.
    files <- if(is.null(which)) {
        Sys.glob(file.path(dir, "doc", "manual", "*.texi"))
    } else {
        ind <- which(which ==
                     basename(tools::file_path_sans_ext(which)))
        which[ind] <-
            file.path(dir, "doc", "manual",
                      sprintf("%s.texi", which[ind]))
        which
    }

    program <- aspell_find_program(program)

    aspell(files,
           control = aspell_control_R_manuals[[names(program)]],
           program = program)
}

## For spell-checking the R Rd files:

aspell_control_R_Rd_files <-
    list(aspell =
         c("--master=en_US",
           "--add-extra-dicts=en_GB"),
         hunspell =
         c("-d en_US,en_GB"))

aspell_R_Rd_files <-
function(which = NULL, dir = NULL, drop = "\\references",
         program = NULL)
{
    files <- character()
    
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    
    if(is.null(which)) {
        which <- tools:::.get_standard_package_names()$base
        # CHANGES.Rd could be dropped from checks in the future;
        # it will not be updated post 2.15.0
        files <- c(file.path(dir, "doc", "NEWS.Rd"),
                   file.path(dir, "src", "gnuwin32", "CHANGES.Rd"))
        files <- files[file_test("-f", files)]
    }

    files <-
        c(files,
          unlist(lapply(file.path(dir, "src", "library", which, "man"),
                        tools::list_files_with_type,
                        "docs", OS_subdirs = c("unix", "windows")),
                 use.names = FALSE))

    program <- aspell_find_program(program)

    aspell(files,
           filter = list("Rd", drop = drop),
           control = aspell_control_R_Rd_files[[names(program)]],
           program = program)
}

## For spell-checking Rd files in a package:

aspell_package_Rd_files <-
function(dir, drop = c("\\author", "\\references"), control = list(),
         program = NULL)
{
    dir <- tools::file_path_as_absolute(dir)

    subdir <- file.path(dir, "man")
    files <- if(file_test("-d", subdir))
        tools::list_files_with_type(subdir,
                                    "docs",
                                    OS_subdirs = c("unix", "windows"))
    else character()

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    defaults <- .aspell_package_defaults(dir, encoding)$Rd_files
    if(!is.null(defaults)) {
        ## For now, allow providing defaults for drop/control directly,
        ## and for setting the personal dictionary (without hard-wiring
        ## the file path).  Direct settings currently override (could
        ## add a list add = TRUE mechanism eventually).
        if(!is.null(d <- defaults$drop))
            drop <- d
        if(!is.null(d <- defaults$control))
            control <- d
        if(!is.null(d <- defaults$personal))
            control <- c(control,
                         sprintf("-p %s",
                                 shQuote(file.path(dir, ".aspell", d))))
        if(!is.null(d <- defaults$program))
            program <- d
    }

    aspell(files,
           filter = list("Rd", drop = drop),
           control = control,
           encoding = encoding,
           program = program)
}

## For spell-checking the R vignettes:

## This should really be done with Aspell as the other checkers have far
## less powerful TeX modes.

aspell_control_R_vignettes <-
    list(aspell =
         c("--mode=tex",
           "--master=en_US",
           "--add-extra-dicts=en_GB",
           "--add-tex-command='code p'",
           "--add-tex-command='pkg p'",
           "--add-tex-command='CRANpkg p'"
           ),
         hunspell =
         c("-t", "-d en_US,en_GB"))

aspell_R_vignettes <-
function(program = NULL)
{
    files <- Sys.glob(file.path(tools:::.R_top_srcdir_from_Rd(),
                                "src", "library", "*", "vignettes",
                                "*.Rnw"))

    program <- aspell_find_program(program)

    aspell(files,
           filter = "Sweave",
           control = aspell_control_R_vignettes[[names(program)]],
           program = program)
}

## For spell-checking vignettes in a package:

## This should really be done with Aspell as the other checkers have far
## less powerful TeX modes.

aspell_control_package_vignettes <-
    list(aspell =
         c("--add-tex-command='citep oop'",
           "--add-tex-command='Sexpr p'",
           "--add-tex-command='code p'",
           "--add-tex-command='pkg p'",
           "--add-tex-command='proglang p'",
           "--add-tex-command='samp p'"
           ))

aspell_package_vignettes <-
function(dir, control = list(), program = NULL)
{
    dir <- tools::file_path_as_absolute(dir)

    subdir <- file.path(dir, "inst", "doc")
    files <- if(file_test("-d", subdir))
        tools::list_files_with_type(subdir, "vignette")
    else character()

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    defaults <- .aspell_package_defaults(dir, encoding)$vignettes
    if(!is.null(defaults)) {
        if(!is.null(d <- defaults$control))
            control <- d
        if(!is.null(d <- defaults$personal))
            control <- c(control,
                         sprintf("-p %s",
                                 shQuote(file.path(dir, ".aspell", d))))
        if(!is.null(d <- defaults$program))
            program <- d
    }

    program <- aspell_find_program(program)

    aspell(files,
           filter = "Sweave",
           control =
           c("-t",
             aspell_control_package_vignettes[[names(program)]],
             control),
           program = program)
}

## Spell-checking R files.

aspell_filter_db$R <-
function(ifile, encoding = "unknown", ignore = character())
{
    pd <- get_parse_data_for_message_strings(ifile, encoding)
    if(is.null(pd) || !NROW(pd)) return(character())

    ## Strip the string delimiters.
    pd$text <- substring(pd$text, 2L, nchar(pd$text) - 1L)

    lines <- readLines(ifile, encoding = encoding)

    ## Column positions in the parse data have tabs expanded to tab
    ## stops using a tab width of 8, so for lines with tabs we need to
    ## map the column positions back to character positions.
    lines_in_pd <- sort(unique(c(pd$line1, pd$line2)))
    tab <- Map(function(tp, nc) {
        if(tp[1L] == -1L) return(NULL)
        widths <- rep.int(1, nc)
        for(i in tp) {
            cols <- cumsum(widths)
            widths[i] <- 8 - (cols[i] - 1) %% 8
        }
        cumsum(c(1, widths))
    },
               gregexpr("\t", lines[lines_in_pd], fixed = TRUE),
               nchar(lines[lines_in_pd]))
    names(tab) <- lines_in_pd        

    lines[lines_in_pd] <- gsub("[^\t]", " ", lines[lines_in_pd])
    lines[-lines_in_pd] <- ""

    for(entry in split(pd, seq_len(NROW(pd)))) {
        line1 <- entry$line1
        line2 <- entry$line2
        col1 <- entry$col1 + 1L
        col2 <- entry$col2 - 1L
        if(line1 == line2) {
            if(length(ptab <- tab[[as.character(line1)]])) {
                col1 <- which(ptab == col1)
                col2 <- which(ptab == col2)
            }
            substring(lines[line1], col1, col2) <- entry$text
        } else {
            texts <- unlist(strsplit(entry$text, "\n", fixed = TRUE))
            n <- length(texts)
            if(length(ptab <- tab[[as.character(line1)]])) {
                col1 <- which(ptab == col1)
            }
            substring(lines[line1], col1) <- texts[1L]
            pos <- seq(from = 2, length.out = n - 2)
            if(length(pos))
                lines[line1 + pos - 1] <- texts[pos]
            if(length(ptab <- tab[[as.character(line2)]])) {
                col2 <- which(ptab == col2)
            }
            substring(lines[line2], 1L, col2) <- texts[n]
        }
    }
    
    for(re in ignore[nzchar(ignore)])
        lines <- blank_out_regexp_matches(lines, re)

    lines
}

get_parse_data_for_message_strings <-
function(file, encoding = "unknown")
{
    ## The message strings considered are the string constants subject to
    ## translation in gettext-family calls (see below for details).
    
    oop <- options(keep.source = TRUE)
    on.exit(options(oop))

    exprs <- parse(file = file, encoding = encoding)
    if(!length(exprs)) return(NULL)

    pd <- getParseData(exprs)

    ## Function for computing grandparent ids.    
    parents <- pd$parent
    names(parents) <- pd$id
    gpids <- function(ids)
        parents[as.character(parents[as.character(ids)])]

    ind <- (pd$token == "SYMBOL_FUNCTION_CALL") &
        !is.na(match(pd$text,
                     c("warning", "stop",
                       "message", "packageStartupMessage", 
                       "gettext", "gettextf", "ngettext")))
    
    funs <- pd$text[ind]

    ids <- gpids(pd$id[ind])
    calls <- getParseText(pd, ids)

    table <- pd[pd$token == "STR_CONST", ]
    pos <- match(gpids(table$id), ids)
    ind <- !is.na(pos)
    table <- split(table[ind, ], factor(pos[ind], seq_along(ids)))

    ## We have synopses
    ##   message(..., domain = NULL, appendLF = TRUE)
    ##   packageStartupMessage(..., domain = NULL, appendLF = TRUE)
    ##   warning(..., call. = TRUE, immediate. = FALSE, domain = NULL)
    ##   stop(..., call. = TRUE, domain = NULL)
    ##   gettext(..., domain = NULL)
    ##   ngettext(n, msg1, msg2, domain = NULL)
    ##   gettextf(fmt, ..., domain = NULL)
    ## For the first five, we simply take all unnamed strings.
    ## (Could make this more precise, of course.)
    ## For the latter two, we take the msg1/msg2 and fmt arguments,
    ## provided these are strings.

    ## <NOTE>
    ## Using domain = NA inhibits translation: perhaps it should
    ## optionally also inhibit spell checking?
    ## </NOTE>

    extract_message_strings <- function(fun, call, table) {
        ## Matching a call containing ... gives
        ##   Error in match.call(message, call) : 
        ##   ... used in a situation where it doesn't exist
        ## so eliminate these.
        ## (Note that we also drop "..." strings.)
        call <- parse(text = call)[[1L]]
        call <- call[ as.character(call) != "..." ]
        mc <- as.list(match.call(get(fun, envir = .BaseNamespaceEnv),
                                 call))
        args <- if(fun == "gettextf")
            mc["fmt"]
        else if(fun == "ngettext")
            mc[c("msg1", "msg2")]
        else {
            if(!is.null(names(mc)))
                mc <- mc[!nzchar(names(mc))]
            mc[-1L]
        }
        strings <- as.character(args[vapply(args, is.character, TRUE)])
        ## Need to canonicalize to match string constants before and
        ## after parsing ...
        texts <- vapply(parse(text = table$text), as.character, "")
        table[!is.na(match(texts, strings)), ]
    }

    do.call(rbind,
            Map(extract_message_strings,
                as.list(funs), as.list(calls), table))
}

## For spell-checking the R R files.

aspell_R_R_files <-
function(which = NULL, dir = NULL,
         ignore = c("[ \t]'[^']*'[ \t[:punct:]]",
                    "[ \t][[:alnum:]_.]*\\(\\)[ \t[:punct:]]"),
         program = NULL)
{
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    if(is.null(which))
        which <- tools:::.get_standard_package_names()$base
    
    files <-
        unlist(lapply(file.path(dir, "src", "library", which, "R"),
                      tools::list_files_with_type,
                      "code",
                      OS_subdirs = c("unix", "windows")),
               use.names = FALSE)

    program <- aspell_find_program(program)

    aspell(files,
           filter = list("R", ignore = ignore),
           control = aspell_control_R_Rd_files[[names(program)]],
           program = program)
}

## For spell-checking R files in a package.
  
aspell_package_R_files <-
function(dir, ignore = "", control = list(), program = NULL)
{
    dir <- tools::file_path_as_absolute(dir)

    subdir <- file.path(dir, "R")
    files <- if(file_test("-d", subdir))
        tools::list_files_with_type(subdir,
                                    "code",
                                    OS_subdirs = c("unix", "windows"))
    else character()

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    ## <FIXME>
    ## Add support for defaults eventually ...
    ##   defaults <- .aspell_package_defaults(dir, encoding)$R_files
    ## </FIXME>

    program <- aspell_find_program(program)    

    aspell(files,
           filter = list("R", ignore = ignore),
           control = control,
           encoding = encoding,
           program = program)
}

## Spell-checking pot files.

## (Of course, directly analyzing the message strings would be more
## useful, but require writing appropriate text filters.)

## See also tools:::checkPoFile().

aspell_filter_db$pot <-
function (ifile, encoding = "unknown", ignore = character()) 
{
    lines <- readLines(ifile, encoding = encoding)

    ind <- grepl("^msgid[ \t]", lines)

    do_entry <- function(s) {
        out <- character(length(s))
        i <- 1L
        out[i] <- blank_out_regexp_matches(s[i], "^msgid[ \t]+\"")
        while(grepl("^\"", s[i <- i + 1L]))
            out[i] <- sub("^\"", " ", s[i])
        if(grepl("^msgid_plural[ \t]", s[i])) {
            out[i] <- blank_out_regexp_matches(s[i], "^msgid_plural[ \t]+\"")
            while(grepl("^\"", s[i <- i + 1L]))
                out[i] <- sub("^\"", " ", s[i])
        }
        out
    }

    entries <- split(lines, cumsum(ind))
    lines <- c(character(length(entries[[1L]])),
               as.character(do.call(c, lapply(entries[-1L], do_entry))))

    lines <- sub("\"[ \t]*$", " ", lines)
    
    for(re in ignore[nzchar(ignore)])
        lines <- blank_out_regexp_matches(lines, re)

    lines
}

## For spell-checking pot files in a package.

aspell_package_pot_files <-
function(dir, ignore = "", control = list(), program = NULL)
{
    dir <- tools::file_path_as_absolute(dir)
    subdir <- file.path(dir, "po")
    files <- if(file_test("-d", subdir))
        Sys.glob(file.path(subdir, "*.pot"))
    else character()

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    program <- aspell_find_program(program)
    
    aspell(files,
           filter = list("pot", ignore = ignore),
           control = control,
           encoding = encoding,
           program = program)
}

## For spell-checking the R C files.

aspell_R_C_files <-
function(which = NULL, dir = NULL,
         ignore = c("[ \t]'[[:alnum:]_.]*'[ \t[:punct:]]",
                    "[ \t][[:alnum:]_.]*\\(\\)[ \t[:punct:]]"),
         program = NULL)
{
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    if(is.null(which))
        which <- tools:::.get_standard_package_names()$base
    if(!is.na(pos <- match("base", which)))
        which[pos] <- "R"

    files <- sprintf("%s.pot",
                     file.path(dir, "src", "library",
                               which, "po", which))
    files <- files[file_test("-f", files)]
    
    program <- aspell_find_program(program)

    aspell(files,
           filter = list("pot", ignore = ignore),
           control = aspell_control_R_Rd_files[[names(program)]],
           program = program)
}

## Spell-checking DCF files.

aspell_filter_db$dcf <-
function(ifile, encoding, keep = c("Title", "Description"))
{
    lines <- readLines(ifile, encoding = encoding)
    line_has_tags <- grepl("^[^[:blank:]][^:]*:", lines)
    tags <- sub(":.*", "", lines[line_has_tags])
    lines <- split(lines, cumsum(line_has_tags))
    ind <- is.na(match(tags, keep)) 
    lines[ind] <- lapply(lines[ind], function(s) rep.int("", length(s)))
    unlist(lines, use.names = FALSE)
}

## For spell-checking package DESCRIPTION files.

aspell_package_description <-
function(dir, control = list(), program = NULL)
{
    dir <- tools::file_path_as_absolute(dir)
    files <- file.path(dir, "DESCRIPTION")

    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if(is.na(encoding <- meta["Encoding"]))
        encoding <- "unknown"

    program <- aspell_find_program(program)
    
    aspell(files, filter = "dcf", control = control,
           encoding = encoding, program = program)
}

## For writing personal dictionaries:

aspell_write_personal_dictionary_file <-
function(x, out, language = "en", program = NULL)
{
    if(inherits(x, "aspell"))
        x <- sort(unique(x$Original))

    program <- aspell_find_program(program)
    if(is.na(program))
        stop("No suitable spell check program found.")

    ## <NOTE>
    ## Ispell and Hunspell take simple word lists as personal dictionary
    ## files, but Aspell requires a special format, see e.g.
    ## http://aspell.net/man-html/Format-of-the-Personal-and-Replacement-Dictionaries.html
    ## and one has to create these by hand, as
    ##   aspell --lang=en create personal ./foo "a b c"
    ## gives: Sorry "create/merge personal" is currently unimplemented.

    ## Encodings are a nightmare.
    ## Try to canonicalize to UTF-8 for Aspell (which allows recording
    ## the encoding in the personal dictionary).
    ## <FIXME>
    ## What should we do for Hunspell (which can handle UTF-8, but has
    ## no encoding information in the personal dictionary), or Ispell
    ## (which cannot handle UTF-8)?
    ## </FIXME>
    
    if(names(program) == "aspell") {
        header <- sprintf("personal_ws-1.1 %s %d UTF-8",
                          language, length(x))
        x <- enc2utf8(x)
    }
    else {
        header <- NULL
    }

    writeLines(c(header, x), out, useBytes = TRUE)
}

## For reading package defaults:

.aspell_package_defaults <-
function(dir, encoding = "unknown")
{
    dfile <- file.path(dir, ".aspell", "defaults.R")
    if(!file_test("-f", dfile))
        return(NULL)
    exprs <- parse(dfile, encoding = encoding)
    envir <- new.env()
    for(e in exprs) eval(e, envir)
    as.list(envir)
}

## Utilities.

blank_out_regexp_matches <-
function(s, re)
{
    m <- gregexpr(re, s)
    regmatches(s, m) <- Map(blanks, lapply(regmatches(s, m), nchar))
    s
}

blanks <-
function(n) {
    vapply(Map(rep.int, rep.int(" ", length(n)), n, USE.NAMES = FALSE),
           paste, "", collapse = "")
}
