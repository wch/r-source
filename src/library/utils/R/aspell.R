aspell <-
function(files, filter, control = list(), encoding = "unknown")
{
    ## Take the given files and feed them through aspell in pipe mode.

    ## Think about options and more command line options eventually.

    filter_args <- list()
    if(missing(filter) || is.null(filter))
        filter <- NULL
    else if(is.character(filter)) {
        ## Look up filter in aspell filter db.
        filter <- aspell_filter_db[[filter[1L]]]
        ## Could warn if the filter was not found in the db.
    }
    else if(is.list(filter)) {
        ## Support
        ##   list("Rd", drop = "\\references"
        ## at least for now.
        filter_args <- filter[-1L]
        filter <- aspell_filter_db[[filter[[1L]][1L]]]
        ## Could warn if the filter was not found in the db.
    }
    else if(!is.function(filter))
        stop("Invalid 'filter' argument.")

    ## No special expansion of control argument for now.
    control <- paste(as.character(control), collapse = " ")

    encoding <- rep(encoding, length.out = length(files))
    
    db <- data.frame(Original = character(), File = character(),
                     Line = integer(), Column = integer(),
                     stringsAsFactors = FALSE)
    db$Suggestions <- list()
    
    tfile <- tempfile("aspell")
    on.exit(unlink(tfile))
    
    for (i in seq_along(files)) {

        file <- files[i]
        enc <- encoding[i]

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
        writeLines(paste("^", lines, sep = ""), tfile)
        ## Note that this re-encodes character strings with marked
        ## encodings to the current encoding (which is definitely fine
        ## if this is UTF-8 and Aspell was compiled with full UTF-8
        ## support).  Alternatively, we could try using something along
        ## the lines of
        ##   writeLines(paste("^", lines, sep = ""), tfile,
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

        exe <- Sys.which("aspell")
        if(exe == "") {
            exe <- Sys.which("ispell")
            if(exe == "")
                stop("Could not find aspell or ispell executable.")
        }
        
        cmd <- sprintf("%s -a %s < %s", shQuote(exe), control, tfile)

	out <- tools:::.shell_with_capture(cmd)

	if(out$status != 0L)
	    stop(gettextf("Running aspell failed with diagnostics:\n%s",
			  paste(out$stderr, collapse = "\n")))

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
			      File = file,
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
			      File = file,
			      Line = pos[ind],
			      Column =
			      as.integer(sapply(one, `[`, 3L)),
			      stringsAsFactors = FALSE)
	    db1$Suggestions <- vector("list", length(one))
	    db <- rbind(db, db1)
	}
    }
    structure(db, class = c("aspell", "data.frame"))
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

aspell_filter_db <- new.env()
aspell_filter_db$Rd <- tools::RdTextFilter
aspell_filter_db$Sweave <- tools::SweaveTeXFilter

### Utilities.

## For spell-checking the R manuals:

aspell_control_R_manuals <-
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
      "--add-texinfo-ignore-env=menu"
      )
    
aspell_R_manuals <-
function(which = NULL, dir = NULL)
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
    
    aspell(files,
           control = aspell_control_R_manuals)
}

## For spell-checking the R Rd files:

aspell_control_R_Rd_files <-
    c("--master=en_US",
      "--add-extra-dicts=en_GB")

aspell_R_Rd_files <-
function(which = NULL, dir = NULL, drop = "\\references")
{
    if(is.null(dir)) dir <- tools:::.R_top_srcdir_from_Rd()
    if(is.null(which))
        which <- tools:::.get_standard_package_names()$base

    files <-
        unlist(lapply(file.path(dir, "src", "library", which, "man"),
                      tools::list_files_with_type,
                      "docs", OS_subdirs = c("unix", "windows")),
               use.names = FALSE)

    aspell(files,
           filter = list("Rd", drop = drop),
           control = aspell_control_R_Rd_files)
}

## For spell-checking Rd files in a package:

aspell_package_Rd_files <-
function(dir, drop = "\\references", control = list())
{
    dir <- tools::file_path_as_absolute(dir)
    
    man_dir <- file.path(dir, "man")
    files <- if(file_test("-d", man_dir))
        tools::list_files_with_type(man_dir,
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
                         sprintf("--personal=%s",
                                 file.path(dir, ".aspell", d)))
    }
    
    aspell(files,
           filter = list("Rd", drop = drop),
           control = control,
           encoding = encoding)
}

## For spell-checking vignettes:

aspell_control_vignettes <-
    c("--mode=tex",
      "--add-tex-command='citep oop'",
      "--add-tex-command='Sexpr p'",
      "--add-tex-command='code p'",
      "--add-tex-command='pkg p'",
      "--add-tex-command='proglang p'",
      "--add-tex-command='samp p'"
      )

aspell_vignettes <-
function(files, control = list())
    aspell(files,
           filter = "Sweave",
           control = c(aspell_control_vignettes, control))

## For spell-checking the R vignettes:

aspell_control_R_vignettes <-
    c("--mode=tex",
      "--master=en_US",
      "--add-extra-dicts=en_GB",
      "--add-tex-command='code p'",
      "--add-tex-command='pkg p'")

aspell_R_vignettes <-
function()
{
    ## No arguments for now.
    ## Currently, all vignettes are in grid.
    files <- Sys.glob(file.path(tools:::.R_top_srcdir_from_Rd(),
                                "src", "library", "grid", "inst", "doc",
                                "*.Snw"))
    aspell(files,
           filter = "Sweave",
           control = aspell_control_R_vignettes)
}

## For spell-checking vignettes in a package:

aspell_package_vignettes <-
function(dir, control = list())
{
    dir <- file.path(dir, "inst", "doc")
    files <- if(file_test("-d", dir))
        tools::list_files_with_type(dir, "vignette")
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
                         sprintf("--personal=%s",
                                 file.path(dir, ".aspell", d)))
    }
    
    aspell_vignettes(files, control)
}

## For writing personal dictionaries:

aspell_write_personal_dictionary_file <-
function(x, out, language = "en")
{
    if(inherits(x, "aspell"))
        x <- sort(unique(x$Original))

    header <- sprintf("personal_ws-1.1 %s %d", language, length(x))
    ## In case UTF_8 is around ...
    ## This would be nice:
    ##   encodings <- unique(Encoding(x))
    ##   if(identical(encodings[encodings != "unknown"], "UTF-8"))
    ##       header <- paste(header, "UTF-8")
    ## but does not work as we currently do not canonicalized to UTF-8
    ## and do not mark encodings when reading Aspell results which are
    ## documented to be in the current encoding ...
    if(localeToCharset()[1L] == "UTF-8") {
        x <- iconv(x, "", "UTF-8")
        if(any(Encoding(x) == "UTF-8"))
            header <- paste(header, "UTF-8")
    }

    writeLines(c(header, x), out)
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
