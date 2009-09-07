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

	cmd <- sprintf("aspell pipe %s < %s", control, tfile)

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

aspell_filter_db <- new.env()
aspell_filter_db$Rd <- tools::RdTextFilter
aspell_filter_db$Sweave <- tools::SweaveTeXFilter
