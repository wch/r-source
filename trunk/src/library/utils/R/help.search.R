.hsearch_db <- local({
    hdb <- NULL
    function(new) {
	if(!missing(new))
	    hdb <<- new
	else
	    hdb
    }
})

help.search <-
function(pattern, fields = c("alias", "concept", "title"),
	 apropos, keyword, whatis, ignore.case = TRUE,
	 package = NULL, lib.loc = NULL,
	 help.db = getOption("help.db"),
	 verbose = getOption("verbose"),
	 rebuild = FALSE, agrep = NULL)
{
    ### Argument handling.
    TABLE <- c("alias", "concept", "keyword", "name", "title")

    .wrong_args <- function(args)
	gettextf("argument '%s' must be a single character string", args)

    if(!missing(pattern)) {
	if(!is.character(pattern) || (length(pattern) > 1))
	    stop(.wrong_args("pattern"), domain = NA)
	i <- pmatch(fields, TABLE)
	if(any(is.na(i)))
	    stop("incorrect field specification")
	else
	    fields <- TABLE[i]
    } else if(!missing(apropos)) {
	if(!is.character(apropos) || (length(apropos) > 1))
	    stop(.wrong_args("apropos"), domain = NA)
	else {
	    pattern <- apropos
	    fields <- c("alias", "title")
	}
    } else if(!missing(keyword)) {
	if(!is.character(keyword) || (length(keyword) > 1))
	    stop(.wrong_args("keyword"), domain = NA)
	else {
	    pattern <- keyword
	    fields <- "keyword"
	    if(is.null(agrep)) agrep <- FALSE
	}
    } else if(!missing(whatis)) {
	if(!is.character(whatis) || (length(whatis) > 1))
	    stop(.wrong_args("whatis"), domain = NA)
	else {
	    pattern <- whatis
	    fields <- "alias"
	}
    } else {
	stop("do not know what to search")
    }

    if(is.null(lib.loc))
	lib.loc <- .libPaths()

    if(!missing(help.db))
	warning("argument 'help.db' is deprecated")


    ### Set up the hsearch db.
    db <- eval(.hsearch_db())
    if(is.null(db))
	rebuild <- TRUE
    else if(!rebuild) {
	## Need to find out whether this has the info we need.
	## Note that when looking for packages in libraries we always
	## use the first location found.  Hence if the library search
	## path changes we might find different versions of a package.
	## Thus we need to rebuild the hsearch db in case the specified
	## library path is different from the one used when building the
	## hsearch db (stored as its "LibPaths" attribute).
	if(!identical(lib.loc, attr(db, "LibPaths")) ||
	   ## We also need to rebuild the hsearch db in case an existing
	   ## dir in the library path was modified more recently than
	   ## the db, as packages might have been installed or removed.
	   any(attr(db, "mtime") <
	       file.info(lib.loc[file.exists(lib.loc)])$mtime) ||
	   ## Or if the user changed the locale character type ...
	   !identical(attr(db, "ctype"), Sys.getlocale("LC_CTYPE"))
	   )
	    rebuild <- TRUE
        ## We also need to rebuild if 'packages' was used before and has
        ## changed.
        if (!is.null(package) &&
            any(! package %in% db$Base[, "Package"]))
            rebuild <- TRUE
    }
    if(rebuild) {
	if(verbose) message("Rebuilding the data base ...")
	## Check whether we can save the hsearch db lateron.
	if(all(is.na(mem.limits()))) {
	    save_db <- save_db_to_memory <- TRUE
	}
	else {
	    save_db <- save_db_to_memory <- FALSE
	    dir <- file.path(tempdir(), ".R")
	    db_file <- file.path(dir, "hsearch.rds")
	    if((file_test("-d", dir)
		|| ((unlink(dir) == 0) && dir.create(dir)))
	       && (unlink(db_file) == 0))
		save_db <- TRUE
	}

	packages_in_hsearch_db <- if(!is.null(package))
	    package
	else
	    .packages(all.available = TRUE, lib.loc = lib.loc)

	## Create the hsearch db.
	contents_DCF_fields <-
	    c("Entry", "Aliases", "Description", "Keywords")
	np <- 0
	if(verbose)
	    message("Packages {.readRDS() sequentially}:")

	## Starting with R 1.8.0, prebuilt hsearch indices are available
	## in Meta/hsearch.rds, and the code to build this from the Rd
	## contents (as obtained from both new and old style Rd indices)
	## has been moved to tools:::.build_hsearch_index() which
	## creates a per-package list of base, aliases and keywords
	## information.	 When building the global index, it seems (see
	## e.g. also the code in tools:::Rdcontents()), most efficient to
	## create a list *matrix* (dbMat below), stuff the individual
	## indices into its rows, and finally create the base, alias,
	## keyword, and concept information in rbind() calls on the
	## columns.  This is *much* more efficient than building
	## incrementally.
	dbMat <- vector("list", length(packages_in_hsearch_db) * 4)
	dim(dbMat) <- c(length(packages_in_hsearch_db), 4)
	defunct_standard_package_names <-
	    tools:::.get_standard_package_names()$stubs

	for(p in packages_in_hsearch_db) {
	    np <- np + 1
	    if(verbose)
		message(" ", p, appendLF = ((np %% 5) == 0), domain=NA)
	    path <- .find.package(p, lib.loc, quiet = TRUE)
	    if(length(path) == 0)
		stop(gettextf("could not find package '%s'", p), domain = NA)

	    ## Hsearch 'Meta/hsearch.rds' indices were introduced in
	    ## R 1.8.0.	 If they are missing, we really cannot use
	    ## the package (as library() will refuse to load it).
	    if(file.exists(hs_file <- file.path(path, "Meta", "hsearch.rds"))) {
		hDB <- .readRDS(hs_file)
		if(!is.null(hDB)) {
		    ## Fill up possibly missing information.
		    if(is.na(match("Encoding", colnames(hDB[[1]]))))
			hDB[[1]] <- cbind(hDB[[1]], Encoding = "")
		    hDB[[1]][, "LibPath"] <- path
		    ## Put the hsearch index for the np-th package into the
		    ## np-th row of the matrix used for aggregating.
		    dbMat[np, seq_along(hDB)] <- hDB
		} else if(verbose)
		    message(gettextf("package '%s' has empty hsearch data - strangely", p), domain=NA)
	    }
	    else warning("no hsearch.rds meta data for package ", p)
	}

	if(verbose)  {
	    message(ifelse(np %% 5 == 0, "\n", "\n\n"),
                    sprintf("Built dbMat[%d,%d]", nrow(dbMat), ncol(dbMat)),
                    domain = NA)
            ## DEBUG save(dbMat, file="~/R/hsearch_dbMat.rda", compress=TRUE)
        }

	## workaround methods:::rbind() misbehavior:
	if(.isMethodsDispatchOn()) {
	    bind_was_on <- methods:::bind_activation(FALSE)
	    if(bind_was_on) on.exit(methods:::bind_activation(TRUE))
	}

	## Create the global base, aliases, keywords and concepts tables
	## via calls to rbind() on the columns of the matrix used for
	## aggregating.
	db <- list(Base     = do.call("rbind", dbMat[, 1]),
		   Aliases  = do.call("rbind", dbMat[, 2]),
		   Keywords = do.call("rbind", dbMat[, 3]),
		   Concepts = do.call("rbind", dbMat[, 4]))
	if(is.null(db$Concepts))
	    db$Concepts <-
		matrix(character(), nc = 3,
		       dimnames = list(NULL,
		       c("Concepts", "ID", "Package")))
	## Make the IDs globally unique by prefixing them with the
	## number of the package in the global index.
	for(i in which(sapply(db, NROW) > 0)) {
	    db[[i]][, "ID"] <-
		paste(rep.int(seq_along(packages_in_hsearch_db),
			      sapply(dbMat[, i], NROW)),
		      db[[i]][, "ID"],
		      sep = "/")
	}
	## And maybe re-encode ...
	if(!identical(Sys.getlocale("LC_CTYPE"), "C") && capabilities("iconv")) {
	    if(verbose) message("reencoding ...", appendLF=FALSE)
	    encoding <- db$Base[, "Encoding"]
	    IDs_to_iconv <- db$Base[encoding != "", "ID"]
	    encoding <- encoding[encoding != ""]
	    ## As iconv is not vectorized in the 'from' argument, loop
	    ## over groups of identical encodings.
	    for(enc in unique(encoding)) {
		IDs <- IDs_to_iconv[encoding == enc]
		for(i in seq_along(db)) {
		    ind <- db[[i]][, "ID"] %in% IDs
		    db[[i]][ind, ] <- iconv(db[[i]][ind, ], enc, "")
		}
	    }
	    if(verbose) message(" done")
	}
	## Let us be defensive about invalid multi-byte character data
	## here.  We simple remove all Rd objects with at least one
	## invalid entry, and warn in case we found one.
	bad_IDs <-
	    unlist(sapply(db,
			  function(u)
			  u[rowSums(is.na(nchar(u, "c"))) > 0, "ID"]))
	if(length(bad_IDs)) {
	    warning("removing all entries with invalid multi-byte character data")
	    for(i in seq_along(db)) {
		ind <- db[[i]][, "ID"] %in% bad_IDs
		db[[i]] <- db[[i]][!ind, ]
	    }
	}

	if(save_db) {
	    if(verbose) message("saving the database ...", appendLF=FALSE)
	    attr(db, "LibPaths") <- lib.loc
	    attr(db, "mtime") <- Sys.time()
	    attr(db, "ctype") <- Sys.getlocale("LC_CTYPE")
	    if(save_db_to_memory)
		.hsearch_db(db)
	    else {
		## If we cannot save to memory, serialize to a file ...
		.saveRDS(db, file = db_file, compress = TRUE)
		## and store a promise to unserialize from this file.
		.hsearch_db(substitute(.readRDS(con),
				       list(con = db_file)))
	    }
	    if(verbose) message(" done")
	}
    }

    ### Matching.
    if(verbose)
	message("Database of ",
                NROW(db$Base), " Rd objects (",
                NROW(db$Aliases), " aliases, ",
                NROW(db$Concepts), " concepts, ",
                NROW(db$Keywords), " keywords)",
                domain = NA)
    if(!is.null(package)) {
	## Argument 'package' was given.  Need to check that all given
	## packages exist in the db, and only search the given ones.
	pos_in_hsearch_db <-
	    match(package, unique(db$Base[, "Package"]), nomatch = 0)
        ## This should not happen for R >= 2.4.0
	if(any(pos_in_hsearch_db) == 0)
	    stop(gettextf("no information in the data base for package '%s': need 'rebuild = TRUE'?",
			  package[pos_in_hsearch_db == 0][1]), domain = NA)
	db <-
	    lapply(db,
		   function(x) {
		       x[x[, "Package"] %in% package, , drop = FALSE]
		   })
    }

    ## <FIXME>
    ## No need continuing if there are no objects in the data base.
    ## But shouldn't we return something of class "hsearch"?
    if(!length(db$Base)) return(invisible())
    ## </FIXME>

    ## If agrep is NULL (default), we want to use fuzzy matching iff
    ## 'pattern' contains no characters special to regular expressions.
    ## We use the following crude approximation: if pattern contains
    ## only alphanumeric characters or whitespace or a '-', it is taken
    ## 'as is', and fuzzy matching is used unless turned off explicitly,
    ## or pattern has very few (currently, less than 5) characters.
    if(is.null(agrep) || is.na(agrep))
	agrep <-
	    ((regexpr("^([[:alnum:]]|[[:space:]]|-)+$", pattern) > 0)
	     && (nchar(pattern, type="c") > 4))
    if(is.logical(agrep)) {
	if(agrep)
	    max.distance <- 0.1
    }
    else if(is.numeric(agrep) || is.list(agrep)) {
	max.distance <- agrep
	agrep <- TRUE
    }
    else
	stop("incorrect 'agrep' specification")

    searchFun <- function(x) {
	if(agrep)
	    agrep(pattern, x, ignore.case = ignore.case,
		  max.distance = max.distance)
	else
	    grep(pattern, x, ignore.case = ignore.case)
    }
    dbBase <- db$Base
    searchDbField <- function(field) {
	switch(field,
	       alias = {
		   aliases <- db$Aliases
		   match(aliases[searchFun(aliases[, "Aliases"]),
				 "ID"],
			 dbBase[, "ID"])
	       },
	       concept = {
		   concepts <- db$Concepts
		   match(concepts[searchFun(concepts[, "Concepts"]),
				  "ID"],
			 dbBase[, "ID"])
	       },

	       keyword = {
		   keywords <- db$Keywords
		   match(keywords[searchFun(keywords[, "Keywords"]),
				  "ID"],
			 dbBase[, "ID"])
	       },
	       searchFun(db$Base[, field]))
    }

    i <- NULL
    for(f in fields) i <- c(i, searchDbField(f))
    db <- dbBase[sort(unique(i)),
		 c("topic", "title", "Package", "LibPath"),
		 drop = FALSE]
    if(verbose) message(gettextf("matched %d objects.", NROW(db)), domain=NA)

    ## Retval.
    y <- list(pattern = pattern, fields = fields,
	      type = if(agrep) "fuzzy" else "regexp",
	      matches = db)
    class(y) <- "hsearch"
    y
}

print.hsearch <- function(x, ...)
    printhsearchInternal(x, ...)


printhsearchInternal  <- function(x, ...)
{
    fields <- paste(x$fields, collapse = " or ")
    type <- switch(x$type, fuzzy = "fuzzy", "regular expression")
    db <- x$matches
    if(NROW(db) > 0) {
	outFile <- tempfile()
	outConn <- file(outFile, open = "w")
	writeLines(c(strwrap(paste("Help files with", fields,
				   "matching", sQuote(x$pattern),
				   "using", type, "matching:")),
		     "\n\n"),
		   outConn)
	dbnam <- paste(db[ , "topic"], "(",
		       db[, "Package"], ")",
		       sep = "")
	dbtit <- paste(db[ , "title"], sep = "")
	writeLines(formatDL(dbnam, dbtit), outConn)
	writeLines(c("\n\n",
		     strwrap(paste("Type 'help(FOO, package = PKG)' to",
				   "inspect entry 'FOO(PKG) TITLE'."))),
		   outConn)
	close(outConn)
	file.show(outFile, delete.file = TRUE)
    } else {
	writeLines(strwrap(paste("No help files found with", fields,
				 "matching", sQuote(x$pattern),
				 "using", type, "matching.")))
    }

    invisible(x)
}
