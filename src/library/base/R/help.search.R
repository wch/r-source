help.search <-
function(pattern, fields = c("alias", "title"),
	 apropos, keyword, whatis, ignore.case = TRUE,
	 package = NULL, lib.loc = NULL,
	 help.db = getOption("help.db"),
	 verbose = getOption("verbose"),
	 rebuild = FALSE, agrep = NULL)
{
    sQuote <- function(s) paste("'", s, "'", sep = "")

    ### Argument handling.
    TABLE <- c("name", "alias", "title", "keyword")
    if(!missing(pattern)) {
	if(!is.character(pattern) || (length(pattern) > 1))
	    stop(paste(sQuote("pattern"),
		       "must be a single character string"))
	i <- pmatch(fields, TABLE)
	if(any(is.na(i)))
	    stop("incorrect field specification")
	else
	    fields <- TABLE[i]
    } else if(!missing(apropos)) {
	if(!is.character(apropos) || (length(apropos) > 1))
	    stop(paste(sQuote("apropos"),
		       "must be a single character string"))
	else {
	    pattern <- apropos
	    fields <- c("alias", "title")
	}
    } else if(!missing(keyword)) {
	if(!is.character(keyword) || (length(keyword) > 1))
	    stop(paste(sQuote("keyword"),
		       "must be a single character string"))
	else {
	    pattern <- keyword
	    fields <- "keyword"
	}
    } else if(!missing(whatis)) {
	if(!is.character(whatis) || (length(whatis) > 1))
	    stop(paste(sQuote("whatis"),
		       "must be a single character string"))
	else {
	    pattern <- whatis
	    fields <- "alias"
	}
    } else {
	stop("don't know what to search")
    }

    if(is.null(lib.loc))
	lib.loc <- .libPaths()

    ## <FIXME>
    ## Currently, the information used for help.search is stored in
    ## package-level CONTENTS files.  As it is expensive to build the
    ## help.search db, we use a global file cache for this information
    ## if possible.  This is wrong because multiple processes or threads
    ## use the same cache (no locking!), and we should really save the
    ## information on a package or library level, preferably already at
    ## package install time.  Argh ...
    ## </FIXME>

    ### Set up the help db.
    if(is.null(help.db) || !file.exists(help.db))
	rebuild <- TRUE
    if(!rebuild) {
	## Try using the saved help db.
	## <FIXME>
	## Shouldn't we unserialize instead?
	load(file = help.db)
	## </FIXME>
	## If not a list (pre 1.7 format), rebuild.
	if(!is.list(db)) rebuild <- TRUE
	## Need to find out whether this has the info we need.
	## Note that when looking for packages in libraries we always
	## use the first location found.  Hence if the library search
	## path changes we might find different versions of a package.
	## Thus we need to rebuild the help db in case the specified
	## library path is different from the one used when building the
	## help db (stored as its "LibPaths" attribute).
	if(!identical(lib.loc, attr(db, "LibPaths")))
	    rebuild <- TRUE
	## We also need to rebuild the help db in case an existing dir
	## in the library path was modified more recently than the db,
	## as packages might have been installed or removed.
	if(any(file.info(help.db)$mtime <
	       file.info(lib.loc[file.exists(lib.loc)])$mtime))
	    rebuild <- TRUE
    }
    if(rebuild) {
	## Check whether we can save the help db lateron.
	save.db <- FALSE
	dir <- switch(.Platform$OS.type,
		      "windows" = Sys.getenv("R_USER"),
		      "unix" = Sys.getenv("HOME"),
		      "mac" = R.home(),
		      "")
	if(nchar(dir) == 0) dir <- getwd()
	dir <- file.path(dir, ".R")
	dbfile <- file.path(dir, "help.db")
	if(((file.exists(dir) && file.info(dir)$isdir)
	    || ((unlink(dir) == 0) && dir.create(dir)))
	   && (unlink(dbfile) == 0))
	    save.db <- TRUE
        ## If we cannot save the help db only use the given packages.
        ## <FIXME>
        ## Why don't we just use the given packages?  The current logic
        ## for rebuilding cannot figure out that rebuilding is needed
        ## the next time (unless we use the same given packages) ...
        packagesInHelpDB <- if(!is.null(package) && !save.db)
            package
        else
            .packages(all.available = TRUE, lib.loc = lib.loc)
        ## </FIXME>
	## Create the help db.
	contentsEnv <- new.env()
	contentsDCFFields <-
	    c("Entry", "Aliases", "Description", "Keywords")
	contentsRDSFields <-
	    c("Name", "Aliases", "Title", "Keywords")
	dbBase <- dbAliases <- dbKeywords <- NULL
	nEntries <- 0
	if(verbose) {
	    cat("Packages:\n")
	    np <- 0
	}

	for(p in packagesInHelpDB) {
	    if(verbose)
		cat("", p, if((np <- np + 1)%% 5 == 0) "\n")
	    contents <- NULL
	    path <- .find.package(p, lib.loc, quiet = TRUE)
	    if(length(path) == 0)
		stop(paste("could not find package", sQuote(p)))
	    lib <- dirname(path)

	    ## Read the contents info from the respective Rd meta
	    ## files.
	    if(file.exists(contentsFile <-
                           file.path(path, "Meta", "Rd.rds"))) {
		contents <-
		    .readRDS(contentsFile)[ , contentsRDSFields,
					   drop = FALSE]
            }
	    ## <FIXME>
	    ## Remove this once 1.7.0 is out.
	    ## (The 1.7 development versions for some time used files
	    ## 'CONTENTS.rds', and 'CONTENTS.rda' generated by save().)
            else if(file.exists(contentsFile <-
                                file.path(path, "CONTENTS.rds"))) {
		contents <-
		    .readRDS(contentsFile)[ , contentsRDSFields,
					   drop = FALSE]
	    }
	    else if(file.exists(contentsFile <-
				file.path(path, "CONTENTS.rda"))) {
		load(contentsFile, envir = contentsEnv)
		contents <-
		    get("contents", envir = contentsEnv)[ ,
				    contentsRDSFields,
				    drop = FALSE]
	    }
	    ## </FIXME>
	    else if(file.exists(contentsFile <-
				file.path(path, "CONTENTS")))
		contents <-
		    read.dcf(contentsFile, fields = contentsDCFFields)

	    if(!is.null(contents)) {
		## If we found something ...
		if((nr <- NROW(contents)) > 0) {
		    if(!is.data.frame(contents)) {
			colnames(contents) <- contentsRDSFields
			base <- contents[, c("Name", "Title"), drop = FALSE]
			## If the contents db is not a data frame, then
			## it has the aliases collapsed.  Split again as
			## we need the first alias as the help topic to
			## indicate for matching Rd objects.
			aliases <-
			    strsplit(contents[, "Aliases"], " +")
			## Don't do it for keywords, though, as these
			## might be non-standard (and hence contain
			## white space ...).
		    }
		    else {
			base <-
			    as.matrix(contents[, c("Name", "Title")])
			aliases <- contents[, "Aliases"]
		    }

		    ## IDs holds the numbers of the Rd objects in the
		    ## help.search db.
		    IDs <- seq(from = nEntries + 1, to = nEntries + nr)
		    ## We create 3 character matrices (cannot use data
		    ## frames for efficiency reasons): 'dbBase' holds
		    ## all character string data, and 'dbAliases' and
		    ## 'dbKeywords' hold character vector data in a
		    ## 3-column character matrix format with entry, ID
		    ## of the Rd object the entry comes from, and the
		    ## package the object comes from.  The latter is
		    ## useful when subscripting according to package.
		    dbBase <-
			rbind(dbBase,
			      cbind(p, lib, IDs, base,
				    topic = sapply(aliases, "[", 1)))
		    ## If there are no aliases at all, cbind() below
		    ## would give matrix(p, nc = 1).  (Of course, Rd
		    ## objects without aliases are useless ...)
		    if(length(a <- unlist(aliases)) > 0)
			dbAliases <-
			    rbind(dbAliases,
				  cbind(a,
					rep(IDs,
					    sapply(aliases, length)),
					p))
		    keywords <- contents[, "Keywords"]
                    ## And similarly if there are no keywords at all.
                    if(length(k <- unlist(keywords)) > 0)
                        dbKeywords <-
                            rbind(dbKeywords,
                                  cbind(k,
                                        rep(IDs,
                                            sapply(keywords, length)),
                                        p))
		    nEntries <- nEntries + nr
		} else {
		    warning(paste("Empty contents for package",
				  sQuote(p), "in", sQuote(lib)))
		}
	    }
	}
	if(verbose)
	    cat(ifelse(np %% 5 == 0, "\n", "\n\n"))
	colnames(dbBase) <-
	    c("Package", "LibPath", "ID", "name", "title", "topic")
	colnames(dbAliases) <-
	    c("Aliases", "ID", "Package")
	colnames(dbKeywords) <-
	    c("Keywords", "ID", "Package")
	db <- list(Base = dbBase,
		   Aliases = dbAliases,
		   Keywords = dbKeywords)
	## Maybe save the help db
	## <FIXME>
	## Shouldn't we serialize instead?
	if(save.db) {
	    attr(db, "LibPaths") <- lib.loc
	    save(db, file = dbfile)
	    options(help.db = dbfile)
	}
	## </FIXME>
    }

    ### Matching.
    if(verbose)
	cat("Database of ",
	    NROW(db$Base), " Rd objects (",
	    NROW(db$Aliases), " aliases, ",
	    NROW(db$Keywords), " keywords),\n",
	    sep = "")
    if(!is.null(package)) {
	## Argument 'package' was given but we built a larger help db to
	## save for future invocations.	 Need to check that all given
	## packages exist, and only search the given ones.
	posInHelpDB <-
	    match(package, unique(db$Base[, "Package"]), nomatch = 0)
	if(any(posInHelpDB) == 0)
	    stop(paste("could not find package",
		       sQuote(package[posInHelpDB == 0][1])))
	db <-
	    lapply(db,
		   function(x) {
		       x[x[, "Package"] %in% package, , drop = FALSE]
		   })
    }

    ## If agrep is NULL (default), we want to use fuzzy matching iff
    ## 'pattern' contains no characters special to regular expressions.
    ## We use the following crude approximation: if pattern contains
    ## only alphanumeric characters or whitespace or a '-', it is taken
    ## 'as is', and fuzzy matching is used unless turned off explicitly.
    if(is.null(agrep) || is.na(agrep))
	agrep <-
	    (regexpr("^([[:alnum:]]|[[:space:]]|-)+$", pattern) > 0)
    if(is.logical(agrep)) {
	if(agrep)
	    max.distance <- 0.15
    }
    else if(is.numeric(agrep) || is.list(agrep)) {
	max.distance <- agrep
	agrep <- TRUE
    }
    else
	stop("incorrect agrep specification")

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
    if(verbose) cat("matched", NROW(db), "objects.\n")

    ## Retval.
    y <- list(pattern = pattern, fields = fields,
              type = if(agrep) "fuzzy" else "regexp",
              matches = db)
    class(y) <- "hsearch"
    y
}

print.hsearch <-
function(x, ...)
{
    sQuote <- function(s) paste("'", s, "'", sep = "")
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
}
