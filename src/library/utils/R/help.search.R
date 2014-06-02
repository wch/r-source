#  File src/library/utils/R/help.search.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

.hsearch_db <- local({
    hdb <- NULL
    function(new) {
	if(!missing(new))
	    hdb <<- new
	else
	    hdb
    }
})

merge.vignette.index <- function(hDB, path, pkg) {
    ## Vignettes in the hsearch index started in R2.14.0
    ## Most packages don't have them, so the following should not be
    ## too inefficient
    if(file.exists(v_file <- file.path(path, "Meta", "vignette.rds"))
       && !is.null(vDB <- readRDS(v_file))
       && nrow(vDB)) {
	## Make it look like an hDB base matrix and append it
	base <- matrix("", nrow=nrow(vDB), ncol=8)
	colnames(base) <- colnames(hDB[[1]])
	base[,"Package"] <- pkg
	base[,"LibPath"] <- path
	id <- as.character(1:nrow(vDB) + NROW(hDB[[1]]))
	base[,"ID"] <- id
	base[,"name"] <- sub("\\.[^.]*$", "", basename(vDB$File))
	base[,"topic"] <- base[,"name"]
	base[,"title"] <- vDB$Title
	base[,"Type"] <- "vignette"
	hDB[[1L]] <- rbind(hDB[[1L]], base)
	aliases <- matrix("", nrow=nrow(vDB), ncol=3)
	colnames(aliases) <- colnames(hDB[[2]])
	aliases[,"Aliases"] <- base[,"name"]
	aliases[,"ID"] <- id
	aliases[,"Package"] <- pkg
	hDB[[2L]] <- rbind(hDB[[2L]], aliases)
	nkeywords <- sum(sapply(vDB$Keywords, length))
	if (nkeywords) {
	    keywords <- matrix("", nrow=nkeywords, ncol=3)
	    colnames(keywords) <- colnames(hDB[[4]])
	    keywords[,"Concepts"] <- unlist(vDB$Keywords)
	    keywords[,"ID"] <- unlist(lapply(1:nrow(vDB),
		   function(i) rep(id[i], length(vDB$Keywords[[i]]))))
	    keywords[,"Package"] <- pkg
	    hDB[[4L]] <- rbind(hDB[[4L]], keywords)
	}
    }
    hDB
}

merge.demo.index <- function(hDB, path, pkg) {
    ## Demos in the hsearch index started in R2.14.0
    if(file.exists(d_file <- file.path(path, "Meta", "demo.rds"))
       && !is.null(dDB <- readRDS(d_file))
       && nrow(dDB)) {
	## Make it look like an hDB base matrix and append it
	base <- matrix("", nrow=nrow(dDB), ncol=8)
	colnames(base) <- colnames(hDB[[1]])
	base[,"Package"] <- pkg
	base[,"LibPath"] <- path
	id <- as.character(1:nrow(dDB) + NROW(hDB[[1]]))
	base[,"ID"] <- id
	base[,"name"] <- dDB[,1]
	base[,"topic"] <- base[,"name"]
	base[,"title"] <- dDB[,2]
	base[,"Type"] <- "demo"
	hDB[[1L]] <- rbind(hDB[[1L]], base)
	aliases <- matrix("", nrow=nrow(dDB), ncol=3)
	colnames(aliases) <- colnames(hDB[[2]])
	aliases[,"Aliases"] <- base[,"name"]
	aliases[,"ID"] <- id
	aliases[,"Package"] <- pkg
	hDB[[2L]] <- rbind(hDB[[2L]], aliases)
    }
    hDB
}

## FIXME: use UTF-8, either always or optionally
## (Needs UTF-8-savvy & fast agrep, and PCRE regexps.)
help.search <-
    function(pattern, fields = c("alias", "concept", "title"),
             apropos, keyword, whatis, ignore.case = TRUE,
             package = NULL, lib.loc = NULL,
             help.db = getOption("help.db"),
             verbose = getOption("verbose"),
             rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE,
             types = getOption("help.search.types")
)
{
    WINDOWS <- .Platform$OS.type == "windows"

    ### Argument handling.
    FIELDS <- c("alias", "concept", "keyword", "name", "title")
    TYPES <- c("help", "vignette", "demo")

    if (is.logical(verbose)) verbose <- 2*as.integer(verbose)
    .wrong_args <- function(args)
	gettextf("argument %s must be a single character string", sQuote(args))

    fuzzy <- agrep
    if(!missing(pattern)) {
	if(!is.character(pattern) || (length(pattern) > 1L))
	    stop(.wrong_args("pattern"), domain = NA)
	i <- pmatch(fields, FIELDS)
	if(anyNA(i))
	    stop("incorrect field specification")
	else
	    fields <- FIELDS[i]
    } else if(!missing(apropos)) {
	if(!is.character(apropos) || (length(apropos) > 1L))
	    stop(.wrong_args("apropos"), domain = NA)
	else {
	    pattern <- apropos
	    fields <- c("alias", "title")
	}
    } else if(!missing(keyword)) {
	if(!is.character(keyword) || (length(keyword) > 1L))
	    stop(.wrong_args("keyword"), domain = NA)
	else {
	    pattern <- keyword
	    fields <- "keyword"
	    if(is.null(fuzzy)) fuzzy <- FALSE
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
    i <- pmatch(types, TYPES)
    if (anyNA(i))
	stop("incorrect type specification")
    else
	types <- TYPES[i]

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
	   !all(types %in% attr(db, "Types")) ||
	   ## We also need to rebuild the hsearch db in case an existing
	   ## dir in the library path was modified more recently than
	   ## the db, as packages might have been installed or removed.
	   any(attr(db, "mtime") < file.mtime(lib.loc[file.exists(lib.loc)])) ||
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
	if(verbose > 0L) {
            message("Rebuilding the help.search() database", " ", "...",
                    if(verbose > 1L) "...", domain = NA)
            flush.console()
        }

	if(!is.null(package)) {
	    packages_in_hsearch_db <- package
            package_paths <- NULL
	} else {
            ## local version of .packages(all.available = TRUE),
            ## recording paths
            ans <- character(0L); paths <- character(0L)
            lib.loc <- lib.loc[file.exists(lib.loc)]
            valid_package_version_regexp <-
                .standard_regexps()$valid_package_version
            for (lib in lib.loc) {
                a <- list.files(lib, all.files = FALSE, full.names = FALSE)
                for (nam in a) {
                    pfile <- file.path(lib, nam, "Meta", "package.rds")
                    if (file.exists(pfile))
                        info <- readRDS(pfile)$DESCRIPTION[c("Package", "Version")]
                    else next
                    if ( (length(info) != 2L) || anyNA(info) ) next
                    if (!grepl(valid_package_version_regexp, info["Version"])) next
                    ans <- c(ans, nam)
                    paths <- c(paths, file.path(lib, nam))
                }
            }
            un <- !duplicated(ans)
	    packages_in_hsearch_db <-  ans[un]
            package_paths <- paths[un]
            names(package_paths) <- ans[un]
        }

	## Create the hsearch db.
	np <- 0L
	if(verbose >= 2L) {
	    message("Packages {readRDS() sequentially}:", domain = NA)
            flush.console()
        }
        tot <- length(package_paths)
        incr <- 0L
        if(verbose && WINDOWS) {
            pb <- winProgressBar("R: creating the help.search() DB", max = tot)
            on.exit(close(pb))
        } else if(verbose == 1L) incr <- ifelse(tot > 500L, 100L, 10L)

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
	dbMat <- vector("list", length(packages_in_hsearch_db) * 4L)
	dim(dbMat) <- c(length(packages_in_hsearch_db), 4L)
	defunct_standard_package_names <-
	    tools:::.get_standard_package_names()$stubs

	for(p in packages_in_hsearch_db) {
            if(incr && np %% incr == 0L) {
                message(".", appendLF = FALSE, domain = NA)
                flush.console()
            }
	    np <- np + 1L
            if(verbose && WINDOWS) setWinProgressBar(pb, np)
	    if(verbose >= 2L) {
		message(" ", p, appendLF = ((np %% 5L) == 0L), domain=NA)
                flush.console()
            }
            path <- if(!is.null(package_paths)) package_paths[p]
	    else find.package(p, lib.loc, quiet = TRUE)
	    if(length(path) == 0L) {
                if(is.null(package)) next
		else stop(gettextf("could not find package %s", sQuote(p)),
                          domain = NA)
            }
	    ## Hsearch 'Meta/hsearch.rds' indices were introduced in
	    ## R 1.8.0.	 If they are missing, we really cannot use
	    ## the package (as library() will refuse to load it).
	    ## We always load hsearch.rds to establish the format,
	    ## sometimes vignette.rds.

	    if(file.exists(hs_file <- file.path(path, "Meta", "hsearch.rds"))) {
		hDB <- readRDS(hs_file)
		if(!is.null(hDB)) {
		    ## Fill up possibly missing information.
		    if(is.na(match("Encoding", colnames(hDB[[1L]]))))
			hDB[[1L]] <- cbind(hDB[[1L]], Encoding = "")
		    nh <- NROW(hDB[[1L]])
		    hDB[[1L]] <- cbind(hDB[[1L]],
		                       Type = rep("help", nh))
		    if (nh)
		    	hDB[[1L]][, "LibPath"] <- path
		    if ("vignette" %in% types)
		    	hDB <- merge.vignette.index(hDB, path, p)
		    if ("demo" %in% types)
		    	hDB <- merge.demo.index(hDB, path, p)
		    ## Put the hsearch index for the np-th package into the
		    ## np-th row of the matrix used for aggregating.
		    dbMat[np, seq_along(hDB)] <- hDB
		} else if(verbose >= 2L) {
		    message(gettextf("package %s has empty hsearch data - strangely",
                                     sQuote(p)), domain = NA)
                    flush.console()
                }
	    }
	    else if(!is.null(package))
                warning("no hsearch.rds meta data for package ", p, domain = NA)
	}

	if(verbose >= 2L)  {
	    message(ifelse(np %% 5L == 0L, "\n", "\n\n"),
                    sprintf("Built dbMat[%d,%d]", nrow(dbMat), ncol(dbMat)),
                    domain = NA)
            flush.console()
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
		matrix(character(), ncol = 3L,
		       dimnames = list(NULL,
		       c("Concepts", "ID", "Package")))
	## Make the IDs globally unique by prefixing them with the
	## number of the package in the global index.
	for(i in which(sapply(db, NROW) > 0L)) {
	    db[[i]][, "ID"] <-
		paste(rep.int(seq_along(packages_in_hsearch_db),
			      sapply(dbMat[, i], NROW)),
		      db[[i]][, "ID"],
		      sep = "/")
	}
	## And maybe re-encode ...
	if(!identical(Sys.getlocale("LC_CTYPE"), "C")) {
	    if(verbose >= 2L) {
                message("reencoding ...", appendLF=FALSE, domain = NA)
                flush.console()
            }
	    encoding <- db$Base[, "Encoding"]
            target <- ifelse(use_UTF8 && !l10n_info()$`UTF-8`, "UTF-8", "")
	    ## As iconv is not vectorized in the 'from' argument, loop
	    ## over groups of identical encodings.
	    for(enc in unique(encoding)) {
                if(enc != target) next
		IDs <- db$Base[encoding == enc, "ID"]
		for(i in seq_along(db)) {
		    ind <- db[[i]][, "ID"] %in% IDs
		    db[[i]][ind, ] <- iconv(db[[i]][ind, ], enc, "")
		}
	    }
	    if(verbose >= 2L) {
                message(" ", "done", domain = NA)
                flush.console()
            }
	}
	bad_IDs <-
	    unlist(sapply(db,
			  function(u)
			  u[rowSums(is.na(nchar(u, "c", TRUE))) > 0, "ID"]))
        ## FIXME: drop this fallback
	if(length(bad_IDs)) { ## try latin1
            for(i in seq_along(db)) {
                ind <- db[[i]][, "ID"] %in% bad_IDs
                db[[i]][ind, ] <- iconv(db[[i]][ind, ], "latin1", "")
            }
            bad_IDs <-
                unlist(sapply(db,
                              function(u)
                              u[rowSums(is.na(nchar(u, "c", TRUE))) > 0, "ID"]))
        }
	## If there are any invalid multi-byte character data
	## left, we simple remove all Rd objects with at least one
	## invalid entry, and warn.
        if(length(bad_IDs)) {
	    warning("removing all entries with invalid multi-byte character data")
	    for(i in seq_along(db)) {
		ind <- db[[i]][, "ID"] %in% bad_IDs
		db[[i]] <- db[[i]][!ind, ]
	    }
	}

        if(verbose >= 2L) {
            message("saving the database ...", appendLF=FALSE, domain = NA)
            flush.console()
        }
        attr(db, "LibPaths") <- lib.loc
        attr(db, "mtime") <- Sys.time()
        attr(db, "ctype") <- Sys.getlocale("LC_CTYPE")
        attr(db, "Types") <- unique(c("help", types))
        .hsearch_db(db)
        if(verbose >= 2L) {
            message(" ", "done", domain = NA)
            flush.console()
        }
        if(verbose > 0L) {
            message("... database rebuilt", domain = NA)
            if(WINDOWS) {
                close(pb)
                on.exit() # clear closing of progress bar
            }
            flush.console()
        }
    }

    ### Matching.
    if(verbose >= 2L) {
	message("Database of ",
                NROW(db$Base), " help objects (",
                NROW(db$Aliases), " aliases, ",
                NROW(db$Concepts), " concepts, ",
                NROW(db$Keywords), " keywords)",
                domain = NA)
        flush.console()
    }
    if(!is.null(package)) {
	## Argument 'package' was given.  Need to check that all given
	## packages exist in the db, and only search the given ones.
	pos_in_hsearch_db <-
	    match(package, unique(db$Base[, "Package"]), nomatch = 0L)
        ## This should not happen for R >= 2.4.0
	if(any(pos_in_hsearch_db) == 0L)
	    stop(gettextf("no information in the database for package %s: need 'rebuild = TRUE'?",
			  sQuote(package[pos_in_hsearch_db == 0][1L])),
                 domain = NA)
	db <-
	    lapply(db,
		   function(x) {
		       x[x[, "Package"] %in% package, , drop = FALSE]
		   })
    }

    ## Subset to the requested help types
    db$Base <- db$Base[db$Base[,"Type"] %in% types,,drop=FALSE]

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
    if(is.null(fuzzy) || is.na(fuzzy))
	fuzzy <-
	    (grepl("^([[:alnum:]]|[[:space:]]|-)+$", pattern)
	     && (nchar(pattern, type="c") > 4L))
    if(is.logical(fuzzy)) {
	if(fuzzy)
	    max.distance <- 0.1
    }
    else if(is.numeric(fuzzy) || is.list(fuzzy)) {
	max.distance <- fuzzy
	fuzzy <- TRUE
    }
    else
	stop("incorrect 'agrep' specification")

    searchFun <- function(x) {
	if(fuzzy)
	    agrep(pattern, x, ignore.case = ignore.case,
		  max.distance = max.distance)
	else
	    grep(pattern, x, ignore.case = ignore.case, perl = use_UTF8)
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
		 c("topic", "title", "Package", "LibPath", "name", "Type"),
		 drop = FALSE]
    if(verbose>= 2L) {
        message(sprintf(ngettext(NROW(db),
                                 "matched %d object.",
                                 "matched %d objects."),
                        NROW(db)),
                domain = NA)
        flush.console()
    }

    ## Retval.
    y <- list(pattern = pattern, fields = fields,
	      type = if(fuzzy) "fuzzy" else "regexp",
	      agrep = agrep,
	      ignore.case = ignore.case, types = types,
	      package = package, lib.loc = lib.loc,
	      matches = db)
    class(y) <- "hsearch"
    y
}

## this extra indirection allows the Mac GUI to replace this
## yet call the printhsearchInternal function.
print.hsearch <- function(x, ...)
    printhsearchInternal(x, ...)


printhsearchInternal  <- function(x, ...)
{
    help_type <- getOption("help_type", default="text")
    types <- x$types
    if (help_type == "html") {
        browser <- getOption("browser")
	if (tools:::httpdPort == 0L) tools::startDynamicHelp()
	if (tools:::httpdPort > 0L) {
	    url <- paste0("http://127.0.0.1:", tools:::httpdPort,
                      "/doc/html/Search?pattern=", tools:::escapeAmpersand(x$pattern),
                      # Only encode non-default values
                      if (!("title" %in% x$fields)) "&title=0",
                      if ("keyword" %in% x$fields) "&keyword=1",
                      if (!("alias" %in% x$fields)) "&alias=0",
                      if (!("concept" %in% x$fields)) "&concept=0",
                      if ("name" %in% x$fields) "&name=1",
                      if (!is.null(x$agrep)) paste0("&agrep=", x$agrep),
                      if (!x$ignore.case) "&ignore.case=0",
                      if (!identical(types, getOption("help.search.types")))
			 paste0("&types=", paste(types, collapse=";")),
                      if (!is.null(x$package))
			 paste0("&package=", paste(x$package, collapse=";")),
                      if (!identical(x$lib.loc, .libPaths()))
			 paste0("&lib.loc=", paste(x$lib.loc, collapse=";")))
            browseURL(url, browser)
            return(invisible(x))
        }
    }
    hfields <- paste(x$fields, collapse = " or ")
    vfieldnames <- c(alias = "name", concept="keyword", keyword=NA,
                     name="name", title="title")
    vfieldnames <- vfieldnames[x$fields]
    vfields <- paste(unique(vfieldnames[!is.na(vfieldnames)]), collapse = " or ")
    dfieldnames <- c(alias = "name", concept=NA, keyword=NA,
                     name = "name", title = "title")
    dfieldnames <- dfieldnames[x$fields]
    dfields <- paste(unique(dfieldnames[!is.na(dfieldnames)]), collapse = " or ")
    fields <- list(help=hfields, vignette=vfields, demo=dfields)
    matchtype <- switch(x$type, fuzzy = "fuzzy", "regular expression")
    typenames <- c(vignette = "Vignettes", help = "Help files", demo="Demos")
    db <- x$matches
    if(NROW(db) == 0) {
    	typenames <- paste(tolower(typenames[types]), collapse=" or ")
	writeLines(strwrap(paste("No", typenames,  "found with", fields$help,
				 "matching", sQuote(x$pattern),
				 "using", matchtype, "matching.")))
        return(invisible(x))
    }

    outFile <- tempfile()
    outConn <- file(outFile, open = "w")
    typeinstruct <- c(vignette = paste("Type 'vignette(\"FOO\", package=\"PKG\")' to",
				       "inspect entries 'PKG::FOO'."),
                      help = paste("Type '?PKG::FOO' to",
				       "inspect entries 'PKG::FOO',",
				       "or 'TYPE?PKG::FOO' for entries like",
				       "'PKG::FOO-TYPE'."),
		      demo = paste("Type 'demo(PKG::FOO)' to",
				       "run demonstration 'PKG::FOO'."))

    for (type in types) {
	if(NROW(dbtemp <- db[db[,"Type"] == type,,drop=FALSE]) > 0) {
	    writeLines(c(strwrap(paste(typenames[type], "with", fields[[type]],
				       "matching", sQuote(x$pattern),
				       "using", matchtype, "matching:")),
			 "\n"),
		       outConn)
	    dbnam <- paste0(dbtemp[, "Package"], "::", dbtemp[ , "topic"])
	    dbtit <- paste0(dbtemp[ , "title"])
	    writeLines(formatDL(dbnam, dbtit), outConn)
	    writeLines(c("\n",
			 strwrap(typeinstruct[type]),
			 "\n\n"),
		       outConn)
	}
    }
    close(outConn)
    file.show(outFile, delete.file = TRUE)
    invisible(x)
}
