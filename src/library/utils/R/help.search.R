#  File src/library/utils/R/help.search.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

.hsearch_db <-
local({
    hdb <- NULL
    function(new) {
	if(!missing(new))
	    hdb <<- new
	else
	    hdb
    }
})

merge_vignette_index <-
function(hDB, path, pkg)
{
    ## Vignettes in the hsearch index started in R 2.14.0
    ## Most packages don't have them, so the following should not be
    ## too inefficient
    if(file.exists(v_file <- file.path(path, "Meta", "vignette.rds"))
       && !is.null(vDB <- readRDS(v_file))
       && nrow(vDB)) {
	## Make it look like an hDB base matrix and append it
	base <- matrix("", nrow = nrow(vDB), ncol = 8L)
	colnames(base) <- colnames(hDB[[1L]])
	base[, "Package"] <- pkg
	base[, "LibPath"] <- path
	id <- as.character(1:nrow(vDB) + NROW(hDB[[1L]]))
	base[, "ID"] <- id
	base[, "Name"] <- sub("\\.[^.]*$", "", basename(vDB$File))
	base[, "Topic"] <- base[, "Name"]
	base[, "Title"] <- vDB$Title
	base[, "Type"] <- "vignette"
	hDB[[1L]] <- rbind(hDB[[1L]], base)
	aliases <- matrix("", nrow = nrow(vDB), ncol = 3L)
	colnames(aliases) <- colnames(hDB[[2L]])
	aliases[, "Alias"] <- base[, "Name"]
	aliases[, "ID"] <- id
	aliases[, "Package"] <- pkg
	hDB[[2L]] <- rbind(hDB[[2L]], aliases)
	nkeywords <- sum(lengths(vDB$Keywords))
	if (nkeywords) {
	    keywords <- matrix("", nrow = nkeywords, ncol = 3L)
	    colnames(keywords) <- colnames(hDB[[4L]])
	    keywords[,"Concept"] <- unlist(vDB$Keywords)
	    keywords[,"ID"] <- unlist(lapply(1:nrow(vDB),
		   function(i) rep(id[i], length(vDB$Keywords[[i]]))))
	    keywords[,"Package"] <- pkg
	    hDB[[4L]] <- rbind(hDB[[4L]], keywords)
	}
    }
    hDB
}

merge_demo_index <-
function(hDB, path, pkg)
{
    ## Demos in the hsearch index started in R 2.14.0
    if(file.exists(d_file <- file.path(path, "Meta", "demo.rds"))
       && !is.null(dDB <- readRDS(d_file))
       && nrow(dDB)) {
	## Make it look like an hDB base matrix and append it
	base <- matrix("", nrow = nrow(dDB), ncol = 8L)
	colnames(base) <- colnames(hDB[[1]])
	base[, "Package"] <- pkg
	base[, "LibPath"] <- path
	id <- as.character(1:nrow(dDB) + NROW(hDB[[1L]]))
	base[, "ID"] <- id
	base[, "Name"] <- dDB[, 1L]
	base[, "Topic"] <- base[, "Name"]
	base[, "Title"] <- dDB[, 2L]
	base[, "Type"] <- "demo"
	hDB[[1L]] <- rbind(hDB[[1L]], base)
	aliases <- matrix("", nrow = nrow(dDB), ncol = 3L)
	colnames(aliases) <- colnames(hDB[[2L]])
	aliases[, "Alias"] <- base[, "Name"]
	aliases[, "ID"] <- id
	aliases[, "Package"] <- pkg
	hDB[[2L]] <- rbind(hDB[[2L]], aliases)
    }
    hDB
}

hsearch_db_fields <-
    c("alias", "concept", "keyword", "name", "title")
hsearch_db_types <-
    c("help", "vignette", "demo")

## FIXME: use UTF-8, either always or optionally
## (Needs UTF-8-savvy & fast agrep, and PCRE regexps.)
help.search <-
function(pattern, fields = c("alias", "concept", "title"),
         apropos, keyword, whatis, ignore.case = TRUE,
         package = NULL, lib.loc = NULL,
         help.db = getOption("help.db"),
         verbose = getOption("verbose"),
         rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE,
         types = getOption("help.search.types"))
{
    ### Argument handling.
    .wrong_args <- function(args)
	gettextf("argument %s must be a single character string", sQuote(args))
    if(is.logical(verbose)) verbose <- 2 * as.integer(verbose)
    fuzzy <- agrep
    if(!missing(pattern)) {
	if(!is.character(pattern) || (length(pattern) > 1L))
	    stop(.wrong_args("pattern"), domain = NA)
	i <- pmatch(fields, hsearch_db_fields)
	if(anyNA(i))
	    stop("incorrect field specification")
	else
	    fields <- hsearch_db_fields[i]
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

    if(!missing(help.db))
	warning("argument 'help.db' is deprecated")

    ## This duplicates expansion in hsearch_db(), but there is no simple
    ## way to avoid this.
    i <- pmatch(types, hsearch_db_types)
    if (anyNA(i))
	stop("incorrect type specification")
    else
	types <- hsearch_db_types[i]
    
    ### Set up the hsearch db.
    db <- hsearch_db(package, lib.loc, types, verbose, rebuild,
                     use_UTF8)
    ## Argument lib.loc was expanded when building the hsearch db, so
    ## get from there.
    lib.loc <- attr(db, "LibPaths")

    ## Subset to the requested help types if necessary.
    if(!identical(sort(types), sort(attr(db, "Types")))) {
        db$Base <- db$Base[!is.na(match(db$Base$Type, types)), ]
        db[-1L] <-
            lapply(db[-1L],
                   function(e) {
                       e[!is.na(match(e$ID, db$Base$ID)), ]
                   })
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
	db[] <-
	    lapply(db,
		   function(e) {
		       e[!is.na(match(e$Package, package)), ]
		   })
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

    dbBase <- db$Base
    search_fun <- if(fuzzy) {
        function(x) {
	    agrep(pattern, x, ignore.case = ignore.case,
		  max.distance = max.distance)
        }
    } else {
        function(x) {
            grep(pattern, x, ignore.case = ignore.case,
                 perl = use_UTF8)
        }
    }
    search_db_results <- function(p, f, e)
        data.frame(Position = p, Field = f, Entry = e,
                   stringsAsFactors = FALSE)
    search_db_field <- function(field) {
	switch(field,
	       alias = {
		   aliases <- db$Aliases$Alias
                   matched <- search_fun(aliases)
                   search_db_results(match(db$Aliases$ID[matched],
                                           dbBase$ID),
                                     rep.int(field, length(matched)),
                                     aliases[matched])
	       },
	       concept = {
		   concepts <- db$Concepts$Concept
                   matched <- search_fun(concepts)
                   search_db_results(match(db$Concepts$ID[matched],
                                           dbBase$ID),
                                     rep.int(field, length(matched)),
                                     concepts[matched])
	       },
	       keyword = {
		   keywords <- db$Keywords$Keyword
                   matched <- search_fun(keywords)
                   search_db_results(match(db$Keywords$ID[matched],
                                           dbBase$ID),
                                     rep.int(field, length(matched)),
                                     keywords[matched])
	       },
               ## Alternatively, generically use field mapped to title
               ## case.
               name = {
                   matched <- search_fun(dbBase$Name)
                   search_db_results(matched,
                                     rep.int("Name", length(matched)),
                                     dbBase$Name[matched])
               },
               title = {
                   matched <- search_fun(dbBase$Title)
                   search_db_results(matched,
                                     rep.int("Title", length(matched)),
                                     dbBase$Title[matched])
               }
               )
    }

    matches <- NULL
    for(f in fields)
        matches <- rbind(matches, search_db_field(f))
    matches <- matches[order(matches$Position), ]
    db <- cbind(dbBase[matches$Position,
                       c("Topic", "Title", "Name", "ID",
                         "Package", "LibPath", "Type"),
                       drop = FALSE],
                matches[c("Field", "Entry")])
    rownames(db) <- NULL
    if(verbose>= 2L) {
        n_of_objects_matched <- length(unique(db[, "ID"]))
        message(sprintf(ngettext(n_of_objects_matched,
                                 "matched %d object.",
                                 "matched %d objects."),
                        n_of_objects_matched),
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

hsearch_db <-
function(package = NULL, lib.loc = NULL,
         types = getOption("help.search.types"),
         verbose = getOption("verbose"),
         rebuild = FALSE, use_UTF8 = FALSE)
{
    WINDOWS <- .Platform$OS.type == "windows"
    if(is.logical(verbose)) verbose <- 2 * as.integer(verbose)
    if(is.null(lib.loc))
	lib.loc <- .libPaths()
    i <- pmatch(types, hsearch_db_types)
    if (anyNA(i))
	stop("incorrect type specification")
    else
	types <- hsearch_db_types[i]

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
	   any(is.na(match(types, attr(db, "Types")))) ||
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
        if(!is.null(package) &&
           any(is.na(match(package, db$Base[, "Package"]))))
            rebuild <- TRUE
    }
    if(rebuild) {
	if(verbose > 0L) {
            message("Rebuilding the help.search() database", " ", "...",
                    if(verbose > 1L) "...", domain = NA)
            flush.console()
        }

        want_type_help <- any(types == "help")
        want_type_demo <- any(types == "demo")
        want_type_vignette <- any(types == "vignette")

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

        ## Empty hsearch index:
        hDB0 <- tools:::.build_hsearch_index(NULL)

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

            hDB <- NULL
            if(want_type_help) {
                if(file.exists(hs_file <-
                    file.path(path, "Meta", "hsearch.rds"))) {
                    hDB <- readRDS(hs_file)
                    if(!is.null(hDB)) {
                        ## Fill up possibly missing information.
                        if(is.na(match("Encoding", colnames(hDB[[1L]]))))
                            hDB[[1L]] <- cbind(hDB[[1L]], Encoding = "")
                        ## <FIXME>
                        ## Transition fro old-style to new-style colnames.
                        ## Remove eventually.
                        for(i in seq_along(hDB)) {
                            colnames(hDB[[i]]) <-
                                tools:::hsearch_index_colnames[[i]]
                        }
                        ## </FIXME>
                    } else if(verbose >= 2L) {
                        message(gettextf("package %s has empty hsearch data - strangely",
                                         sQuote(p)),
                                domain = NA)
                        flush.console()
                    }
                } else if(!is.null(package))
                      warning("no hsearch.rds meta data for package ", p,
                              domain = NA)
            }
            if(is.null(hDB))
                hDB <- hDB0
            nh <- NROW(hDB[[1L]])
            hDB[[1L]] <- cbind(hDB[[1L]], Type = rep("help", nh))
            if(nh)
                hDB[[1L]][, "LibPath"] <- path
            if(want_type_vignette)
                hDB <- merge_vignette_index(hDB, path, p)
            if(want_type_demo)
                hDB <- merge_demo_index(hDB, path, p)
            ## Put the hsearch index for the np-th package into the
            ## np-th row of the matrix used for aggregating.
            dbMat[np, seq_along(hDB)] <- hDB
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
        rownames(db$Base) <- NULL
        ## <FIXME>
        ## Remove eventually ...
	if(is.null(db$Concepts)) {
	    db$Concepts <-
                matrix(character(), ncol = 3L,
                       dimnames =
                           list(NULL,
                                tools:::hsearch_index_colnames$Concepts))
        }
        ## </FIXME>

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
                message("reencoding ...", appendLF = FALSE, domain = NA)
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
                              u[rowSums(is.na(nchar(u, "chars",
                                                    allowNA = TRUE,
                                                    keepNA = FALSE))) > 0,
                                "ID"]))
        ## FIXME: drop this fallback
	if(length(bad_IDs)) {           # try latin1
            for(i in seq_along(db)) {
                ind <- db[[i]][, "ID"] %in% bad_IDs
                db[[i]][ind, ] <- iconv(db[[i]][ind, ], "latin1", "")
            }
            bad_IDs <-
                unlist(sapply(db,
                              function(u)
                                  u[rowSums(is.na(nchar(u, "chars",
                                                        allowNA = TRUE,
                                                        keepNA = FALSE))) > 0,
                                    "ID"]))
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

        ## Drop entries without topic as these cannot be accessed.
        ## (These come from help pages without \alias.)
        bad_IDs <- db$Base[is.na(db$Base[, "Topic"]), "ID"]
        if(length(bad_IDs)) {
	    for(i in seq_along(db)) {
		ind <- db[[i]][, "ID"] %in% bad_IDs
		db[[i]] <- db[[i]][!ind, ]
	    }
	}

        ## Remove keywords which are empty or package.skeleton()
        ## leftovers.
        ind <- is.na(match(db$Keywords[, "Keyword"],
                           c("", "~kwd1", "~kwd2",
                             "~~ other possible keyword(s) ~~")))
        db$Keywords <- db$Keywords[ind, , drop = FALSE]
        ## Remove concepts which are empty.
        ind <- nzchar(db$Concepts[, "Concept"])
        db$Concepts <- db$Concepts[ind, , drop = FALSE]

        ## Map non-standard keywords to concepts, and use the
        ## descriptions of the standard keywords as concepts, with the
        ## exception of keyword 'internal'.
        standard <- .get_standard_Rd_keywords_with_descriptions()
        keywords <- standard$Keywords
        concepts <- standard$Descriptions
        pos <- match(db$Keywords[, "Keyword"], keywords)
        ind <- !is.na(pos) & (keywords[pos] != "internal")
        db$Concepts <-
            rbind(db$Concepts,
                  db$Keywords[is.na(pos), , drop = FALSE],
                  cbind(concepts[pos[ind]],
                        db$Keywords[ind, -1L, drop = FALSE]))
        db$Keywords <- db$Keywords[!is.na(pos), , drop = FALSE]

        ## Doing this earlier will not work: in particular, re-encoding
        ## is written for character matrices.
        db <- lapply(db, as.data.frame,
                     stringsAsFactors = FALSE, row.names = NULL)

        if(verbose >= 2L) {
            message("saving the database ...", appendLF = FALSE, domain = NA)
            flush.console()
        }
        attr(db, "LibPaths") <- lib.loc
        attr(db, "mtime") <- Sys.time()
        attr(db, "ctype") <- Sys.getlocale("LC_CTYPE")
        attr(db, "Types") <- unique(c("help", types))
        class(db) <- "hsearch_db"
        .hsearch_db(db)
        if(verbose >= 2L) {
            message(" ", "done", domain = NA)
            flush.console()
        }
        if(verbose > 0L) {
            message("... database rebuilt", domain = NA)
            if(WINDOWS) {
                close(pb)
                on.exit()               # clear closing of progress bar
            }
            flush.console()
        }
    }

    db
}

## Cf. tools:::.get_standard_Rd_keywords().
.get_standard_Rd_keywords_with_descriptions <-
function()
{
    lines <- readLines(file.path(R.home("doc"), "KEYWORDS.db"))
    ## Strip top-level entries.
    lines <- grep("^.*\\|([^:]*):.*", lines, value = TRUE)
    ## Strip comments.
    lines <- sub("[[:space:]]*#.*", "", lines)
    list(Keywords = sub("^.*\\|([^:]*):.*", "\\1", lines),
         Descriptions = sub(".*:[[:space:]]*", "", lines))
}

## This extra indirection allows the Mac GUI to replace this
## yet call the printhsearchInternal function.
print.hsearch <-
function(x, ...)
    printhsearchInternal(x, ...)

printhsearchInternal <-
function(x, ...)
{
    help_type <- getOption("help_type", default = "text")
    types <- x$types
    if (help_type == "html") {
        browser <- getOption("browser")
        port <- tools::startDynamicHelp(NA)
	if (port > 0L) {
            .hsearch_results(x)
            url <- paste0("http://127.0.0.1:", port,
                          "/doc/html/Search?results=1")
            ## <NOTE>
            ## Older versions used the following, which invokes the
            ## dynamic HTML help system in a way that this calls
            ## help.search() to give the results to be displayed.
            ## This is now avoided by passing the (already available)
            ## results to the dynamic help system using the dynamic
            ## variable .hsearch_results().
	    ## url <-
            ##     paste0("http://127.0.0.1:", port,
            ##            "/doc/html/Search?pattern=",
            ##            tools:::escapeAmpersand(x$pattern),
            ##            paste0("&fields.", x$fields, "=1",
            ##                   collapse = ""),
            ##            if (!is.null(x$agrep)) paste0("&agrep=", x$agrep),
            ##            if (!x$ignore.case) "&ignore.case=0",
            ##            if (!identical(types,
            ##                           getOption("help.search.types")))
            ##                paste0("&types.", types, "=1",
            ##                       collapse = ""),
            ##            if (!is.null(x$package))
            ##                paste0("&package=",
            ##                       paste(x$package, collapse=";")),
            ##            if (!identical(x$lib.loc, .libPaths()))
            ##                paste0("&lib.loc=",
            ##                       paste(x$lib.loc, collapse=";"))
            ##            )
            ## </NOTE>
            browseURL(url, browser)
            return(invisible(x))
        }
    }
    hfields <- paste(x$fields, collapse = " or ")
    vfieldnames <-
        c(alias = "name", concept = "keyword", keyword = NA,
          name = "name", title = "title")
    vfieldnames <- vfieldnames[x$fields]
    vfields <- paste(unique(vfieldnames[!is.na(vfieldnames)]),
                     collapse = " or ")
    dfieldnames <-
        c(alias = "name", concept = NA, keyword = NA,
          name = "name", title = "title")
    dfieldnames <- dfieldnames[x$fields]
    dfields <- paste(unique(dfieldnames[!is.na(dfieldnames)]),
                     collapse = " or ")
    fields_used <-
        list(help = hfields, vignette = vfields, demo = dfields)
    matchtype <- switch(x$type, fuzzy = "fuzzy", "regular expression")
    typenames <-
        c(vignette = "Vignettes", help = "Help files", demo = "Demos")
    fields_for_match_details <-
        list(help = c("alias", "concept", "keyword"),
             vignette = c("concept"),
             demo = character())
    field_names_for_details <-
        c(alias = "Aliases", concept = "Concepts", keyword = "Keywords")

    db <- x$matches
    if(NROW(db) == 0) {
    	typenames <- paste(tolower(typenames[types]), collapse= " or ")
	writeLines(strwrap(paste("No", typenames,
                                 "found with", fields_used$help,
				 "matching", sQuote(x$pattern),
				 "using", matchtype,
                                 "matching.")))
        return(invisible(x))
    }

    outFile <- tempfile()
    outConn <- file(outFile, open = "w")
    typeinstruct <-
        c(vignette =
              paste("Type 'vignette(\"FOO\", package=\"PKG\")' to",
                    "inspect entries 'PKG::FOO'."),
          help =
              paste("Type '?PKG::FOO' to",
                    "inspect entries 'PKG::FOO',",
                    "or 'TYPE?PKG::FOO' for entries like",
                    "'PKG::FOO-TYPE'."),
          demo =
              paste("Type 'demo(PKG::FOO)' to",
                    "run demonstration 'PKG::FOO'."))

    for(type in types) {
	if(NROW(dbtemp <- db[db[, "Type"] == type, , drop = FALSE]) > 0) {
	    writeLines(c(strwrap(paste(typenames[type], "with",
                                       fields_used[[type]], "matching",
                                       sQuote(x$pattern), "using",
                                       matchtype, "matching:")),
			 "\n"),
		       outConn)
            fields <- fields_for_match_details[[type]]
            chunks <- split.data.frame(dbtemp,
                                       paste0(dbtemp[, "Package"],
                                              "::",
                                              dbtemp[ , "Topic"]))
            nms <- names(chunks)
            for(i in seq_along(nms)) {
                chunk <- chunks[[i]]
                writeLines(formatDL(nms[i], chunk[1L, "Title"]),
                           outConn)
                matches <- Filter(length,
                                  split(chunk[, "Entry"],
                                        chunk[, "Field"])[fields])
                if(length(matches)) {
                    tags <- field_names_for_details[names(matches)]
                    vals <- vapply(matches, paste, "", collapse = ", ")
                    writeLines(strwrap(paste0(tags, ": ", vals),
                                       indent = 2L, exdent = 4L),
                               outConn)
                }
            }
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

.hsearch_results <-
local({
    res <- NULL
    function(new) {
	if(!missing(new))
	    res <<- new
	else
	    res
    }
})

hsearch_db_concepts <-
function(db = hsearch_db())
{
    ## <NOTE>
    ## This should perhaps get an ignore.case = TRUE argument.
    ## </NOTE>
    pos <- match(db$Concepts[, "ID"], db$Base[, "ID"])
    entries <- split(as.data.frame(db$Base[pos, ],
                                   stringsAsFactors = FALSE),
                     db$Concepts[, "Concept"])
    enums <- sapply(entries, NROW)
    pnums <- sapply(entries, function(e) length(unique(e$Package)))
    pos <- order(enums, pnums, decreasing = TRUE)
    data.frame(Concept = names(entries)[pos],
               Frequency = enums[pos],
               Packages = pnums[pos],
               stringsAsFactors = FALSE,
               row.names = NULL)
}

hsearch_db_keywords <-
function(db = hsearch_db())
{
    pos <- match(db$Keywords[, "ID"], db$Base[, "ID"])
    entries <- split(as.data.frame(db$Base[pos, ],
                                   stringsAsFactors = FALSE),
                     db$Keywords[, "Keyword"])
    enums <- sapply(entries, NROW)
    pnums <- sapply(entries, function(e) length(unique(e$Package)))
    standard <- .get_standard_Rd_keywords_with_descriptions()
    concepts <- standard$Descriptions[match(names(entries),
                                            standard$Keywords)]
    pos <- order(enums, pnums, decreasing = TRUE)
    data.frame(Keyword = names(entries)[pos],
               Concept = concepts[pos],
               Frequency = enums[pos],
               Packages = pnums[pos],
               stringsAsFactors = FALSE,
               row.names = NULL)
}

print.hsearch_db <-
function(x, ...)
{
    writeLines(c("A help search database:",
                 sprintf("Objects: %d, Aliases: %d, Keywords: %d, Concepts: %d",
                         NROW(x$Base),
                         NROW(x$Aliases),
                         NROW(x$Keywords),
                         NROW(x$Concepts))))
    invisible(x)
}
