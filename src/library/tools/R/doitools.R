##  File src/library/tools/R/doitools.R
##  Part of the R package, https://www.R-project.org
##
##  Copyright (C) 2015-2023 The R Core Team
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  https://www.R-project.org/Licenses/

doi_db <-
function(dois, parents)
{
    db <- list2DF(list(DOI = trimws(as.character(dois)),
                       Parent = as.character(parents)))
    class(db) <- c("doi_db", "data.frame")
    db
}

doi_db_from_package_metadata <- 
function(meta)
{
    dois <- character()
    pattern <- "<(DOI|doi):([^>]*)>"
    if(!is.na(v <- meta["Description"])) {
        m <- gregexpr(pattern, v)
        dois <- c(dois, .gregexec_at_pos(pattern, v, m, 3L))
    }
    ## DOI names may contain ">", but we need this as a delimiter when
    ## writing the names in <doi:name> style.  So at least ">" and hence
    ## also "%" must be percent encoded ...
    doi_db(utils::URLdecode(dois), rep.int("DESCRIPTION", length(dois)))
}

doi_db_from_package_citation <-
function(dir, meta, installed = FALSE)
{
    dois <- character()
    path <- if(installed) "CITATION" else file.path("inst", "CITATION")
    cfile <- file.path(dir, path)
    if(file.exists(cfile)) {
        cinfo <- .read_citation_quietly(cfile, meta)
        if(!inherits(cinfo, "error"))
            dois <- trimws(unique(unlist(cinfo$doi, use.names = FALSE)))
    }
    doi_db(dois, rep.int(path, length(dois)))
}

## \doi a user-defined macro (from system.Rd) which gets expanded by 
## parse_Rd().  To extract programmatically, we try to find the user
## macros with the (current) expansion.
## Alternative, we could call .build_Rd_db() on the package Rd sources
## with e.g. macros = c("\\newcommand{\\doi}{<DOI:#1>}" and look for
## TEXT nodes matching the expansion.  However, we cannot necessarily
## safely process build-time Sexprs ...

doi_db_from_package_Rd_db <-
function(db)
{
    dois <- Filter(length, lapply(db, .get_dois_from_Rd))
    doi_db(.canonicalize_doi(unlist(dois, use.names = FALSE)),
           rep.int(file.path("man", names(dois)),
                   lengths(dois)))
}

.get_dois_from_Rd <-
function(x)
{
    dois <- character()
    recurse <- function(e) {
        if(identical(attr(e, "Rd_tag"), "USERMACRO") &&
           identical(attr(e, "macro"), "\\doi"))
            dois <<- c(dois, e[2L])
        else if(is.list(e))
            lapply(e, recurse)
    }
    if(getDynamicFlags(x)["\\Sexpr"])
        lapply(x, recurse)
    dois
}

doi_db_from_package_sources <-
function(dir, add = FALSE, Rd = FALSE)
{
    meta <- .get_package_metadata(dir, FALSE)
    db <- rbind(doi_db_from_package_metadata(meta),
                doi_db_from_package_citation(dir, meta),
                if(Rd) {
                    rddb <- Rd_db(dir = dir)
                    doi_db_from_package_Rd_db(rddb)
                })
    if(add)
        db$Parent <- file.path(basename(dir), db$Parent)
    db
}

doi_db_from_installed_packages <-
function(packages, lib.loc = NULL, verbose = FALSE, Rd = FALSE)
{
    if(!length(packages)) return()
    one <- function(p) {
        if(verbose)
            message(sprintf("processing %s", p))
        dir <- system.file(package = p, lib.loc = lib.loc)
        if(dir == "") return()
        meta <- .read_description(file.path(dir, "DESCRIPTION"))
        db <- rbind(doi_db_from_package_metadata(meta),
                    doi_db_from_package_citation(dir, meta,
                                                 installed = TRUE),
                    if(Rd) {
                        rddb <- Rd_db(p, lib.loc = dirname(dir))
                        doi_db_from_package_Rd_db(rddb)
                    })
        db$Parent <- file.path(p, db$Parent)
        db
    }
    do.call(rbind,
            c(lapply(packages, one),
              list(make.row.names = FALSE)))
}

check_doi_db <-
function(db, verbose = FALSE, parallel = FALSE, pool = NULL)
{
    if(parallel && is.null(pool))
        pool <- curl::new_pool()    
    
    .gather <- function(d = character(),
                        p = list(),
                        s = rep.int("", length(d)),
                        m = rep.int("", length(d))) {
        y <- list2DF(list(DOI = d, From = p, Status = s, Message = m))
        class(y) <- c("check_doi_db", "data.frame")
        y
    }

    .fetch_headers <-
        if(parallel)
            function(urls, dois)
                .fetch_headers_via_curl(urls, verbose, pool)
        else
            function(urls, dois)
                .fetch_headers_via_base(urls, verbose, dois)

    .check <- function(h) {
        if(inherits(h, "error")) {
            s <- "Error"
            m <- sub("[[:space:]]*$", "", conditionMessage(h))
        } else {
            s <- as.character(attr(h, "status"))
            m <- table_of_HTTP_status_codes[s]
        }
        c(s, m)
    }

    bad <- .gather()

    if(!NROW(db)) return(bad)

    if(inherits(db, "check_doi_db")) {
        ## Allow re-checking check results.
        parents <- db$From
        dois <- db$DOI
    } else {
        parents <- split(db$Parent, db$DOI)
        dois <- names(parents)
    }

    ## <FIXME>
    ## According to <https://www.iana.org/assignments/urn-formal/doi>,
    ##   The 2022 edition of ISO 26324 has amended the syntax of the
    ##   prefix by removing the requirement for the directory indicator
    ##   to be "10" and allow also DOI names without a registrant code.
    ## (ISO 26324 is the DOI standard).
    ## As of 2023-06-06, this is not yet reflected in the DOI Handbook
    ## (<https://doi.org/10.1000/182>) last updated on 2019-12-19, which
    ## still says in
    ## <https://www.doi.org/the-identifier/resources/handbook/2_numbering#2.2>
    ## that
    ##   The DOI prefix shall be composed of a directory indicator
    ##   followed by a registrant code. These two components shall be
    ##   separated by a full stop (period).
    ##   The directory indicator shall be "10".
    ## Nevertheless, let us drop the check below:
    ## <CODE>
    ## ind <- !startsWith(dois, "10")
    ## </CODE>
    ## But do at least minimal tests for formal validity (could do
    ## more):
    ind <- !grepl("/", dois, fixed = TRUE)
    if(any(ind)) {
        len <- sum(ind)
        bad <- rbind(bad,
                     .gather(dois[ind], parents[ind],
                             m = rep.int("Invalid DOI", len)))
    }
    pos <- which(!ind)
    ## </FIXME>

    ## See <https://www.doi.org/the-identifier/resources/handbook/3_resolution#3.8.3>:
    ##   Ideally we would perform GET requests and would look at the
    ##   responseCode in the JSON response.  However, we cannot do this
    ##   with base, and at least for now we can also check using HEAD
    ##   requests and looking at the status code (200 vs 404).
    if(length(pos)) {
        doispos <- dois[pos]
        urlspos <- paste0("https://doi.org/api/handles/",
                          vapply(doispos, urlify_doi, ""))
        ## Do we need to percent encode parts of the DOI name?
        headers <- .fetch_headers(urlspos, doispos)
        results <- do.call(rbind, lapply(headers, .check))
        status <- results[, 1L]
        ind <- (status != "200")
        if(any(ind)) {
            pos <- pos[ind]
            s <- status[ind]
            m <- results[ind, 2L]
            m[is.na(m)] <- ""
            bad <- rbind(bad,
                         .gather(dois[pos], parents[pos], s, m))
        }
    }

    bad
}

format.check_doi_db <-
function(x, ...)
{
    if(!NROW(x)) return(character())

    paste0(sprintf("DOI: %s", x$DOI),
           sprintf("\nFrom: %s",
                   vapply(x$From, paste, "", collapse = "\n      ")),
           ifelse((s <- x$Status) == "",
                  "",
                  sprintf("\nStatus: %s", s)),
           ifelse((m <- x$Message) == "",
                  "",
                  sprintf("\nMessage: %s", m)))
}

print.check_doi_db <-
function(x, ...)
{
    if(NROW(x))
        writeLines(paste(format(x), collapse = "\n\n"))
    invisible(x)
}

check_package_dois <-
function(dir, verbose = FALSE)
{
    db <- doi_db_from_package_sources(dir, Rd = TRUE)
    check_doi_db(db, verbose = verbose, parallel = TRUE)
}
