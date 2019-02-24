##  File src/library/tools/R/doitools.R
##  Part of the R package, https://www.R-project.org
##
##  Copyright (C) 2015-2016 The R Core Team
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
    db <- data.frame(DOI = trimws(as.character(dois)),
                     Parent = as.character(parents),
                     stringsAsFactors = FALSE)
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
    doi_db(dois, rep.int("DESCRIPTION", length(dois)))
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
    meta <- .read_description(file.path(dir, "DESCRIPTION"))
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
function(db, verbose = FALSE)
{
    use_curl <-
        config_val_to_logical(Sys.getenv("_R_CHECK_URLS_USE_CURL_",
                                         "TRUE")) &&
        requireNamespace("curl", quietly = TRUE)
    
    .gather <- function(d = character(),
                        p = list(),
                        s = rep.int("", length(d)),
                        m = rep.int("", length(d))) {
        y <- data.frame(DOI = d, From = I(p), Status = s, Message = m,
                        stringsAsFactors = FALSE)
        y$From <- p
        class(y) <- c("check_doi_db", "data.frame")
        y
    }

    .fetch <- function(u, d) {
        if(verbose) message(sprintf("processing %s", d))
        tryCatch(curlGetHeaders(u), error = identity)
    }

    .check <- function(d) {
        u <- paste0("https://doi.org/", d)
        ## Do we need to percent encode parts of the DOI name?
        h <- .fetch(u, d)
        if(inherits(h, "error")) {
            s <- "-1"
            msg <- sub("[[:space:]]*$", "", conditionMessage(h))
        } else {
            s <- as.character(attr(h, "status"))
            msg <- table_of_HTTP_status_codes[s]
        }

        ## Similar to URLs, see e.g.
        ##   curl -I -L https://doi.org/10.1016/j.csda.2009.12.005
        ## (As of 2016-12, this actually gives 400 Bad Request.)
        if(any(grepl("301 Moved Permanently", h, useBytes = TRUE))) {
            ind <- grep("^[Ll]ocation: ", h, useBytes = TRUE)
            new <- sub("^[Ll]ocation: ([^\r]*)\r\n", "\\1", h[max(ind)])
            if((s == "503") && grepl("www.sciencedirect.com", new))
                s <- "405"
        }

        if((s != "200") && use_curl) {
            g <- .curl_GET_status(u)
            if(g == "200") {
                s <- g
                msg <- "OK"
            }
        }

        c(s, msg)
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

    ## See <https://www.doi.org/doi_handbook/2_Numbering.html#2.2>:
    ##   The DOI prefix shall be composed of a directory indicator
    ##   followed by a registrant code. These two components shall be
    ##   separated by a full stop (period).
    ##   The directory indicator shall be "10".
    ind <- !startsWith(dois, "10")
    if(any(ind)) {
        len <- sum(ind)
        bad <- rbind(bad,
                     .gather(dois[ind],
                             parents[ind],
                             m = rep.int("Invalid DOI", len)))
    }

    pos <- which(!ind)
    if(length(pos)) {
        results <- do.call(rbind, lapply(dois[pos], .check))
        status <- as.numeric(results[, 1L])
        ind <- (status %notin% c(200L, 405L))
        if(any(ind)) {
            pos <- pos[ind]
            s <- as.character(status[ind])
            s[s == "-1"] <- "Error"
            m <- results[ind, 2L]
            m[is.na(m)] <- ""
            bad <- rbind(bad,
                         .gather(dois[pos],
                                 parents[pos],
                                 m,
                                 s))
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
