#  File src/library/tools/R/bibtools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2025 The R Core Team
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

R_bibliographies_dir <-
function()
    file.path(R.home("share"), "bibliographies")

R_bibentries <-
function()
{
    readRDS(file.path(R_bibliographies_dir(), "R.rds"))
}

## utils:::.bibentry_get_key
.bibentry_get_key <-
function (x)
{
    if(!length(x))
        return(character())
    keys <- lapply(unclass(x), attr, "key")
    keys[!lengths(keys)] <- ""
    unlist(keys, use.names = FALSE)
}

.bibentries_from_keys <-
function(keys)
{
    keys <- keys[nzchar(keys)]
    bad <- character()
    if(!any(ind <- grepl("::", keys, fixed = TRUE))) {
        ## Special-case for efficiency.
        bib <- R_bibentries()
        pos <- match(keys, .bibentry_get_key(bib),
                     nomatch = 0L)
        bad <- keys[pos == 0L]
        y <- bib[pos]
    } else {
        n <- length(keys)
        pkgs <- character(n)
        pkgs[ind] <- sub("::.*", "", keys[ind])
        i <- split(seq_len(n), pkgs)
        y <- vector("list", length(i))
        for(j in seq_along(i)) {
            pj <- names(i)[j]
            bib <- if(!nzchar(pj))
                       R_bibentries()
                   else if(nzchar(path <- system.file("REFERENCES.rds",
                                                      package = pj)))
                       readRDS(path)
                   else if(nzchar(path <- system.file("REFERENCES.R",
                                                      package = pj)))
                       utils::readCitationFile(path,
                                               list(Encoding = "UTF-8"))
                   else if(nzchar(path <- system.file("REFERENCES.bib",
                                                      package = pj)))
                       `names<-`(bibtex::read.bib(path), NULL)
                   else
                       utils::bibentry()
            kj <- keys[i[[j]]]
            pos <- match(sub(".*::", "", kj),
                         .bibentry_get_key(bib),
                         nomatch = 0L)
            bib <- bib[pos]
            bib$key <- as.list(kj[pos > 0L])
            y[[j]] <- bib
        }
        y <- do.call(c, y)
        pos <- match(keys, .bibentry_get_key(y), nomatch = 0L)
        bad <- keys[pos == 0L]
        y <- y[pos]
    }
    if(length(bad)) {
        msg <- paste(c("Could not find bibentries for the following keys:",
                       .strwrap22(sQuote(bad))),
                     collapse = "\n")
        warning(msg, call. = FALSE)
    }
    y
}

.bibentries_from_bibtex <-
function(file, text)
{
    if(missing(file)) {
        tf <- tempfile()
        on.exit(unlink(tf))
        writeLines(enc2utf8(text), tf, useBytes = TRUE)
        file <- tf
    }
    bib <- bibtex::read.bib(file, encoding = "UTF-8")
    key <- lapply(bib,
                  function(e) {
                      a <- e$author
                      if(is.null(a))
                          a <- e$editor
                      a <- a[seq_len(min(length(a), 3L))]
                      sprintf("R:%s:%s",
                              paste(gsub("[^[:alpha:]-]", "_",
                                         unlist(a$family)),
                                    collapse = "+"),
                              e$year)
                  })
    do.call(c, Map(function(u, v) { u$key <- v; u }, bib, key))
}

.read_bibentries <-
function(file)
    utils::readCitationFile(file, list(Encoding = "UTF-8"))

.dump_bibentries <-
function(bib, con = stdout())
    writeLines(paste(format(bib, "R"), collapse = "\n\n"), con,
               useBytes = TRUE)

.persons_from_bibentry <-
function(bib)
{
    aut <- lapply(unclass(bib), `[[`, "author")
    len <- lengths(aut)
    ind <- (len > 0L)
    aut[ind] <- Map(`names<-`,
                    aut[ind],
                    Map(rep.int,
                        .bibentry_get_key(bib[ind]),
                        len[ind]))
    edi <- lapply(unclass(bib), `[[`, "editor")
    len <- lengths(edi)
    ind <- (len > 0L)
    edi[ind] <- Map(`names<-`,
                    edi[ind],
                    Map(rep.int,
                        .bibentry_get_key(bib[ind]),
                        len[ind]))
    do.call(c, c(aut, edi))
}

.check_Rd_bibentries_cited_not_shown <-
function(package, dir, lib.loc = NULL)
{
    db <- if(!missing(package))
              Rd_db(package, lib.loc)
          else
              Rd_db(dir = dir)
    x <- Filter(length, lapply(db, .bibentries_cited_or_shown))
    if(!length(x))
        return(NULL)
    u <- FALSE
    ## Check whether we got everything from the build stage expansions.
    if(!missing(package))
        dir <- system.file(package = package, lib.loc = lib.loc)
    f <- file.path(dir, "build", "partial.rdb")
    if(!file.exists(f)) {
        u <- TRUE
    } else {
        y <- lapply(readRDS(f)[names(x)], .bibentries_cited_or_shown)
        ## Cannot simply use identical() as entries in the partial Rd db
        ## are subject to section re-ordering.
        g <- function(u, v) {
            length(setdiff(split(u, row(u)), split(v, row(v))) > 0L)
        }
        if(any(unlist(Map(g, x, y), use.names = FALSE)))
            u <- TRUE
        else
            x <- y
    }
    f <- function(x) {
        if(!length(x))
            return(NULL)
        delta <- cited <- character()
        for(e in split(x, row(x))) {
            if(e[1L] != "\\bibshow") {
                cited <- c(cited, .bibkeys_from_cite(e[2L]))
            } else {
                given <- c(cited, .bibkeys_from_show(e[2L]))
                if(!length(given)) {
                    delta <- c(delta, cited)
                    cited <- character()
                } else {
                    if(any(given == "*"))
                        given <- c(given[given != "*"], cited)
                    cited <- setdiff(cited, given)
                }
            }
        }
        c(delta, cited)
    }
    y <- Filter(length, lapply(x, f))
    if(u) attr(y, "unexpected_macro_expansion") <- TRUE
    y
}

.bibentries_cited_or_shown <- function(x) {
    tab <- NULL
    recurse <- function(e) {
        if(identical(attr(e, "Rd_tag"), "USERMACRO") &&
           ((m <- attr(e, "macro")) %in%
            c("\\bibcitep", "\\bibcitet", "\\bibshow")))
            tab <<- rbind(tab, c(m, e[[2L]], e[[1L]]))
        else if(is.list(e))
            lapply(e, recurse)
    }
    if(getDynamicFlags(x)["\\Sexpr"])
        lapply(x, recurse)
    tab
}

## This somewhat duplicates the code in the helpers: ideally we could
## have the same code for extracting the keys ...
.bibkeys_from_cite <-
function(x)
{
    x <- trimws(x)
    given <- strsplit(x, "(?<!\\\\),[[:space:]]*", perl = TRUE)[[1L]]
    parts <- regmatches(given, gregexpr("|", given, fixed = TRUE),
        invert = TRUE)
    keys <- rep_len("", length(parts))
    if (any(ind <- (lengths(parts) == 1L))) {
        keys[ind] <- unlist(parts[ind], use.names = FALSE)
    }
    if (any(ind <- (lengths(parts) == 3L))) {
        keys[ind] <- vapply(parts[ind], `[`, "", 2L)
    }
    keys
}

.bibkeys_from_show <- function(x)
{
    x <- trimws(x)
    strsplit(x, ",[[:space:]]*")[[1L]]
}
