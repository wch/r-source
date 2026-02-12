#  File src/library/tools/R/bibtools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2025-2026 The R Core Team
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
    ind <- grepl("::", keys, fixed = TRUE)
    brp <- if(!all(ind)) {
               ## Bibentries for base R and current package maybe.
               rdfile <- processRdChunk_data_store()$Rdfile
               dir <- dirname(normalizePath(rdfile, mustWork = FALSE))
               if(basename(dir) %in% c("unix", "windows"))
                   dir <- dirname(dir)
               dir <- if(basename(dir) == "man") {
                          dir <- dirname(dir)
                          c(dir, file.path(dir, "inst"))
                      } else character()
               c(R_bibentries(),
                 .bibentries_from_REFERENCES(dir))
           } else NULL
    if(!any(ind)) {
        ## Special-case for efficiency.
        bib <- brp
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
                       brp
                   else {
                       dir <- system.file(package = pj)
                       .bibentries_from_REFERENCES(dir)
                   }
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

.bibentries_from_REFERENCES <-
function(dir)
{
    for(d in dir[nzchar(dir)]) {
        if(file.exists(path <- file.path(d, "REFERENCES.rds")))
            return(readRDS(path))
        else if(file.exists(path <- file.path(d, "REFERENCES.R")))
            return(utils::readCitationFile(path,
                                           list(Encoding = "UTF-8")))
        else if(file.exists(path <- file.path(d, "REFERENCES.bib")))
            return(`names<-`(bibtex::read.bib(path), NULL))
    }
    utils::bibentry()        
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
function(dir)
{
    dir <- file_path_as_absolute(dir)

    ## Base packages are special as they have no partial Rd db, and
    ## Rd_db() re-orders sections after processing Sexprs, so we need to
    ## go via .build_Rd_db() which allows step control.
    ## For non-base packages, we really need build/partial.rdb (which we
    ## only have for the package sources) to check that all bibentry
    ## macros were properly expanded.
    ## There is no need to process \Sexpr code here (and we are not
    ## prepared to find the installed package/REFERENCES anyway).
    db <- .build_Rd_db(dir, step = 1L, stages = NULL)
    if(length(db)) {
        first <- nchar(file.path(dir, "man")) + 2L
        names(db) <- substring(names(db), first)
    }

    x <- Filter(length, lapply(db, .bibentries_cited_or_shown))
    if(!length(x))
        return(NULL)

    u <- FALSE

    if(basename(dir) %notin% .get_standard_package_names()$base) {
        ## Check whether we got everything from the build stage
        ## expansions.  Catch if built with \bib* stubs/unknowns.
        built_file <- file.path(dir, "build", "partial.rdb")
        if(!file.exists(built_file)) {
            u <- TRUE
        } else {
            y <- lapply(readRDS(built_file)[names(x)],
                        .bibentries_cited_or_shown)
            g <- function(u, v) {
                is.null(v) || # page not in partial.rdb (no build-stage macros)
                              # or built with undefined macros
                    sum(startsWith(v[,3L], "\\Sexpr")) != nrow(u)
            }
            if(any(unlist(Map(g, x, y), use.names = FALSE)))
                u <- TRUE
            else
                x <- y
        }
    }

    f <- function(x) {
        if(!length(x))
            return(NULL)
        cache <- cited <- shown <- character()
        for(e in split(x, row(x))) {
            if(e[1L] != "\\bibshow") {
                given <- .bibkeys_from_cite(e[2L])
                cited <- unique(c(cited, given))
                cache <- unique(c(cache, given))
            } else {
                given <- .bibkeys_from_show(e[2L])
                if(!length(given))
                    cited <- character()
                else {
                    if(any(given == "*"))
                        given <- c(given[given != "*"], cache)
                    shown <- unique(c(shown, given))
                }
            }
        }
        setdiff(cited, shown)
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
