#  File src/library/tools/R/htmltools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2022-2024 The R Core Team
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

tidy_validate <-
function(f, tidy = "tidy") {
    ## HTML Tidy complains about empty spans, which may be "ok" (and
    ## KaTeX uses these to achieve vertical alignment).
    ## One can suppress these complaints via command line option
    ##   "--drop-empty-elements no"
    ## but this suppresses complaints about all empty elements (and not
    ## only spans).
    ## To allow experimenting, we provide env var
    ##   _R_CHECK_RD_VALIDATE_RD2HTML_OPTS_
    ## for setting command line options.  As of 2024-03, by default we
    ## leave this empty, and filter out
    ##    Warning: trimming empty <span>
    ## messages when checking the Rd2HTML refman conversions.
    z <- suppressWarnings(system2(tidy,
                                  c("-language en", "-qe",
                                    Sys.getenv("_R_CHECK_RD_VALIDATE_RD2HTML_OPTS_",
                                               ""),
                                    f),
                                  stdout = TRUE, stderr = TRUE))
    if(!length(z)) return(NULL)
    ## Strip trailing \r from HTML Tidy output on Windows:
    z <- trimws(z, which = "right")
    ## (Alternatively, replace '$' by '[ \t\r\n]+$' in the regexp below.)
    s <- readLines(f, warn = FALSE)
    m <- regmatches(z,
                    regexec("^line ([0-9]+) column ([0-9]+) - (.+)$",
                            z))
    m <- unique(do.call(rbind, m[lengths(m) == 4L]))
    p <- m[, 2L]
    concordance <- as.Rconcordance(grep("^<!-- concordance:", s, value = TRUE))
    result <- cbind(line = p, col = m[, 3L], msg = m[, 4L], txt = s[as.numeric(p)])
    
    if (!is.null(concordance))
    	result <- cbind(result, matchConcordance(p, concordance = concordance))
    
    result
}

tidy_validate_db <-
function(x, paths = NULL, ignore = character()) {
    if(!is.null(paths))
        names(x) <- paths
    i <- vapply(x, inherits, NA, "error")
    e <- x[i]
    x <- Filter(length, x[!i])
    if(!length(x) && !length(e)) return(NULL)
    y <- do.call(rbind, x)
    if(is.null(y)) {
        y <- list() # cannot set an attr on NULL
    } else {
        y <- cbind(path = rep.int(names(x), vapply(x, nrow, 0)), y)
        if(length(ignore)) {
            y <- y[y[, "msg"] %notin% ignore, , drop = FALSE]
        }
    }
    if(length(e))
        attr(y, "errors") <- e
    y
}

tidy_validate_files <-
function(files, verbose = interactive()) {
    tidy_validate_db(lapply(files,
                            function(f) {
                                if(verbose)
                                    message(sprintf("Processing %s ...",
                                                    f))
                                tidy_validate(f)
                            }),
                     files)
}

tidy_validate_R_httpd_path <-
function(path) {
    y <- tryCatch(httpd(path, query = NULL), error = identity)
    if(inherits(y, "error"))
        return(y)
    if(!is.null(f <- y$file)) {
        ## Should only do this for appropriate content types
        if(is.null(y$"content-type"))
            tidy_validate(f)
        else
            NULL
    } else if(!is.null(payload <- y$payload)) {
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(payload, f)
        tidy_validate(f)
    } else NULL
}

tidy_validate_package_Rd_files <-
function(package, dir, lib.loc = NULL, auto = NA, verbose = interactive())
{
    if(!missing(dir))
        return(tidy_validate_package_Rd_files_from_dir(dir, auto, verbose))

    if(!length(package)) return(NULL)

    n <- 3L

    one <- function(p) {
        if(verbose)
            message(sprintf("* Package: %s", p))
        db <- Rd_db(p, lib.loc = lib.loc)
        files <- sub("[Rr]d$", "html", basename(names(db)))
        results <-
            lapply(files,
                   function(f) {
                       if(verbose)
                           message(sprintf("Processing %s ...", f))
                       path <- sprintf("/library/%s/html/%s", p, f)
                       tryCatch(tidy_validate_R_httpd_path(path),
                                error = identity)
                   })
        ## names(results) <- sprintf("%s/%s", p, files)
        ## results <- Filter(length, results)
        ## if(!length(results)) return(NULL)
        ## cbind(file = rep.int(names(results), vapply(results, nrow, 0)),
        ##       do.call(rbind, results))
        tidy_validate_db(results, sprintf("%s/%s", p, files))
    }

    do.call(rbind, lapply(package, one))
}

tidy_validate_package_Rd_files_from_dir <- function(dir, auto = NA, verbose) {

    if(!length(dir)) return(NULL)

    out <- tempfile()
    on.exit(unlink(out))

    one <- function(d) {
        if(verbose)
            message(sprintf("* Package: %s", basename(d)))
        db <- Rd_db(dir = d)
        if(!is.na(auto)) {
            is <- vapply(db,
                         function(e) {
                             g <- attr(e, "meta")$generator
                             (is.character(g) &&
                              (length(g) == 1L) &&
                              startsWith(g, "% Generated by roxygen2"))
                         },
                         NA)
            db <- db[if(auto) is else !is]
        }
        results <-
            lapply(db,
                   function(x) {
                       tryCatch({
                           Rd2HTML(x, out, concordance = TRUE)
                           tidy_validate(out)
                       },
                       error = identity)
                   })
        tidy_validate_db(results,
                         sprintf("%s::%s", basename(d), names(db)))
    }

    do.call(rbind, lapply(dir, one))
}


tidy_validate_urls <-
function(urls, verbose = interactive()) {
    destfile <- tempfile("tidy_validate")
    on.exit(unlink(destfile))
    tidy_validate_db(lapply(urls,
                            function(u) {
                                if(verbose)
                                    message(sprintf("Processing %s ...",
                                                    u))
                                utils::download.file(u, destfile,
                                                     quiet = TRUE)
                                tidy_validate(destfile)
                            }),
                     urls)
}
