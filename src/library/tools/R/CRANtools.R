#  File src/library/tools/R/CRANtools.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2014 The R Core Team
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

summarize_CRAN_check_status <-
function(package, results = NULL, details = NULL, mtnotes = NULL)
{
    if(is.null(results))
        results <- CRAN_check_results()
    results <-
        results[!is.na(match(results$Package, package)) & !is.na(results$Status), ]

    if(!NROW(results)) {
        s <- character(length(package))
        names(s) <- package
        return(s)
    }

    if(any(results$Status != "OK")) {
        if(is.null(details))
            details <- CRAN_check_details()
        details <- details[!is.na(match(details$Package, package)), ]
        ## Remove trailing white space from outputs ... remove eventually
        ## when this is done on CRAN.
        details$Output <- sub("[[:space:]]+$", "", details$Output)

    } else {
        ## Create empty details directly to avoid the cost of reading
        ## and subscripting the actual details db.
        details <- as.data.frame(matrix(character(), ncol = 7L),
                                 stringsAsFactors = FALSE)
        names(details) <-
            c("Package", "Version", "Flavor", "Check", "Status", "Output",
              "Flags")
    }

    if(is.null(mtnotes))
        mtnotes <- CRAN_memtest_notes()


    summarize_results <- function(p, r) {
        if(!NROW(r)) return(character())
        tab <- table(r$Status)[c("ERROR", "WARN", "NOTE", "OK")]
        tab <- tab[!is.na(tab)]
        paste(c(sprintf("Current CRAN status: %s",
                        paste(sprintf("%s: %s", names(tab), tab),
                              collapse = ", ")),
                sprintf("See: <http://CRAN.R-project.org/web/checks/check_results_%s.html>",
                        p)),
              collapse = "\n")
    }

    summarize_details <- function(p, d) {
        if(!NROW(d)) return(character())

        pof <- which(names(d) == "Flavor")
        poo <- which(names(d) == "Output")
        ## Outputs from checking "whether package can be installed" will
        ## have a machine-dependent final line
        ##    See ....... for details.
        ind <- d$Check == "whether package can be installed"
        if(any(ind)) {
            d[ind, poo] <-
                sub("\nSee[^\n]*for details[.]$", "", d[ind, poo])
        }
        txt <- apply(d[-pof], 1L, paste, collapse = "\r")
        ## Outputs from checking "installed package size" will vary
        ## according to system.
        ind <- d$Check == "installed package size"
        if(any(ind)) {
            txt[ind] <-
                apply(d[ind, - c(pof, poo)],
                      1L, paste, collapse = "\r")
        }

        ## Canonicalize fancy quotes.
        ## Could also try using iconv(to = "ASCII//TRANSLIT"))
        txt <- .canonicalize_quotes(txt)
        out <-
            lapply(split(seq_len(NROW(d)), match(txt, unique(txt))),
                   function(e) {
                       tmp <- d[e[1L], ]
                       flags <- tmp$Flags
                       flavors <- d$Flavor[e]
                       c(sprintf("Version: %s", tmp$Version),
                         if(nzchar(flags)) sprintf("Flags: %s", flags),
                         sprintf("Check: %s, Result: %s", tmp$Check, tmp$Status),
                         sprintf("  %s",
                                 gsub("\n", "\n  ", tmp$Output,
                                      perl = TRUE, useBytes = TRUE)),
                         sprintf("See: %s",
                                 paste(sprintf("<http://www.r-project.org/nosvn/R.check/%s/%s-00check.html>",
                                               flavors,
                                               p),
                                       collapse = ",\n     ")))
                   })
        paste(unlist(lapply(out, paste, collapse = "\n")),
              collapse = "\n\n")
    }

    summarize_mtnotes <- function(p, m) {
        if(!length(m)) return(character())
        tests <- m[, "Test"]
        paths <- m[, "Path"]
        isdir <- !grepl("-Ex.Rout$", paths)
        if(any(isdir))
            paths[isdir] <- sprintf("%s/", paths[isdir])
        paste(c(paste("Memtest notes:",
                      paste(unique(tests), collapse = " ")),
                sprintf("See: %s",
                        paste(sprintf("<http://www.stats.ox.ac.uk/pub/bdr/memtests/%s/%s>",
                                      tests,
                                      paths),
                              collapse = ",\n     "))),
              collapse = "\n")
    }

    summarize <- function(p, r, d, m) {
        paste(c(summarize_results(p, r),
                summarize_mtnotes(p, m),
                summarize_details(p, d)),
              collapse = "\n\n")
    }

    s <- if(length(package) == 1L) {
        summarize(package, results, details, mtnotes[[package]])
    } else {
        results <- split(results, factor(results$Package, package))
        details <- split(details, factor(details$Package, package))
        unlist(lapply(package,
                      function(p) {
                          summarize(p,
                                    results[[p]],
                                    details[[p]],
                                    mtnotes[[p]])
                      }))
    }

    names(s) <- package
    class(s) <- "summarize_CRAN_check_status"
    s
}

format.summarize_CRAN_check_status <-
function(x, header = NA, ...)
{
    if(is.na(header)) header <- (length(x) > 1L)
    if(header) {
        s <- sprintf("Package: %s", names(x))
        x <- sprintf("%s\n%s\n\n%s", s, gsub(".", "*", s), x)
    }
    x
}

print.summarize_CRAN_check_status <-
function(x, ...)
{
    writeLines(paste(format(x, ...), collapse = "\n\n"))
    invisible(x)
}


## CRAN_check_results <-
## function()
## {
##     ## This allows for partial local mirrors, or to
##     ## look at a more-freqently-updated mirror
##     CRAN_repos <- Sys.getenv("R_CRAN_WEB", getOption("repos")["CRAN"])
##     rds <- gzcon(url(sprintf("%s/%s", CRAN_repos,
##                              "web/checks/check_results.rds"),
##                      open = "rb"))
##     results <- readRDS(rds)
##     close(rds)
##
##     results
## }

## CRAN_check_details <-
## function()
## {
##     CRAN_repos <- Sys.getenv("R_CRAN_WEB", getOption("repos")["CRAN"])
##     rds <- gzcon(url(sprintf("%s/%s", CRAN_repos,
##                              "web/checks/check_details.rds"),
##                      open = "rb"))
##     details <- readRDS(rds)
##     close(rds)
##
##     details
## }

## CRAN_memtest_notes <-
## function()
## {
##     CRAN_repos <- Sys.getenv("R_CRAN_WEB", getOption("repos")["CRAN"])
##     rds <- gzcon(url(sprintf("%s/%s", CRAN_repos,
##                              "web/checks/memtest_notes.rds"),
##                      open = "rb"))
##     mtnotes <- readRDS(rds)
##     close(rds)
##
##     mtnotes
## }

CRAN_baseurl_for_src_area <-
function()
    .get_standard_repository_URLs()[1L]

## This allows for partial local mirrors, or to look at a
## more-freqently-updated mirror.
CRAN_baseurl_for_web_area <-
function()
    Sys.getenv("R_CRAN_WEB", getOption("repos")["CRAN"])

read_CRAN_object <-
function(cran, path)
{
    con <- gzcon(url(sprintf("%s/%s", cran, path),
                     open = "rb"))
    on.exit(close(con))
    readRDS(con)
}

CRAN_check_results <- 
function(flavors = NULL)
{
    db <- read_CRAN_object(CRAN_baseurl_for_web_area(),
                           "web/checks/check_results.rds")
    if(!is.null(flavors))
        db <- db[!is.na(match(db$Flavor, flavors)), ]
    db
}

CRAN_check_details <-
function(flavors = NULL)
{
    db <- read_CRAN_object(CRAN_baseurl_for_web_area(),
                           "web/checks/check_details.rds")
    if(!is.null(flavors))
        db <- db[!is.na(match(db$Flavor, flavors)), ]
    ## <FIXME>
    ## Remove eventually ...
    class(db) <- c("CRAN_check_details", "check_details", "data.frame")
    ## </FIXME>
    db
}

CRAN_memtest_notes <-
function()
    read_CRAN_object(CRAN_baseurl_for_web_area(),
                     "web/checks/memtest_notes.rds")

CRAN_package_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_web_area(),
                     "web/packages/packages.rds")

CRAN_aliases_db <-
function()    
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/aliases.rds")

CRAN_archive_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/archive.rds")

CRAN_current_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/current.rds")

check_CRAN_mirrors <-
function(mirrors = NULL, verbose = FALSE)
{
    read_package_db <- function(baseurl) {
        con <- gzcon(url(sprintf("%s/src/contrib/PACKAGES.gz", baseurl),
                     "rb"))
        on.exit(close(con))
        readLines(con)
    }

    check_mirror <- function(mirror) {
        mirror_packages <- tryCatch(read_package_db(mirror),
                                    error = identity)
        if(inherits(mirror_packages, "error"))
            stop(sprintf("Reading %ssrc/contrib/PACKAGES.gz failed with message: %s",
                         mirror, conditionMessage(mirror_packages)))
        mirror_ts1 <- tryCatch(readLines(sprintf("%s%s", mirror,
                                                 path_ts1)),
                               error = identity)
        if(inherits(mirror_ts1, "error"))
            stop(sprintf("Reading %s%s failed with message: %s",
                         mirror, path_ts1, conditionMessage(mirror_ts1)))
        mirror_ts2 <- tryCatch(readLines(sprintf("%s%s", mirror,
                                                 path_ts2)),
                               error = identity)
        if(inherits(mirror_ts2, "error"))
            stop(sprintf("Reading %s%s failed with message: %s",
                         mirror, path_ts2, conditionMessage(mirror_ts2)))

        list("PACKAGES" =
             c("Delta_master_mirror" =
               sprintf("%d/%d",
                       length(setdiff(master_packages,
                                      mirror_packages)),
                       length(master_packages)),
               "Delta_mirror_master" =
               sprintf("%d/%d",
                       length(setdiff(mirror_packages,
                                      master_packages)),
                       length(mirror_packages))),
             "TIME_r-release" =
             difftime(as.POSIXct(as.numeric(master_ts1),
                                 origin = "1970-01-01"),
                      as.POSIXct(as.numeric(mirror_ts1),
                                 origin = "1970-01-01")),
             "TIME_r-old-release" =
             difftime(as.POSIXct(as.numeric(master_ts2),
                                 origin = "1970-01-01"),
                      as.POSIXct(as.numeric(mirror_ts2),
                                 origin = "1970-01-01"))
             )
    }

    master <- "http://CRAN.R-project.org/"
    master_packages <- read_package_db(master)
    path_ts1 <- "bin/windows/contrib/r-release/TIME_r-release"
    master_ts1 <- readLines(sprintf("%s%s", master, path_ts1))
    path_ts2 <- "bin/windows/contrib/r-old-release/TIME_r-old-release"
    master_ts2 <- readLines(sprintf("%s%s", master, path_ts2))

    if(is.null(mirrors)) {
        mirrors <- as.character(getCRANmirrors()$URL)
    }

    results <- lapply(mirrors,
                      function(m) {
                          if(verbose)
                              message(sprintf("Checking %s", m))
                          suppressWarnings(tryCatch(check_mirror(m),
                                                    error = identity))
                      })
    names(results) <- mirrors

    results
}
