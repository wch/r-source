#  File src/library/tools/R/CRANtools.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2014-2015 The R Core Team
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
        ## Remove all ok stubs.
        details <- details[details$Check != "*", ]
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

CRAN_rdxrefs_db <-
function()
    read_CRAN_object(CRAN_baseurl_for_src_area(),
                     "src/contrib/Meta/rdxrefs.rds")

check_CRAN_mirrors <-
function(mirrors = NULL, verbose = FALSE)
{
    retry_upon_error <- function(expr, n = 3L) {
        i <- 1L
        repeat {
            y <- tryCatch(expr, error = identity)
            if(!inherits(y, "error") || (i >= n))
                break
            i <- i + 1L
        }
        y
    }

    read_package_db <- function(baseurl) {
        path <- sprintf("%ssrc/contrib/PACKAGES.gz", baseurl)
        db <- retry_upon_error({
            con <- gzcon(url(path, "rb"))
            on.exit(close(con))
            readLines(con)
        })
        if(inherits(db, "error")) {
            msg <- sprintf("Reading %s failed with message: %s",
                           path, conditionMessage(db))
            return(simpleError(msg))
        }
        db
    }

    read_timestamp <- function(baseurl, path) {
        path <- sprintf("%s%s", baseurl, path)
        ts <- retry_upon_error(readLines(path))
        if(inherits(ts, "error")) {
            msg <- sprintf("Reading %s failed with message: %s",
                           path, conditionMessage(ts))
            return(simpleError(msg))
        }
        as.POSIXct(as.numeric(ts), origin = "1970-01-01")
    }

    if_ok <- function(u, v) if(inherits(u, "error")) u else v

    check_mirror <- function(mirror) {
        mirror_packages <- read_package_db(mirror)
        mirror_ts1 <- read_timestamp(mirror, path_ts1)
        mirror_ts2 <- read_timestamp(mirror, path_ts2)
        mirror_ts3 <- read_timestamp(mirror, path_ts3)

        list("PACKAGES" =
             if_ok(mirror_packages,
                   c("Delta_master_mirror" =
                         sprintf("%d/%d",
                                 length(setdiff(master_packages,
                                                mirror_packages)),
                                 length(master_packages)),
                     "Delta_mirror_master" =
                         sprintf("%d/%d",
                                 length(setdiff(mirror_packages,
                                                master_packages)),
                                 length(mirror_packages)))),
             "TIME" =
             if_ok(mirror_ts1, difftime(master_ts1, mirror_ts1)),
             "TIME_r-release" =
             if_ok(mirror_ts2, difftime(master_ts2, mirror_ts2)),
             "TIME_r-old-release" =
             if_ok(mirror_ts3, difftime(master_ts3, mirror_ts3))
             )
    }

    master <- "http://CRAN.R-project.org/"
    path_ts1 <- "TIME"
    path_ts2 <- "bin/windows/contrib/r-release/TIME_r-release"
    path_ts3 <- "bin/windows/contrib/r-old-release/TIME_r-old-release"

    master_packages <- read_package_db(master)
    master_ts1 <- read_timestamp(master, path_ts1)
    master_ts2 <- read_timestamp(master, path_ts2)
    master_ts3 <- read_timestamp(master, path_ts3)

    if(is.null(mirrors)) {
        mirrors <- as.character(utils::getCRANmirrors(all = TRUE)$URL)
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

CRAN_mirror_maintainers_info <-
function(mirrors, db = NULL)
{
    if(is.null(db))
        db <- utils::getCRANmirrors(all = TRUE)
    mirrors <- sort(unique(mirrors))
    ind <- match(mirrors, as.character(db$URL))
    addresses <- db[ind, "Maintainer"]
    addresses <- gsub("[[:space:]]*#[[:space:]]*", "@", addresses)
    to <- paste(unique(unlist(strsplit(addresses,
                                       "[[:space:]]*,[[:space:]]*"))),
                collapse = ",\n    ")
    len <- length(addresses)
    body <- c(if(len > 1L) {
                  "Dear maintainers,"
              } else {
                  "Dear maintainer,"
              },
              "",
              strwrap(paste(if(length(mirrors) > 1L) {
                                "This concerns the following CRAN mirrors"
                            } else {
                                "This concerns the following CRAN mirror"
                            },
                            "maintained by",
                            if(len > 1L) "one of",
                            "you:")),
              "",
              paste0("  ", formatDL(mirrors, addresses, style = "list"))
              )
    list(to = to, body = body)
}

CRAN_mirror_mirmon_status <-
function()
{
    ## See
    ## <http://www.projects.science.uu.nl/csg/mirmon/mirmon.html#state_file_format>.

    fields <-
        c("url",
          "age",
          "status_last_probe",
          "time_last_successful_probe",
          "probe_history",
          "state_history",
          "last_probe")
    ts_to_POSIXct <- function(ts) {
        suppressWarnings(as.POSIXct(as.numeric(as.character(ts)),
                                    origin = "1970-01-01"))
    }
    read_mirmon_state_file <- function(con) {
        db <- utils::read.table(con, header = FALSE, col.names = fields)
        db$url <- as.character(db$url)
        db$age <- ts_to_POSIXct(db$age)
        db$time_last_successful_probe <-
            ts_to_POSIXct(db$time_last_successful_probe)
        db$last_probe <- ts_to_POSIXct(db$last_probe)
        db$delta <- difftime(Sys.time(), db$age, units = "days")
        db
    }
    state_files <-
        c("TIME" = "mirror.state",
          "TIME_r-release" = "mirror_release.state",
          "TIME_r-old-release" = "mirror_old_release.state")

    ## Need to always use master for now (the mirrors do not have the
    ## state files).
    do.call(rbind,
            c(Map(function(u, v) {
                      u <- paste0("https://cran.r-project.org/mirmon/data/", u)
                      cbind(read_mirmon_state_file(u),
                            timestamp = v,
                            stringsAsFactors = FALSE)
                  },
                  state_files,
                  names(state_files)),
              list(make.row.names = FALSE)))
}


CRAN_Rd_xref_db_with_expansions <-
function()
{
    db <- CRAN_rdxrefs_db()
    ## Flatten:
    db <- cbind(do.call(rbind, db),
                rep.int(names(db), sapply(db, NROW)))
    colnames(db) <- c(colnames(db)[1L : 2L], "S_File", "S_Package")
    unique(cbind(db, .expand_anchored_Rd_xrefs(db)))
}

CRAN_Rd_xref_available_target_ids <-
function()
{
    targets <- lapply(CRAN_aliases_db(), .Rd_available_xref_targets)
    .Rd_object_id(rep.int(names(targets), lengths(targets)),
                  unlist(targets, use.names = FALSE))
}

CRAN_Rd_xref_reverse_depends <-
function(packages, db = NULL, details = FALSE)
{
    if(is.null(db))
        db <- CRAN_Rd_xref_db_with_expansions()
    y <- split.data.frame(db, db[, "T_Package"])[packages]
    if(!details)
        y <- lapply(y, function(e) unique(e[, "S_Package"]))
    y
}

CRAN_Rd_xref_problems <-
function()
{
    y <- list()

    db <- CRAN_Rd_xref_db_with_expansions()
    db <- db[nzchar(db[, "T_Package"]), , drop = FALSE]
    ## Add ids:
    db <- cbind(db,
                T_ID = .Rd_object_id(db[, "T_Package"], db[, "T_File"]))

    ## Do we have Rd xrefs to current CRAN packages which no longer work?
    current <- rownames(CRAN_current_db())
    db1 <- db[!is.na(match(db[, "T_Package"], current)), , drop = FALSE]
    y$broken_xrefs_to_current_CRAN_packages <-
        db1[is.na(match(db1[, "T_ID"],
                        CRAN_Rd_xref_available_target_ids())), ,
            drop = FALSE]

    ## Do we have Rd xrefs "likely" to archived CRAN packages?
    ## This is a bit tricky because packages could have been archived on
    ## CRAN but still be available from somewhere else.  The code below
    ## catches availability in standard repositories, but not in
    ## additional repositories.
    archived <- setdiff(names(CRAN_archive_db()),
                        c(rownames(utils::available.packages(filters = list())),
                          unlist(.get_standard_package_names(),
                                 use.names = FALSE)))
    y$xrefs_likely_to_archived_CRAN_packages <-
        db[!is.na(match(db[, "T_Package"], archived)), , drop = FALSE]

    y
}

.Rd_available_xref_targets <-
function(aliases)
{
    ## Argument aliases as obtained from Rd_aliases(), or directly by
    ## calling
    ##   lapply(rddb, .Rd_get_metadata, "alias")
    ## on an Rd db.
    unique(c(unlist(aliases, use.names = FALSE),
             sub("\\.[Rr]d", "", basename(names(aliases)))))
}

.Rd_object_id <-
function(package, nora)
{
    ## Name OR Alias: nora.
    sprintf("%s::%s", package, nora)
}

CRAN_package_maintainers_db <-
function()
{
    db <- CRAN_package_db()
    maintainer <- db[, "Maintainer"]
    address <- tolower(sub(".*<(.*)>.*", "\\1", maintainer))
    maintainer <- gsub("\n", " ", maintainer)
    cbind(Package = db[, "Package"],
          Address = address,
          Maintainer = maintainer)
}

CRAN_package_maintainers_info <-
function(packages, db = NULL)
{
    if(is.null(db))
        db <- CRAN_package_maintainers_db()
    ind <- match(packages, db[, "Package"])
    addresses <- db[ind, "Address"]
    to <- paste(sort(unique(addresses)), collapse = ",\n    ")
    lst <- split(db[ind, "Package"], db[ind, "Maintainer"])
    len <- length(addresses)
    body <- c(if(len > 1L) {
                  "Dear maintainers,"
              } else {
                  "Dear maintainer,"
              },
              "",
              if(length(packages) > 1L) {
                  "This concerns the CRAN packages"
              } else {
                  "This concerns the CRAN package"
              },
              "",
              paste(strwrap(paste(sort(packages), collapse = " "),
                            indent = 2L, exdent = 2L),
                    collapse = "\n"),
              "",
              paste("maintained by",
                    if(len > 1L) "one of",
                    "you:"),
              "",
              paste0("  ",
                     formatDL(vapply(lst, paste, "", collapse = " "),
                              style = "list"))
              )
    list(to = to, body = body)
}
    
CRAN_reverse_depends_and_views <-
function(packages)
{
    a <- utils::available.packages(filters = list())

    v <- read_CRAN_object(CRAN_baseurl_for_src_area(),
                          "src/contrib/Views.rds")
    v <- do.call("rbind",
                 mapply(cbind,
                        Package =
                        lapply(v, function(e) e$packagelist$name),
                        View = sapply(v, "[[", "name")))
    v <- split(v[, 2L], v[, 1L])

    r <- package_dependencies(packages, a, reverse = TRUE)
    rr <- package_dependencies(packages, a,
                               reverse = TRUE, recursive = TRUE)
    rrs <- package_dependencies(packages, a, "Suggests",
                                reverse = TRUE, recursive = TRUE)

    rxrefs <- CRAN_Rd_xref_reverse_depends(packages)

    y <- lapply(packages,
                function(p) {
                    c(Package = p,
                      if(length(z <- r[[p]])) {
                          c("Reverse depends" =
                            paste(sort(z), collapse = " "),
                            if(length(zz <- setdiff(rr[[p]], z))) {
                                c("Additional recursive reverse depends" =
                                  paste(sort(zz), collapse = " "))
                            })
                      },
                      if(length(z <- rrs[[p]])) {
                          c("Reverse recursive suggests" =
                            paste(sort(z), collapse = " "))
                      },
                      if(length(z <- rxrefs[[p]])) {
                          c("Reverse Rd xref depends" =
                            paste(sort(z), collapse = " "))
                      },
                      if(length(z <- v[[p]])) {
                          c("Views" = paste(sort(z), collapse = " "))
                      })
                })
    class(y) <- "CRAN_reverse_depends_and_views"
    y
}
    
format.CRAN_reverse_depends_and_views <-
function(x, ...)
{
    vapply(x,
           function(e) {
               paste(formatDL(e, style = "list", indent = 2L),
                     collapse = "\n")
           },
           "")
}

print.CRAN_reverse_depends_and_views <-
function(x, ...)
{
    writeLines(paste(format(x, ...), collapse = "\n\n"))
    invisible(x)
}
