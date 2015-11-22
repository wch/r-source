#  File src/library/tools/R/urltools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2015 The R Core Team
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

get_IANA_URI_scheme_db <-
function()
{
    ## See
    ## <http://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml>.
    baseurl <- "http://www.iana.org/assignments/uri-schemes/"
    db <- utils::read.csv(url(paste0(baseurl, "uri-schemes-1.csv")),
                          stringsAsFactors = FALSE)
    names(db) <- chartr(".", "_", names(db))
    db
}

parse_URL_reference <-
function(x)
{
    ## See RFC_3986 <http://www.ietf.org/rfc/rfc3986.txt>.
    re <- "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
    if(length(x)) {
        y <- do.call(rbind, regmatches(x, regexec(re, x)))
        y <- y[, c(3, 5, 6, 8, 10), drop = FALSE]
    } else {
        y <- matrix(character(), 0L, 5L)
    }
    colnames(y) <- c("scheme", "authority", "path", "query", "fragment")
    y
}

.get_urls_from_Rd <-
function(x)
{
    urls <- character()
    recurse <- function(e) {
        tag <- attr(e, "Rd_tag")
        ## Rd2HTML and Rd2latex remove whitespace and \n from URLs.
        if(identical(tag, "\\url")) {
            urls <<-
                c(urls, trimws(gsub("\n", "", .Rd_deparse(e, tag = FALSE),
                                    fixed = TRUE, useBytes = TRUE)))
        } else if(identical(tag, "\\href")) {
            urls <<-
                c(urls, trimws(gsub("\n", "",
                                    .Rd_deparse(e[[1L]], tag = FALSE),
                                    fixed = TRUE, useBytes = TRUE)))
        } else if(is.list(e))
              lapply(e, recurse)
    }
    lapply(x, recurse)
    unique(trimws(urls))
}

.get_urls_from_HTML_file <-
function(f)
{
    hrefs <- character()
    XML::htmlParse(f,
                   handlers =
                       list(a = function(node) {
                           href <- XML::xmlAttrs(node)["href"]
                           ## <FIXME>
                           ## ? XML::xmlAttrs says the value is always a
                           ## named character vector, but
                           ##   XML::xmlAttrs(XML::xmlNode("a", "foobar"))
                           ## gives NULL ...
                           ## Hence, use an extra is.null() test.
                           if(!is.null(href) && !is.na(href))
                               hrefs <<- c(hrefs, href)
                           ## </FIXME>
                       })
                   )
    unique(unname(hrefs[!grepl("^#", hrefs)]))
}

url_db <-
function(urls, parents)
{
    ## Some people get leading LFs in URLs, so trim before checking.
    db <- data.frame(URL = trimws(as.character(urls)),
                     Parent = as.character(parents),
                     stringsAsFactors = FALSE)
    class(db) <- c("url_db", "data.frame")
    db
}

url_db_from_package_Rd_db <-
function(db)
{
    urls <- Filter(length, lapply(db, .get_urls_from_Rd))
    url_db(unlist(urls, use.names = FALSE),
           rep.int(file.path("man", names(urls)),
                   lengths(urls)))
}

url_db_from_package_metadata <-
function(meta)
{
    gregexec_at_pos <- function(pattern, v, m, pos) {
        unlist(lapply(regmatches(v, m),
                      function(e)
                          do.call(rbind,
                                  regmatches(e,
                                             regexec(pattern, e)))[, 3L]
                      ),
               use.names = FALSE)
    }

    urls <- character()
    fields <- c("URL", "BugReports")
    for(v in meta[fields]) {
        if(is.na(v)) next
        pattern <-
            "<(URL: *)?((https?|ftp)://[^[:space:],]*)[[:space:]]>"
        m <- gregexpr(pattern, v)
        urls <- c(urls, gregexec_at_pos(pattern, v, m, 3L))
        regmatches(v, m) <- ""
        pattern <- "(^|[^>\"])((https?|ftp)://[^[:space:],]*)"
        m <- gregexpr(pattern, v)
        urls <- c(urls, gregexec_at_pos(pattern, v, m, 3L))
    }

    url_db(urls, rep.int("DESCRIPTION", length(urls)))
}

url_db_from_package_citation <-
function(dir, meta, installed = FALSE)
{
    urls <- character()
    path <- if(installed) "CITATION" else file.path("inst", "CITATION")
    cfile <- file.path(dir, path)
    if(file.exists(cfile)) {
        cinfo <- .read_citation_quietly(cfile, meta)
        if(!inherits(cinfo, "error"))
            urls <- trimws(unique(unlist(cinfo$url, use.names = FALSE)))
    }
    url_db(urls, rep.int(path, length(urls)))
}

url_db_from_package_news <-
function(dir, installed = FALSE)
{
    urls <- character()
    path <- if(installed) "NEWS.Rd" else file.path("inst", "NEWS.Rd")
    nfile <- file.path(dir, path)
    if(file.exists(nfile)) {
        macros <- loadRdMacros(file.path(R.home("share"),
                                         "Rd", "macros", "system.Rd"))
        urls <- .get_urls_from_Rd(prepare_Rd(tools::parse_Rd(nfile,
                                                             macros = macros),
                                             stages = "install"))
    }
    url_db(urls, rep.int(path, length(urls)))
}

url_db_from_package_HTML_files <-
function(dir, installed = FALSE)
{
    urls <- parents <- character()
    path <- if(installed) "doc" else file.path("inst", "doc")
    files <- Sys.glob(file.path(dir, path, "*.html"))
    if(installed && file.exists(rfile <- file.path(dir, "README.html")))
        files <- c(files, rfile)
    if(length(files)) {
        urls <- lapply(files, .get_urls_from_HTML_file)
        names(urls) <- files
        urls <- Filter(length, urls)
        if(length(urls)) {
            parents <- rep.int(.file_path_relative_to_dir(names(urls),
                                                          dir),
                               lengths(urls))
            urls <- unlist(urls, use.names = FALSE)
        }
    }
    url_db(urls, parents)
}

url_db_from_package_README_md <-
function(dir, installed = FALSE)
{
    urls <- path <- character()
    rfile <- Filter(file.exists,
                    c(if(!installed) file.path("inst", "README.md"),
                      "README.md"))[1L]
    if(!is.na(rfile) && nzchar(Sys.which("pandoc"))) {
        path <- .file_path_relative_to_dir(rfile, dir)
        tfile <- tempfile("README", fileext = ".html")
        on.exit(unlink(tfile))
        out <- .pandoc_md_for_CRAN(rfile, tfile)
        if(!out$status) {
            urls <- .get_urls_from_HTML_file(tfile)
        }
    }
    url_db(urls, rep.int(path, length(urls)))
}

url_db_from_package_NEWS_md <-
function(dir, installed = FALSE)
{
    urls <- path <- character()
    nfile <- Filter(file.exists,
                    c(if(!installed) file.path("inst", "NEWS.md"),
                      "NEWS.md"))[1L]
    if(!is.na(nfile) && nzchar(Sys.which("pandoc"))) {
        path <- .file_path_relative_to_dir(nfile, dir)
        tfile <- tempfile("NEWS", fileext = ".html")
        on.exit(unlink(tfile))
        out <- .pandoc_md_for_CRAN(nfile, tfile)
        if(!out$status) {
            urls <- .get_urls_from_HTML_file(tfile)
        }
    }
    url_db(urls, rep.int(path, length(urls)))
}

url_db_from_package_sources <-
function(dir, add = FALSE) {
    meta <- .read_description(file.path(dir, "DESCRIPTION"))
    db <- rbind(url_db_from_package_metadata(meta),
                url_db_from_package_Rd_db(Rd_db(dir = dir)),
                url_db_from_package_citation(dir, meta),
                url_db_from_package_news(dir))
    if(requireNamespace("XML", quietly = TRUE)) {
        db <- rbind(db,
                    url_db_from_package_HTML_files(dir),
                    url_db_from_package_README_md(dir),
                    url_db_from_package_NEWS_md(dir)
                    )
    }
    if(add)
        db$Parent <- file.path(basename(dir), db$Parent)
    db
}

url_db_from_installed_packages <-
function(packages, lib.loc = NULL, verbose = FALSE)
{
    if(!length(packages)) return()
    one <- function(p) {
        if(verbose)
            message(sprintf("processing %s", p))
        dir <- system.file(package = p, lib.loc = lib.loc)
        if(dir == "") return()
        meta <- .read_description(file.path(dir, "DESCRIPTION"))
        rddb <- Rd_db(p, lib.loc = dirname(dir))
        db <- rbind(url_db_from_package_metadata(meta),
                    url_db_from_package_Rd_db(rddb),
                    url_db_from_package_citation(dir, meta,
                                                 installed = TRUE),
                    url_db_from_package_news(dir, installed = TRUE))
        if(requireNamespace("XML", quietly = TRUE)) {
            db <- rbind(db,
                        url_db_from_package_HTML_files(dir,
                                                       installed = TRUE),
                        url_db_from_package_README_md(dir,
                                                      installed = TRUE),
                        url_db_from_package_NEWS_md(dir,
                                                    installed = TRUE)
                        )
        }
        db$Parent <- file.path(p, db$Parent)
        db
    }
    do.call(rbind,
            c(lapply(packages, one),
              list(make.row.names = FALSE)))
}

get_IANA_HTTP_status_code_db <-
function()
{
    ## See
    ## <http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml>
    baseurl <- "http://www.iana.org/assignments/http-status-codes/"
    db <- utils::read.csv(url(paste0(baseurl, "http-status-codes-1.csv")),
                          stringsAsFactors = FALSE)
    ## Drop "Unassigned".
    db[db$Description != "Unassigned", ]
}

## See <http://en.wikipedia.org/wiki/List_of_FTP_server_return_codes>
## and <http://tools.ietf.org/html/rfc959>,
## Section 4.2.2 "Numeric Order List of Reply Codes",
## and <https://tools.ietf.org/html/rfc2228>,
## Section 5 "New FTP Replies".
## Only need those >= 400.
table_of_FTP_server_return_codes <-
    c("421" = "Service not available, closing control connection.",
      "425" = "Can't open data connection.",
      "426" = "Connection closed; transfer aborted.",
      "430" = "Invalid username or password",
      "431" = "Need some unavailable resource to process security.",
      "434" = "Requested host unavailable.",
      "450" = "Requested file action not taken.",
      "451" = "Requested action aborted: local error in processing.",
      "452" = "Requested action not taken.  Insufficient storage space in system.",
      "500" = "Syntax error, command unrecognized.",
      "501" = "Syntax error in parameters or arguments.",
      "502" = "Command not implemented.",
      "503" = "Bad sequence of commands.",
      "504" = "Command not implemented for that parameter.",
      "530" = "Not logged in.",
      "532" = "Need account for storing files.",
      "533" = "Command protection level denied for policy reasons.",
      "534" = "Request denied for policy reasons.",
      "535" = "Failed security check (hash, sequence, etc).",
      "536" = "Requested PROT level not supported by mechanism.",
      "537" = "Command protection level not supported by security mechanism.",
      "550" = "Requested action not taken.  File unavailable",
      "551" = "Requested action aborted: page type unknown.",
      "552" = "Requested file action aborted.  Exceeded storage allocation (for current directory or dataset).",
      "553" = "Requested action not taken.  File name not allowed.",
      "631" = "Integrity protected reply.",
      "632" = "Confidentiality and integrity protected reply.",
      "633" = "Confidentiality protected reply."
      )

check_url_db <-
function(db, verbose = FALSE)
{
    .gather <- function(u = character(),
                        p = list(),
                        s = rep.int("", length(u)),
                        m = rep.int("", length(u)),
                        cran = rep.int("", length(u)),
                        spaces = rep.int("", length(u))) {
        y <- data.frame(URL = u, From = I(p), Status = s, Message = m,
                        CRAN = cran, Spaces = spaces,
                        stringsAsFactors = FALSE)
        y$From <- p
        class(y) <- c("check_url_db", "data.frame")
        y
    }

    .fetch <- function(u) {
        if(verbose) message(sprintf("processing %s", u))
        h <- tryCatch(curlGetHeaders(u), error = identity)
        if(inherits(h, "error")) {
            msg <- conditionMessage(h)
            if (grepl("libcurl error code (51|60)", msg)) {
                h2 <- tryCatch(curlGetHeaders(u, verify = FALSE),
                               error = identity)
                attr(h, "no-verify") <- h2
            }
        }
        h
    }

    .check_ftp <- function(u) {
        h <- .fetch(u)
        if(inherits(h, "error")) {
            s <- "-1"
            msg <- sub("[[:space:]]*$", "", conditionMessage(h))
        } else {
            s <- as.character(attr(h, "status"))
            msg <- table_of_FTP_server_return_codes[s]
        }
        c(s, msg, "", "")
    }

    .check_http <- function(u) {
        h <- .fetch(u)
        newLoc <- ""
        if(inherits(h, "error")) {
            s <- "-1"
            msg <- sub("[[:space:]]*$", "", conditionMessage(h))
            if (!is.null(v <- attr(h, "no-verify"))) {
                s2 <- as.character(attr(v, "status"))
                msg <- paste0(msg, "\n\t(Status without verification: ",
                              table_of_HTTP_status_codes[s2], ")")
            }
        } else {
            s <- as.character(attr(h, "status"))
            msg <- table_of_HTTP_status_codes[s]
        }
        ## Look for redirected URLs
        if (any(grepl("301 Moved Permanently", h, useBytes = TRUE))) {
            ind <- grep("^[Ll]ocation: ", h, useBytes = TRUE)
            if (length(ind))
                newLoc <- sub("^[Ll]ocation: ([^\r]*)\r\n", "\\1", h[max(ind)])
        }
        ## A mis-configured site
        if (s == "503" && any(grepl("www.sciencedirect.com", c(u, newLoc))))
            s <- "405"
        cran <- grepl("https?://cran.r-project.org/web/packages/[.[:alnum:]]+(|/|/index.html)$",
                      u, ignore.case = TRUE)
        spaces <- grepl(" ", u)
        c(s, msg, newLoc, if(cran) u else "", if(spaces) u else "")
    }

    bad <- .gather()

    if(!NROW(db)) return(bad)

    parents <- split(db$Parent, db$URL)
    urls <- names(parents)
    parts <- parse_URL_reference(urls)

    ## Empty URLs.
    ind <- apply(parts == "", 1L, all)
    if(any(ind)) {
        len <- sum(ind)
        bad <- rbind(bad,
                     .gather(urls[ind],
                             parents[ind],
                             m = rep.int("Empty URL", len)))
    }

    ## Invalid URI schemes.
    schemes <- parts[, 1L]
    ind <- is.na(match(schemes,
                       c("",
                         IANA_URI_scheme_db$URI_Scheme,
                         ## Also allow 'javascript' scheme, see
                         ## <https://tools.ietf.org/html/draft-hoehrmann-javascript-scheme-03>
                         ## (but apparently never registered with IANA).
                         "javascript")))
    if(any(ind)) {
        len <- sum(ind)
        msg <- rep.int("Invalid URI scheme", len)
        doi <- schemes[ind] == "doi"
        if(any(doi))
            msg[doi] <- paste(msg[doi], "(use \\doi for DOIs)")
        bad <- rbind(bad,
                     .gather(urls[ind], parents[ind], m = msg))
    }

    ## ftp.
    pos <- which(schemes == "ftp")
    if(length(pos)) {
        results <- do.call(rbind, lapply(urls[pos], .check_ftp))
        status <- as.numeric(results[, 1L])
        ind <- (status < 0L) | (status >= 400L)
        if(any(ind)) {
            pos <- pos[ind]
            s <- as.character(status[ind])
            s[s == "-1"] <- "Error"
            m <- results[ind, 2L]
            m[is.na(m)] <- ""
            bad <- rbind(bad,
                         .gather(urls[pos], parents[pos], s, m))
        }
    }

    ## http/https.
    pos <- which(schemes == "http" | schemes == "https")
    if(length(pos)) {
        results <- do.call(rbind, lapply(urls[pos], .check_http))
        status <- as.numeric(results[, 1L])
        ## 405 is HTTP not allowing HEAD requests
        ## maybe also skip 500, 503, 504 as likely to be temporary issues
        ind <- !(status %in% c(200L, 405L)) |
            nzchar(results[, 4L]) | nzchar(results[, 5L])
        if(any(ind)) {
            pos <- pos[ind]
            s <- as.character(status[ind])
            s[s == "-1"] <- "Error"
            m <- results[ind, 2L]
            m[is.na(m)] <- ""
            url <- urls[pos]; newLoc <- results[ind, 3L]
            ind2 <- nzchar(newLoc)
            url[ind2] <-
                paste0(url[ind2], " (moved to ", newLoc[ind2], ")")
            bad <- rbind(bad,
                         .gather(url, parents[pos], s, m,
                                 results[ind, 4L], results[ind, 5L]))
        }
    }
    bad
}

format.check_url_db <-
function(x, ...)
{
    if(!NROW(x)) return(character())

    paste0(sprintf("URL: %s", x$URL),
           sprintf("\nFrom: %s",
                   sapply(x$From, paste, collapse = "\n      ")),
           ifelse((s <- x$Status) == "",
                  "",
                  sprintf("\nStatus: %s", s)),
           ifelse((m <- x$Message) == "",
                  "",
                  sprintf("\nMessage: %s", m)),
           ifelse((m <- x$Spaces) == "",
                  "",
                  "\nURL contains spaces"),
           ifelse((m <- x$CRAN) == "",
                  "",
                  "\nCRAN URL not in canonical form")
           )
}

print.check_url_db <-
function(x, ...)
{
    if(NROW(x))
        writeLines(paste(format(x), collapse = "\n\n"))
    invisible(x)
}
