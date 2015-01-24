#  File src/library/tools/R/urltools.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

get_IANA_URI_scheme_db <-
function()
{
    ## See <http://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml>.
    baseurl <- "http://www.iana.org/assignments/uri-schemes/"
    permanent <- read.csv(url(paste0(baseurl, "uri-schemes-1.csv")),
                          stringsAsFactors = FALSE)
    provisional <- read.csv(url(paste0(baseurl, "uri-schemes-2.csv")),
                            stringsAsFactors = FALSE)
    historical <- read.csv(url(paste0(baseurl, "uri-schemes-3.csv")),
                           stringsAsFactors = FALSE)
    db <- rbind(permanent, provisional, historical)
    db$Category <-
        rep.int(c("permanent", "provisional", "historical"),
                c(nrow(permanent),
                  nrow(provisional),
                  nrow(historical)))
    names(db) <- chartr(".", "_", names(db))
    db
}

## Ideally this would be in sysdata.rda.
## For now, re-create using
##   dput(get_IANA_URI_scheme_db()$URI_Scheme)
## if necessary ...
IANA_URI_schemes <-
c("aaa", "aaas", "about", "acap", "acct", "cap", "cid", "coap",
"coaps", "crid", "data", "dav", "dict", "dns", "file", "ftp",
"geo", "go", "gopher", "h323", "http", "https", "iax", "icap",
"im", "imap", "info", "ipp", "ipps", "iris", "iris.beep", "iris.xpc",
"iris.xpcs", "iris.lwz", "jabber", "ldap", "mailto", "mid", "msrp",
"msrps", "mtqp", "mupdate", "news", "nfs", "ni", "nih", "nntp",
"opaquelocktoken", "pop", "pres", "reload", "rtsp", "rtsps",
"rtspu", "service", "session", "shttp", "sieve", "sip", "sips",
"sms", "snmp", "soap.beep", "soap.beeps", "stun", "stuns", "tag",
"tel", "telnet", "tftp", "thismessage", "tn3270", "tip", "turn",
"turns", "tv", "urn", "vemmi", "ws", "wss", "xcon", "xcon-userid",
"xmlrpc.beep", "xmlrpc.beeps", "xmpp", "z39.50r", "z39.50s",
"acr", "adiumxtra", "afp", "afs", "aim", "apt", "attachment",
"aw", "barion", "beshare", "bitcoin", "bolo", "callto", "chrome",
"chrome-extension", "com-eventbrite-attendee", "content", "cvs",
"dlna-playsingle", "dlna-playcontainer", "dtn", "dvb", "ed2k",
"facetime", "feed", "feedready", "finger", "fish", "gg", "git",
"gizmoproject", "gtalk", "ham", "hcp", "icon", "ipn", "irc",
"irc6", "ircs", "itms", "jar", "jms", "keyparc", "lastfm", "ldaps",
"magnet", "maps", "market", "message", "mms", "ms-help", "ms-settings-power",
"msnim", "mumble", "mvn", "notes", "oid", "palm", "paparazzi",
"pkcs11", "platform", "proxy", "psyc", "query", "res", "resource",
"rmi", "rsync", "rtmfp", "rtmp", "secondlife", "sftp", "sgn",
"skype", "smb", "smtp", "soldat", "spotify", "ssh", "steam",
"submit", "svn", "teamspeak", "teliaeid", "things", "udp", "unreal",
"ut2004", "ventrilo", "view-source", "webcal", "wtai", "wyciwyg",
"xfire", "xri", "ymsgr", "fax", "mailserver", "modem", "pack",
"prospero", "snews", "videotex", "wais", "z39.50")

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

.Rd_get_urls <-
function(x)
{
    urls <- character()
    recurse <- function(e) {
        tag <- attr(e, "Rd_tag")
        if(identical(tag, "\\url")) {
            urls <<-
                c(urls,
                  if(length(e))
                      .strip_whitespace(as.character(e[[1L]]))
                  else "")
            ## There could be \url{} ...
        }
        if(is.list(e)) lapply(e, recurse)
    }
    lapply(x, recurse)
    unique(urls)
}

url_db <-
function(urls, parents)
{
    db <- data.frame(URL = urls, Parent = parents,
                     stringsAsFactors = FALSE)
    class(db) <- c("url_db", "data.frame")
    db
}

url_db_from_package_Rd_db <-
function(db)
{
    urls <- Filter(length, lapply(db, .Rd_get_urls))
    url_db(unlist(urls, use.names = FALSE),
           rep.int(file.path("man", names(urls)),
                   sapply(urls, length)))
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

url_db_from_package_sources <-
function(dir, add = FALSE) {
    meta <- .read_description(file.path(dir, "DESCRIPTION"))
    db <- rbind(url_db_from_package_metadata(meta),
                url_db_from_package_Rd_db(Rd_db(dir = dir)))
    if(add)
        db$Parent <- file.path(basename(dir), db$Parent)
    db
}

url_db_from_installed_packages <-
function(packages, lib.loc = NULL)
{
    if(!length(packages)) return()
    verbose <- interactive()
    one <- function(p) {
        if(verbose)
            message(sprintf("processing %s", p))
        dir <- system.file(package = p, lib.loc = lib.loc)
        if(dir == "") return()
        meta <- .read_description(file.path(dir, "DESCRIPTION"))
        rddb <- Rd_db(p, lib.loc = dirname(dir))
        db <- rbind(url_db_from_package_metadata(meta),
                    url_db_from_package_Rd_db(rddb))
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
    db <- read.csv(url(paste0(baseurl, "http-status-codes-1.csv")),
                   stringsAsFactors = FALSE)
    ## Drop "Unassigned".
    db[db$Description != "Unassigned", ]
}

## Ideally this would be in sysdata.rda.
## For now, use something based on
##   IANA_HTTP_status_code_db <- get_IANA_HTTP_status_code_db()
##   writeLines(sprintf("      \"%s\" = \"%s\",",
##                      IANA_HTTP_status_code_db$Value,
##                      IANA_HTTP_status_code_db$Description))
## See <http://en.wikipedia.org/wiki/List_of_HTTP_status_codes>.
table_of_HTTP_status_codes <-
    c("100" = "Continue",
      "101" = "Switching Protocols",
      "102" = "Processing",
      "200" = "OK",
      "201" = "Created",
      "202" = "Accepted",
      "203" = "Non-Authoritative Information",
      "204" = "No Content",
      "205" = "Reset Content",
      "206" = "Partial Content",
      "207" = "Multi-Status",
      "208" = "Already Reported",
      "226" = "IM Used",
      "300" = "Multiple Choices",
      "301" = "Moved Permanently",
      "302" = "Found",
      "303" = "See Other",
      "304" = "Not Modified",
      "305" = "Use Proxy",
      "306" = "(Unused)",
      "307" = "Temporary Redirect",
      "308" = "Permanent Redirect",
      "400" = "Bad Request",
      "401" = "Unauthorized",
      "402" = "Payment Required",
      "403" = "Forbidden",
      "404" = "Not Found",
      "405" = "Method Not Allowed",
      "406" = "Not Acceptable",
      "407" = "Proxy Authentication Required",
      "408" = "Request Timeout",
      "409" = "Conflict",
      "410" = "Gone",
      "411" = "Length Required",
      "412" = "Precondition Failed",
      "413" = "Payload Too Large",
      "414" = "URI Too Long",
      "415" = "Unsupported Media Type",
      "416" = "Range Not Satisfiable",
      "417" = "Expectation Failed",
      "422" = "Unprocessable Entity",
      "423" = "Locked",
      "424" = "Failed Dependency",
      "426" = "Upgrade Required",
      "428" = "Precondition Required",
      "429" = "Too Many Requests",
      "431" = "Request Header Fields Too Large",
      "500" = "Internal Server Error",
      "501" = "Not Implemented",
      "502" = "Bad Gateway",
      "503" = "Service Unavailable",
      "504" = "Gateway Timeout",
      "505" = "HTTP Version Not Supported",
      "506" = "Variant Also Negotiates",
      "507" = "Insufficient Storage",
      "508" = "Loop Detected",
      "510" = "Not Extended",
      "511" = "Network Authentication Required")

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
                        s = character(),
                        m = character()) {
        y <- data.frame(URL = u, From = I(p), Status = s, Message = m,
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
        c(s, msg)
    }

    .check_http <- function(u) {
        h <- .fetch(u)
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
        c(s, msg)
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
                             rep.int("", len),
                             rep.int("Empty URL", len)))
    }

    ## Invalid URI schemes.
    schemes <- parts[, 1L]
    ind <- is.na(match(schemes, c("", IANA_URI_schemes)))
    if(any(ind)) {
        len <- sum(ind)
        bad <- rbind(bad,
                     .gather(urls[ind],
                             parents[ind],
                             rep.int("", len),
                             rep.int("Invalid URI scheme", len)))
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
            bad <- rbind(bad, .gather(urls[pos], parents[pos], s, m))
        }
    }

    ## http/https.
    pos <- which(schemes == "http" | schemes == "https")
    if(length(pos)) {
        results <- do.call(rbind, lapply(urls[pos], .check_http))
        status <- as.numeric(results[, 1L])
        ## 405 is HTTP not allowing HEAD requests
        ## maybe also skip 500, 503, 504 as likely to be temporary issues
        ind <- !(status %in% c (200L, 405L))
        if(any(ind)) {
            pos <- pos[ind]
            s <- as.character(status[ind])
            s[s == "-1"] <- "Error"
            m <- results[ind, 2L]
            m[is.na(m)] <- ""
            bad <- rbind(bad, .gather(urls[pos], parents[pos], s, m))
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
                  sprintf("\nMessage: %s", m)))
}

print.check_url_db <-
function(x, ...)
{
    if(NROW(x))
        writeLines(paste(format(x), collapse = "\n\n"))
    invisible(x)
}
