#  File src/library/tools/R/sign.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2026 The R Core Team
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

## This is a very limited interface to gpg. gpg is a real mess, so
## we keep it to an absolute minimum to verify signatures and possibly
## sign.
.gpg.run <- function(gpg, ..., stderr=TRUE) {
    args <- as.character(unlist(list(...)))
    e <- tryCatch(system2(gpg, sapply(args, shQuote), TRUE, stderr),
                  error=function(e) e)
}

.gpg <- function(min.ver, quiet=FALSE) {
    gpg <- Sys.getenv("GPG")
    if (!nzchar(gpg)) gpg <- "gpg"
    e <- if (quiet) suppressWarnings(.gpg.run(gpg, "--version", stderr=FALSE)) else .gpg.run(gpg, "--version", stderr=FALSE)
    if (!inherits(e, "error")) {
        if (missing(min.ver)) return(gpg)
        if (length(e) && length(grep("^gpg.* [0-9.]+$", e[1L]))) {
            ver <- package_version(gsub("^gpg.* ([0-9.]+)$", "\\1", e[1L]), FALSE)
            min.ver <- package_version(min.ver)
            if (!any(is.na(ver)) && ver < min.ver) {
                if (!quiet)
                    warning(sprintf("'%s' is too old: need version %s, found %s",
                                    gpg, min.ver, ver), domain = NA)
                return(FALSE)
            }
        } else {
            if (!quiet) warning("Cannot determine version of gpg (", sQuote(gpg), "), won't use.", domain = NA)
            return(FALSE)
        }
    } else {
        if (!quiet) warning("gpg (", sQuote(gpg),") is not present", domain = NA)
        return(FALSE)
    }
    gpg
}

.gpg.sign <- function(doc, out, uid, keyring, use.default.keyring=missing(keyring), overwrite=TRUE, quiet=FALSE) {
    gpg <- .gpg(quiet=quiet)
    if (isFALSE(gpg)) stop("'gpg' is not available for signing")
    doc <- normalizePath(doc, "/", TRUE)
    out <- path.expand(out)
    if (file.exists(out)) {
        if (overwrite)
            unlink(out)
        else
            stop("Target file for the signature already exists")
    }
    if (!missing(keyring))
        keyring <- normalizePath(keyring, "/", TRUE)
    args <- c("--status-fd", "1")
    if (!missing(uid)) {
        if (!is.character(uid) || length(uid) != 1L)
            stop(gettextf("'%s' must be a character string", "uid"), domain = NA)
        args <- c(args, "-u", uid)
    }
    if (isFALSE(use.default.keyring)) {
        if (missing(keyring))
            stop("'keyring' must be specified if 'use.default.keyring=FALSE'")
        args <- c(args, "--no-default-keyring")
    }
    if (!missing(keyring)) args <- c(args, sapply(keyring, function(x) c("--keyring", x)))
    if (!use.default.keyring) {
        tf <- tempfile()
        dh <- file.path(paste0(tf, ".d"))
        dir.create(dh, mode="0700")
        args <- c(args, "--homedir", dh)
        on.exit(unlink(dh, TRUE, TRUE))
    }
    args <- c(args, "-o", out, "--detach-sign", doc)
    res <- if (quiet) suppressWarnings(.gpg.run(gpg, args)) else .gpg.run(gpg, args)
    if (isFALSE(res))
        stop("Failed to invoke 'gpg' for signing")
    .g <- function(x, pat) if (length(v <- grep(pat, x, value=TRUE))) regmatches(v, regexec(pat, v))[[1]][-1] else NULL
    sfp <- .g(res, "^\\[GNUPG:\\] KEY_CONSIDERED ([0-9a-fA-F]+) ")
    rst <- if (is.numeric(attr(res, "status"))) attr(res, "status") else 0L
    sfe <- .g(res, "gpg: signing failed: (.*)")
    if (!quiet) {
        if (length(grep("^\\[GNUPG:\\] PINENTRY_LAUNCHED", res)) && rst != 0L)
            stop(gettextf("Cannot sign due to failed pin entry for key %s", sfp %||% "??"),
                 if (length(sfe)) paste0(": ", sfe) else "", domain = NA)
        if (length(sfe))
            stop("Signing failed: ", sfe)
    }
    isTRUE(file.exists(out)) && (rst == 0L)
}

.gpg.verify <- function(doc, sig, keyring, use.default.keyring=missing(keyring), quiet=FALSE) {
    gpg <- .gpg(quiet=quiet)
    if (isFALSE(gpg)) {
        if (quiet)
            return(NA)
        stop("'gpg' is not available for signing")
    }
    doc <- normalizePath(doc, "/", TRUE)
    sig <- normalizePath(sig, "/", TRUE)
    if (!missing(keyring))
        keyring <- normalizePath(keyring, "/", TRUE)
    args <- c("--status-fd", "1")
    if (isFALSE(use.default.keyring)) {
        if (missing(keyring))
            stop("'keyring' must be specified if 'use.default.keyring=FALSE'")
        args <- c(args, "--no-default-keyring")
    }
    if (!missing(keyring)) args <- c(args, sapply(keyring, function(x) c("--keyring", x)))
    if (!use.default.keyring) {
        tf <- tempfile()
        dh <- file.path(paste0(tf, ".d"))
        dir.create(dh, mode="0700")
        args <- c(args, "--homedir", dh)
        on.exit(unlink(dh, TRUE, TRUE))
    }
    args <- c(args, "--verify", sig, doc)
    res <- if (quiet) suppressWarnings(.gpg.run(gpg, args)) else .gpg.run(gpg, args)
    .g <- function(x, pat) if (length(v <- grep(pat, x, value=TRUE))) regmatches(v, regexec(pat, v))[[1]][-1] else NULL
    vs   <- .g(res, "^\\[GNUPG:\\] VALIDSIG ([0-9a-fA-F]+) [0-9-]+ ([0-9]+) ")
    uid  <- .g(res, "^\\[GNUPG:\\] GOODSIG [0-9a-fA-F]+ (.*)")
    kid  <- .g(res, "^\\[GNUPG:\\] GOODSIG ([0-9a-fA-F]+) ")
    sfp  <- .g(res, "^\\[GNUPG:\\] KEY_CONSIDERED ([0-9a-fA-F]+) ")
    errs <- .g(res, "^\\[GNUPG:\\] ERRSIG ([0-9a-fA-F]+) [0-9]+ [0-9]+ [0-9]+ ([0-9]+) [0-9]+ ([0-9a-fA-F]+)")
    nok  <- .g(res, "^\\[GNUPG:\\] NO_PUBKEY ([0-9a-fA-F]+)")
    vr <- if (length(kid))
        list(valid=TRUE, fingerprint=vs[1], keyid=kid, userid=uid, ts=.POSIXct(as.numeric(vs[2])), missing.pubkey=FALSE)
    else
        list(valid=FALSE, fingerprint=if (length(errs) > 2) errs[3] else sfp,
             keyid=if(length(errs)) errs[1] else NULL,
             ts=if(length(errs)) .POSIXct(as.numeric(errs[2])) else NULL,
             missing.pubkey = if (length(nok)) TRUE else NULL, gpg.output=res)
    vr
}

## locations of R keyrings: $R_HOME/etc/keyrings and ~/.R/keyrings (is that a good idea?)
.R.keyrings <- function() {
    etc <- R.home("etc")
    c(Sys.glob(file.path(etc, "keyrings", "*.gpg")), Sys.glob(file.path("~",".R","keyrings","*.gpg")))
}

## This function silent, returns NA if gpg is not present (or too old).
## It uses R keyrings first and then falls back to the user's gpg keyring.
## Additonal details (such as key fingerprint and user ID if available)
## are returned in the "result" attribute.
verify.signature <- function(doc, sig) {
    keyrings <- .R.keyrings()
    if (length(keyrings)) {
        res <- .gpg.verify(doc, sig, keyring=keyrings, quiet=TRUE)
        if (is.list(res) && isTRUE(res$valid)) return(structure(TRUE, result=res, source="R"))
    }
    res <- .gpg.verify(doc, sig, quiet=TRUE)
    if (is.list(res) && is.logical(res$valid)) structure(res$valid, result=res, source="default") else res
}

verifySHA256signature <- function(package, dir)
{
    if(missing(dir)) dir <- find.package(package, quiet = TRUE)
    if(length(dir) != 1L) return(NA)
    res <- checkSHA256sums(package, dir)
    if (!isTRUE(res)) return(res)
    sha256sig <- file.path(dir, "SHA256.sig")
    if(!file.exists(sha256sig)) return(NA)
    verify.signature(file.path(dir, "SHA256"), sha256sig)
}

create.signature <- function(doc, sig, uid, keyring)
    .gpg.sign(doc, sig, uid, keyring)
