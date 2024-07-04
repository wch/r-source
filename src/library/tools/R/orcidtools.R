#  File src/library/tools/R/orcidtools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2024 The R Core Team
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

### ** .ORCID_iD_regexp

.ORCID_iD_regexp <-
    "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[X0-9]"

### ** .ORCID_iD_variants_regexp

.ORCID_iD_variants_regexp <-
    sprintf("^<?((https?://|)orcid.org/)?(%s)>?$", .ORCID_iD_regexp)

### ** .ORCID_iD_canonicalize

.ORCID_iD_canonicalize <- function(x)
    sub(.ORCID_iD_variants_regexp, "\\3", x)

### ** .ORCID_iD_is_valid

.ORCID_iD_is_valid <- function(x) {
    one <- function(s) {
        if(!grepl(.ORCID_iD_variants_regexp, s))
            return(FALSE)
        s <- .ORCID_iD_canonicalize(s)
        ## Checksum test, see
        ## <https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier>
        s <- strsplit(gsub("-", "", s, fixed = TRUE), "")[[1L]]
        x <- as.numeric(s[-16L])
        t <- sum(x * 2 ^ (15L : 1L))
        rem <- t %% 11
        res <- (12 - rem) %% 11
        z <- if(res == 10) "X" else as.character(res)
        z == s[16L]
    }
    vapply(x, one, NA)
}

### ** .ORCID_iD_is_alive

.ORCID_iD_is_alive <- function(x) {
    ## See
    ## <https://info.orcid.org/documentation/api-tutorials/api-tutorial-read-data-on-a-record/#h-a-note-on-non-existent-orcids>
    ## Assume all given ids are canonical.
    urls <- sprintf("https://orcid.org/%s", x)
    hdrs <- list("Accept" = "application/xml")
    resp <- .curl_multi_run_worker(urls, nobody = TRUE, hdrs = hdrs)
    vapply(resp, .curl_response_status_code, 0L) == 200L
}

### ** .ORCID_iD_from_person

.ORCID_iD_from_person <- function(x)
    vapply(unclass(x),
           function(e) e$comment["ORCID"] %||% NA_character_,
           "")

### ** .ORCID_iD_db_from_package_sources

.ORCID_iD_db_from_package_sources <-
function(dir, add = FALSE)
{
    ids1 <- .ORCID_iD_from_person(.persons_from_metadata(dir))
    ids1 <- ids1[!is.na(ids1)]
    ids2 <- .ORCID_iD_from_person(.persons_from_citation(dir))
    ids2 <- ids2[!is.na(ids2)]
    db  <- data.frame(ID = c(character(), ids1, ids2),
                      Parent = c(rep_len("DESCRIPTION",
                                         length(ids1)),
                                 rep_len("inst/CITATION",
                                         length(ids2))))
    if(add)
        db$Parent <- file.path(basename(dir), db$Parent)
    db
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
