#  File src/library/tools/R/rortools.R
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

## See <https://ror.org/about/faqs/#what-is-a-ror-identifier>:
##   A ROR ID consists of a unique 9-character string appended to the
##   ROR domain. The preferred form of a ROR identifier is the entire
##   URL: https://ror.org/02mhbdp94 rather than ror.org/02mhbdp94 or
##   02mhbdp94, although the ROR API will recognize all three of these
##   forms as ROR IDs. 
## So apparently the unique 9-character string is not the ROR ID and has 
## no name to refer to ... argh.  For now, name things as we do for
## ORCID iDs.

### ** .ROR_ID_regexp

.ROR_ID_regexp <-
    ".{9}"

### ** .ROR_ID_variants_regexp

.ROR_ID_variants_regexp <-
    sprintf("^<?((https://|)ror.org/)?(%s)>?$", .ROR_ID_regexp)

### ** .ROR_ID_canonicalize

.ROR_ID_canonicalize <- function(x)
    sub(.ROR_ID_variants_regexp, "\\3", x)

### ** .ROR_ID_is_valid

.ROR_ID_is_valid <- function(x)
    grepl(.ROR_ID_variants_regexp, x)

### ** .ROR_ID_is_alive

.ROR_ID_is_alive <- function(x) {
    ## For now use HEAD requests for the full ROR ID.
    ## See <https://ror.readme.io/v2/docs/rest-api> for getting more
    ## information.
    ## Assume all given ids are canonical.
    urls <- sprintf("https://ror.org/%s", x)
    resp <- .curl_multi_run_worker(urls, nobody = TRUE)
    vapply(resp, .curl_response_status_code, 0L) == 200L
}

### ** ROR_ID_from_person

.ROR_ID_from_person <- function(x)
    vapply(unclass(x),
           function(e) e$comment["ROR"] %||% NA_character_,
           "")

### ** .ROR_ID_db_from_package_sources

.ROR_ID_db_from_package_sources <-
function(dir, add = FALSE)
{
    ids1 <- .ROR_ID_from_person(.persons_from_metadata(dir))
    ids1 <- ids1[!is.na(ids1)]
    ids2 <- .ROR_ID_from_person(.persons_from_citation(dir))
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
