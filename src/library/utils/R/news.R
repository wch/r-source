#  File src/library/utils/R/news.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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


news <-
function(query, package = "R", lib.loc = NULL,
         format = NULL, reader = NULL, db = NULL)
{
    if(is.null(db)) {
        db <- if(package == "R")
            tools:::.build_news_db_from_R_NEWS_Rd()
        else
            tools:::.build_news_db(package, lib.loc, format, reader)
    }
    if(is.null(db))
        return(NULL)

    attr(db, "package") <- package

    ## Is there a way to directly call/use subset.data.frame?
    ## E.g.,
    ##   subset(db, query)
    ## does not work.
    if(missing(query))
        return(db)

    ## For queries we really need to force Version to package_version
    ## and Date to Date ...
    ## This is tricky because we do not necessarily have valid package
    ## versions (e.g., R NEWS has "2.8.1 patched") or could have the
    ## version info missing (and package_version() does not like NAs).

    has_bad_attr <-
        !is.null(bad <- attr(db, "bad")) && (length(bad) == NROW(db))

    ## Manipulate fields for querying (but return the original ones).
    db1 <- db
    ## Canonicalize version entries which *start* with a valid numeric
    ## version, i.e., drop things like " patched".
    version <- db$Version
    pos <- regexpr(sprintf("^%s",
                           .standard_regexps()$valid_numeric_version),
                   version)
    if(any(ind <- (pos > -1L)))
        version[ind] <-
            substring(version[ind], 1L, attr(pos, "match.length")[ind])
    db1$Version <- numeric_version(version, strict = FALSE)
    db1$Date <- as.Date(db$Date)

    r <- eval(substitute(query), db1, parent.frame())
    ## Do something if this is not logical ...
    if(is.null(r))
        return(db)
    else if(!is.logical(r) || length(r) != length(version))
        stop("invalid query")
    r <- r & !is.na(r)
    result <- db[r, ]
    if (any(!r))
    	result <- structure(result, subset = r)
    if(has_bad_attr)
        result <- structure(result, bad = bad[r])
    result
}

format.news_db <-
function(x, ...)
{
    if(inherits(x, "news_db_from_Rd") ||
       (!(is.null(bad <- attr(x, "bad")))
        && (length(bad) == NROW(x))
        && all(!bad))) {

        ## Format news in the preferred input format:
        ##   Changes in $VERSION [($DATE)]:
        ##   [$CATEGORY$]
        ##   indented/formatted bullet list of $TEXT entries.
        ## <FIXME>
        ## Add support for DATE.
        ## </FIXME>

        vchunks <- split(x, x$Version)
        ## Re-order according to decreasing version.
        ## R NEWS has invalid "versions" such as "R-devel" and
        ## "2.4.1 patched".  We can remap the latter (to e.g. 2.4.1.1)
        ## and need to ensure the former come first.
        vstrings <- names(vchunks)
        ind <- vstrings != "R-devel"
        pos <- c(which(!ind),
                 which(ind)[order(as.numeric_version(sub(" *patched", ".1",
                                                         vstrings[ind])),
                                  decreasing = TRUE)])
        vchunks <- vchunks[pos]
	if(length(vchunks)) {
            dates <- sapply(vchunks, function(v) v$Date[1L])
            vstrings <- names(vchunks)
            ind <- vstrings != "R-devel"
            vstrings[ind] <- sprintf("version %s", vstrings[ind])
            vheaders <-
                sprintf("Changes in %s%s:",
                        vstrings,
                        ifelse(is.na(dates), "",
                               sprintf(" (%s)", dates)))
        } else vheaders <- character()

        format_items <- function(x)
            paste0("    o   ", gsub("\n", "\n\t", x$Text))
        format_vchunk <- function(vchunk) {
            if(all(!is.na(category <- vchunk$Category)
                   & nzchar(category))) {
                ## need to preserve order of headings.
                cchunks <-
                    split(vchunk,
                          factor(category, levels = unique(category)))
                Map(c, names(cchunks), lapply(cchunks, format_items),
                    USE.NAMES = FALSE)
            } else {
                format_items(vchunk)
            }
        }

        Map(c, vheaders, lapply(vchunks, format_vchunk),
            USE.NAMES = FALSE)
    } else {
        ## Simple and ugly.
        ## Drop all-NA variables.
        apply(as.matrix(x),
              1L,
              function(e)
              paste(formatDL(e[!is.na(e)], style = "list"),
                    collapse = "\n"))
    }
}

print.news_db <-
function(x, ...)
{
    port <- tools::startDynamicHelp(NA)
    if (port > 0L) {
    	if ((pkg <- attr(x, "package")) == "R")
    	    url <- sprintf("http://127.0.0.1:%d/doc/html/NEWS.html", port)
    	else
    	    url <- sprintf("http://127.0.0.1:%d/library/%s/NEWS", port, pkg)
    	if (!is.null(subset <- attr(x, "subset"))) {
	    # Subsets are typically ranges of dates or version numbers, so we run-length encode
	    # the subset vector.  We put TRUE in front so the values alternate TRUE, FALSE, ... .
    	    rle <- paste(rle(c(TRUE, subset))$lengths, collapse="_")
    	    url <- paste0(url, "?subset=", rle)
    	}
    	browseURL(url)
    } else 
	writeLines(paste(unlist(format(x, ...)), collapse = "\n\n"))
    invisible(x)
}
