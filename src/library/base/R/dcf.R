#  File src/library/base/R/dcf.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

read.dcf <-
function(file, fields = NULL, all = FALSE, keep.white = NULL)
{
    if(is.character(file)){
        file <- gzfile(file)
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    ## For historical reasons, the default is not to accumulate repeated
    ## fields in a record (in fact picking the *last* field occurrence).
    ## Use the internal code for performance reasons, but note that we
    ## could of course as well use
    ##   do.call("cbind",
    ##           lapply(out,
    ##                  function(s)
    ##                  if(is.atomic(s)) s
    ##                  else mapply("[[", s, lengths(s))))
    if(!all) return(.Internal(readDCF(file, fields, keep.white)))

    .assemble_things_into_a_data_frame <- function(tags, vals, nums) {
        tf <- factor(tags, levels = unique(tags))

        cnts <- table(nums, tf)
        out <- array(NA_character_, dim = dim(cnts),
                     dimnames = list(NULL, levels(tf)))
        if(all(cnts <= 1L)) {
            ## No repeated tags ...
            out[cbind(nums, tf)] <- vals
            out <- as.data.frame(out, stringsAsFactors = FALSE)
        }
        else {
            levs <- colSums(cnts > 1L) == 0L
            if(any(levs)) {
                inds <- tf %in% levels(tf)[levs]
                out[cbind(nums[inds], tf[inds])] <- vals[inds]
            }
            out <- as.data.frame(out, stringsAsFactors = FALSE)
            for(l in levels(tf)[!levs]) {
                out[[l]] <- rep.int(list(NA_character_), nrow(cnts))
                i <- tf == l
                out[[l]][unique(nums[i])] <- split(vals[i], nums[i])
            }
        }

        out
    }

    ## <FIXME>
    ## This should no longer be necessary?
    ## <COMMENT>
    ## This needs to be done in an 8-bit locale for the regexps.
    ## ctype <-  Sys.getlocale("LC_CTYPE")
    ## on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
    ## Sys.setlocale("LC_CTYPE", "C")
    ## </COMMENT>
    ## </FIXME>

    lines <- readLines(file, skipNul = TRUE)

    ## Try to find out about invalid things: mostly, lines which do not
    ## start with blanks but have no ':' ...
    ind <- grep("^[^[:blank:]][^:]*$", lines, perl = TRUE)
    if(length(ind)) {
        lines <- substr(lines[ind], 1L, 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nRegular lines must have a tag.\nOffending lines start with:\n%s",
                      paste0("  ", lines, collapse = "\n")),
             domain = NA)
    }

    line_is_not_empty <- !grepl("^[[:space:]]*$", lines, perl = TRUE)
    nums <- cumsum(diff(c(FALSE, line_is_not_empty) > 0L) > 0L)
    ## Remove the empty ones so that nums knows which record each line
    ## belongs to.
    nums <- nums[line_is_not_empty]
    lines <- lines[line_is_not_empty]

    ## Deal with escaped blank lines (used by Debian at least for the
    ## Description: values, see man 5 deb-control):
    line_is_escaped_blank <-
        grepl("^[[:space:]]+\\.[[:space:]]*$", lines, perl = TRUE)
    if(any(line_is_escaped_blank))
        lines[line_is_escaped_blank] <- ""

    line_has_tag <- grepl("^[^[:blank:]][^:]*:", lines, perl = TRUE)

    ## Check that records start with tag lines.
    pos <- c(1L, which(diff(nums) > 0L) + 1L)
    ind <- !line_has_tag[pos]
    if(any(ind)) {
        lines <- substr(lines[pos[ind]], 1L, 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nContinuation lines must not start a record.\nOffending lines start with:\n%s",
                      paste0("  ", lines, collapse = "\n")),
             domain = NA)
    }

    lengths <- rle(cumsum(line_has_tag))$lengths
    ## End positions of field entries.
    pos <- cumsum(lengths)

    tags <- sub(":.*", "", lines[line_has_tag], perl = TRUE)
    lines[line_has_tag] <-
        sub("[^:]*:[[:space:]]*", "", lines[line_has_tag], perl = TRUE)
    fold <- is.na(match(tags, keep.white))
    foldable <- rep.int(fold, lengths)
    lines[foldable] <-
        sub("^[[:space:]]*", "", lines[foldable], perl = TRUE)
    lines[foldable] <-
        sub("[[:space:]]*$", "", lines[foldable], perl = TRUE)

    vals <- mapply(function(from, to) paste(lines[from:to],
                                            collapse = "\n"),
                   c(1L, pos[-length(pos)] + 1L), pos)
    vals[fold] <- trimws(vals[fold])

    out <- .assemble_things_into_a_data_frame(tags, vals, nums[pos])

    if(!is.null(fields))
        out <- out[fields]

    out
}

write.dcf <-
function(x, file = "", append = FALSE, useBytes = FALSE,
         indent = 0.1 * getOption("width"),
         width = 0.9 * getOption("width"),
         keep.white = NULL)
{
    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, if(append) "a" else "w")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    ## We need to take care of two things:
    ## * We really should not write out NA entries.
    ## * We have to handle multiple fields per record.

    ## do not assume that the input is valid in this locale
    escape_paragraphs <- function(s)
	gsub("\n \\.([^\n])","\n  .\\1",
	     gsub("\n[ \t]*\n", "\n .\n ", s, perl = TRUE, useBytes = TRUE),
             perl = TRUE, useBytes = TRUE)
    fmt <- function(tag, val, fold = TRUE) {
        s <- if(fold)
            formatDL(rep.int(tag, length(val)), val, style = "list",
                     width = width, indent = indent)
        else {
            ## Need to ensure a leading whitespace for continuation
            ## lines.
            sprintf("%s: %s", tag,
                    gsub("\n([^[:blank:]])", "\n \\1", val))
        }
        escape_paragraphs(s)
    }


    if(!is.data.frame(x))
        x <- as.data.frame(x, stringsAsFactors = FALSE)
    nmx <- names(x)
    out <- matrix("", nrow(x), ncol(x))

    foldable <- is.na(match(nmx, keep.white))

    for(j in seq_along(x)) {
        xj <- x[[j]]
        if(is.atomic(xj)) {
            ## For atomic ("character") columns, things are simple ...
            i <- !is.na(xj)
            out[i, j] <- fmt(nmx[j], xj[i], foldable[j])
        }
        else {
            ## Should be a list ...
            nmxj <- nmx[j]
            fold <- foldable[j]
            i <- !vapply(xj, function(s) (length(s) == 1L) && is.na(s), NA)
            out[i, j] <-
		vapply(xj[i],
                       function(s) {
                           paste(fmt(nmxj, s, fold), collapse = "\n")
                       }, "")
        }
    }
    out <- t(out)
    is_not_empty <- nzchar(out)
    eor <- character(sum(is_not_empty))
    if(length(eor)) {
        ## Newline for end of record.
        ## Note that we do not write a trailing blank line.
        eor[ which(diff(c(col(out))[is_not_empty]) >= 1L) ] <- "\n"
    }
    writeLines(paste0(c(out[is_not_empty]), eor), file, useBytes=useBytes)
}
