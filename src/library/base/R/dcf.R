#  File src/library/base/R/dcf.R
#  Part of the R package, http://www.R-project.org
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

read.dcf <-
function(file, fields = NULL, all = FALSE)
{
    if(is.character(file)){
        file <- gzfile(file, "r")
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
    ##                  else mapply("[[", s, sapply(s, length))))
    if(!all) return(.Internal(readDCF(file, fields)))

    .assemble_things_into_a_data_frame <- function(tags, vals, nums) {
        tf <- factor(tags, levels = unique(tags))

        cnts <- table(nums, tf)
        out <- array(as.character(NA), dim = dim(cnts),
                     dimnames = list(NULL, levels(tf)))
        if(all(cnts <= 1)) {
            ## No repeated tags ...
            out[cbind(nums, tf)] <- vals
            out <- as.data.frame(out, stringsAsFactors = FALSE)
        }
        else {
            levs <- colSums(cnts > 1) == 0
            if(any(levs)) {
                inds <- tf %in% levels(tf)[levs]
                out[cbind(nums[inds], tf[inds])] <- vals[inds]
            }
            out <- as.data.frame(out, stringsAsFactors = FALSE)
            for(l in levels(tf)[!levs]) {
                out[[l]] <- rep.int(list(as.character(NA)), nrow(cnts))
                i <- tf == l
                out[[l]][unique(nums[i])] <- split(vals[i], nums[i])
            }
        }

        out
    }

    on.exit(Sys.setlocale("LC_CTYPE", Sys.getlocale("LC_CTYPE")), add = TRUE)
    Sys.setlocale("LC_CTYPE", "C")

    lines <- readLines(file)

    ## Try to find out about invalid things: mostly, lines which do not
    ## start with blanks but have no ':' ...
    ind <- grep("^[^[:blank:]][^:]*$", lines)
    if(length(ind)) stop("Invalid DCF format.")

    line_is_not_empty <- regexpr("^[[:space:]]*$", lines) < 0
    nums <- cumsum(diff(c(FALSE, line_is_not_empty) > 0) > 0)
    ## Remove the empty ones so that nums knows which record each line
    ## belongs to.
    nums <- nums[line_is_not_empty]
    lines <- lines[line_is_not_empty]

    ## Deal with escaped blank lines (used by Debian at least for the
    ## Description: values, see man 5 deb-control):
    line_is_escaped_blank <-
        regexpr("^[[:space:]]+\\.[[:space:]]*$", lines) > -1
    if(any(line_is_escaped_blank))
        lines[line_is_escaped_blank] <- ""

    line_has_tag <- regexpr("^[^[:blank:]][^:]*:", lines) > -1

    ## Check that records start with tag lines.
    if(!all(line_has_tag[which(diff(nums) > 0) + 1]))
        stop("Invalid DCF format.")

    ## End positions of field entries.
    pos <- cumsum(rle(cumsum(line_has_tag))$lengths)

    tags <- sub(":.*", "", lines[line_has_tag])
    lines[line_has_tag] <-
        sub("[^:]*:[[:space:]]*", "", lines[line_has_tag])
    lines[!line_has_tag] <-
        sub("^[[:space:]]*", "", lines[!line_has_tag])

    vals <- mapply(function(from, to) paste(lines[from:to],
                                            collapse = "\n"),
                   c(1, pos[-length(pos)] + 1), pos)

    out <- .assemble_things_into_a_data_frame(tags, vals, nums[pos])

    if(!is.null(fields))
        out <- out[fields]

    out
}

write.dcf <-
function(x, file = "", append = FALSE,
         indent = 0.1 * getOption("width"),
         width = 0.9 * getOption("width"))
{
    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    ## We need to take care of two things:
    ## * We really should not write out NA entries.
    ## * We have to handle multiple fields per record.

    escape_paragraphs <- function(s)
        gsub("\n[[:space:]]*\n", "\n .\n ", s)

    if(!is.data.frame(x))
        x <- as.data.frame(x, stringsAsFactors = FALSE)
    nmx <- names(x)
    out <- matrix("", nrow(x), ncol(x))
    for(j in seq_along(x)) {
        xj <- x[[j]]
        if(is.atomic(xj)) {
            ## For atomic ("character") columns, things are simple ...
            i <- !is.na(xj)
            s <- formatDL(rep.int(nmx[j], sum(i)), xj[i],
                          style = "list", width = width,
                          indent = indent)
            out[i, j] <- escape_paragraphs(s)
        }
        else {
            ## Should be a list ...
            nmxj <- nmx[j]
            i <- !sapply(xj, function(s) (length(s) == 1) && is.na(s))
            out[i, j] <-
                sapply(xj[i],
                       function(s) {
                           s <- formatDL(rep.int(nmxj, length(s)), s,
                                         style = "list", width = width,
                                         indent = indent)
                           paste(escape_paragraphs(s), collapse = "\n")
                       })
        }
    }

    out <- t(out)
    is_not_empty <- c(out != "")
    eor <- character(sum(is_not_empty))
    if(length(eor)) {
        ## Newline for end of record.
        ## Note that we do not write a trailing blank line.
        eor[ diff(c(col(out))[is_not_empty]) >= 1 ] <- "\n"
    }
    writeLines(paste(c(out[is_not_empty]), eor, sep = ""), file)
}
