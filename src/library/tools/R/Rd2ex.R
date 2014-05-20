#  File src/library/tools/R/Rd2ex.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

## This warns on multiple \examples sections, never fails.

Rd2ex <-
    function(Rd, out="", defines=.Platform$OS.type, stages="render",
             outputEncoding="UTF-8", commentDontrun = TRUE, ...)
{
    encode_warn <- FALSE
    WriteLines <- function(x, con, outputEncoding, ...) {
        if (outputEncoding != "UTF-8") {
            x <- iconv(x, "UTF-8", outputEncoding,  mark=FALSE)
            if (anyNA(x)) {
                x <- iconv(x, "UTF-8", outputEncoding, sub="byte", mark=FALSE)
                encode_warn <<- TRUE
            }
        }
        writeLines(x, con, useBytes = TRUE, ...)
    }

    dropNewline <- FALSE # drop next char if newline

    of0 <- function(...)
        of1(paste0(...))
    of1 <- function(text) {
        if (dropNewline && length(text)) {
            text[1L] <- psub("^\n", "", text[1L])
            dropNewline <<- FALSE
        }
        WriteLines(text, con, outputEncoding, sep = "")
    }
    wr <- function(x)
	paste0("###", strwrap(remap(x), 73L, indent=1L, exdent=3L), collapse="\n")

    remap <- function(x) {
        if(!length(x)) return(x)
        ## \link, \var are untouched in comments: e.g. is.R
        x <- psub("\\\\(link|var)\\{([^}]+)\\}", "\\2", x)
        ## not valid in perl: use lookbehind instead.
        ## x <- gsub("(^|[^\\])\\\\([%{])", "\\1\\2", x)
        x <- psub("(?<!\\\\)\\\\([%{])", "\\1", x)
        x <- psub("\\\\(l|)dots", "...", x)
        ## FIXME:  Previously said "Want to leave file bytes unchanged"
        x
    }

    render <- function(x, prefix = "")
    {
        tag <- attr(x, "Rd_tag")
        if(tag %in% c("\\dontshow", "\\testonly")) {
            of1("## Don't show: ")
            if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && RdTags(x)[1L] != "COMMENT")
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            last <- x[[length(x)]]
            if (!grepl("\n$", last[length(last)], perl = TRUE))
                writeLines("", con)
            of1("## End Don't show")
        } else if (tag  == "\\dontrun") {
            if (commentDontrun)
                of1("## Not run: ")
            ## Special case for one line.
            if (length(x) == 1L && commentDontrun) {
                render(x[[1L]], prefix)
            } else {
                if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && RdTags(x)[1L] != "COMMENT") {
                    writeLines("", con)
                    render(x[[1L]], paste0(if (commentDontrun) "##D ", prefix))
                } else render(x[[1L]], prefix)
                if(length(x) >= 2L)
                    for(i in 2:length(x))
                        render(x[[i]], paste0(if (commentDontrun) "##D ", prefix))
                last <- x[[length(x)]]
                if (!grepl("\n$", last[length(last)], perl = TRUE))
                    writeLines("", con)
                if (commentDontrun)
                    of1("## End(Not run)")
            }
        } else if (tag  == "\\donttest") {
            of1("## No test: ")
            if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && RdTags(x)[1L] != "COMMENT")
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            last <- x[[length(x)]]
            if (!grepl("\n$", last[length(last)], perl = TRUE))
                writeLines("", con)
            of1("## End(No test)")
        } else if (tag == "COMMENT") {
            ## % can escape a whole line (e.g. beavers.Rd) or
            ## be trailing when we want a NL
            ## This is not right (leading spaces?) but it may do
            if(attr(x, "srcref")[2L] == 1L) dropNewline <<- TRUE
        } else if (tag %in% c("\\dots", "\\ldots")) {
            of1("...")
        } else if (tag == "\\if" || tag == "\\ifelse") {
            if (testRdConditional("example", x, Rdfile))
            	for(i in seq_along(x[[2L]])) render(x[[2L]][[i]], prefix)
            else if (tag == "\\ifelse")
            	for(i in seq_along(x[[3L]])) render(x[[3L]][[i]], prefix)
        } else if (tag == "\\out") {
            for (i in seq_along(x))
            	of1(x[[i]])
        } else if (tag %in% c("USERMACRO", "\\newcommand", "\\renewcommand")) {
            # do nothing
        } else {
            txt <- unlist(x)
            of0(prefix, remap(txt))
        }
    }

    Rd <- prepare_Rd(Rd, defines=defines, stages=stages, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)

    ## FIXME should we skip empty \examples sections?
    where <- which(sections == "\\examples")
    if(length(where)) {
	if (is.character(out)) {
	    if(out == "") {
		con <- stdout()
	    } else {
		con <- file(out, "wt")
		on.exit(close(con))
	    }
        } else {
            con <- out
            out <- summary(con)$description
        }

        if(length(where) > 1L)
            warning("more than one \\examples section, using the first")
        ex <- Rd[[ where[1L] ]]
        exl <- unlist(ex)
        ## Do we need to output an encoding?
        if(length(exl) && any(Encoding(exl) != "unknown")) {
            if(any(f <- sections == "\\encoding")) {
                encoding <- unlist(Rd[which(f)])[1L]
                ## FIXME: which should win here?
                if(nzchar(outputEncoding))
                    encoding <- outputEncoding
                else
                    outputEncoding <- encoding
                of0("### Encoding: ", encoding, "\n\n") #
            }
        }
        nameblk <- sections == "\\name"
        if (any(nameblk)) {
            ## perl wrapped here, but it seems unnecessary
            name <- as.character(Rd[[ which.max(nameblk) ]])
            of0("### Name: ", name, "\n")
        }
        title <- .Rd_format_title(.Rd_get_title(Rd))
        if (!length(title))
            title <- "No title found"
        of0(wr(paste0("Title: ", title)), "\n")
        aliasblks <- sections == "\\alias"
        if (any(aliasblks)) {
            aliases <- unlist(Rd[aliasblks])
            sp <- grep(" ", aliases, fixed = TRUE)
            aliases[sp] <- paste0("'", aliases[sp], "'")
            of0(wr(paste0("Aliases: ", paste(aliases, collapse=" "))),
                "\n")
        }
        keyblks <- sections == "\\keyword"
        if (any(keyblks)) {
            ## some people have only empty keyword blocks.
            keys <- unlist(Rd[keyblks])
            if(length(keys)) {
                keys <- psub("^\\s+", "", keys)
                of0(wr(paste("Keywords: ",
                             paste0(keys, collapse=" "))), "\n")
            }
        }
        writeLines(c("", "### ** Examples"), con)
        for (i in seq_along(ex)) render(ex[[i]])
        of1("\n\n\n")
    }
    invisible(out)
}
