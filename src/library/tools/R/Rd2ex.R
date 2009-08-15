#  File src/library/tools/R/Rd2ex.R
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

Rd2ex <-
    function(Rd, out="", defines=.Platform$OS.type, stages="render",
             outputEncoding="UTF-8", ...)
{
    of0 <- function(...)
        writeLinesUTF8(paste(..., sep=""), con, outputEncoding, sep ="")
    of1 <- function(text)
        writeLinesUTF8(text, con, outputEncoding, sep = "")

    wr <- function(x)
        paste("###", strwrap(remap(x), 73L, indent=1L, exdent=3L),
              sep="", collapse="\n")

    remap <- function(x) {
        if(!length(x)) return(x)
        ## \link, \var are untouched in comments: e.g. is.R
        Encoding(x) <- "unknown" ## Avoid overhead of all those gsubUTF8 calls here
        x <- gsub("\\\\(link|var)\\{([^}]+)\\}", "\\2", x, perl = TRUE)
        ## FIXME not valid in perl: use lookbehind instead.
        x <- gsub("(^|[^\\])\\\\([%{])", "\\1\\2", x)
        x <- gsub("\\\\(l|)dots", "...", x, perl = TRUE)
        ## FIXME:  Previously said "Want to leave file bytes unchanged"
        Encoding(x) <- "UTF-8"
        x
    }

    render <- function(x, prefix = "")
    {
        tag <- attr(x, "Rd_tag")
        if(tag %in% c("\\dontshow", "\\testonly")) {
            ## There are fancy rules here if not followed by \n
            ## FIXME: do this better
            of1("## Don't show: ")
            if (!grepl("^\n", x[[1L]][1L], perl = TRUE))
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            last <- x[[length(x)]]
            if (!grepl("\n$", last[length(last)], perl = TRUE))
                writeLines("", con)
            of1("## End Don't show")
        } else if (tag  == "\\dontrun") {
            ## Special case for one line.
            if (length(x) == 1L) {
                of1("## Not run: ")
                render(x[[1L]], prefix)
            } else {
                of1("## Not run: ")
                if (!grepl("^\n", x[[1L]][1L], perl = TRUE)) {
                    writeLines("", con)
                    render(x[[1L]], paste("##D", prefix))
                } else render(x[[1L]], prefix)
                for(i in 2:length(x)) render(x[[i]], paste("##D", prefix))
                last <- x[[length(x)]]
                if (!grepl("\n$", last[length(last)], perl = TRUE))
                    writeLines("", con)
                of1("## End(Not run)")
            }
        } else if (tag  == "\\donttest") {
            of1("## No test: ")
            if (!grepl("^\n", x[[1L]][1L], perl = TRUE))
                writeLines("", con)
            for(i in seq_along(x)) render(x[[i]], prefix)
            last <- x[[length(x)]]
            if (!grepl("\n$", last[length(last)], perl = TRUE))
                writeLines("", con)
            of1("## End(No test)")
        } else if (tag == "COMMENT") {
            ## % can escape a whole line (e.g. beavers.Rd) or
            ## be trailing when we want a NL
            ## This is not right (leaading spaces?) but it may do
            if(attr(x, "srcref")[2L] > 1L) writeLines("", con)
        } else if (tag %in% c("\\dots", "\\ldots")) {
            of1("...")
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

        if(length(which) > 1L)
            warning("more than one \\examples section, using the first")
        ex <- Rd[[ where[1L] ]]
        exl <- unlist(ex)
        ## Do we need to output an encoding?
        if(length(exl) && any(Encoding(exl) != "unknown")) {
            if(any(f <- sections == "\\encoding")) {
                encoding <- unlist(Rd[[which(f)]])[1L]
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
            name <- as.character(Rd[[ which(nameblk)[1L] ]])
            of0("### Name: ", name, "\n")
        }
        titleblk <- sections == "\\title"
        if (any(titleblk)) {
            title <- as.character(Rd[[ which(titleblk)[1L] ]])
            ## remove empty lines, leading whitespace
            title <- paste(sub("^\\s+", "", title[nzchar(title)], perl = TRUE),
                           collapse=" ")
            ## FIXME: more?
            title <- gsubUTF8("(---|--)", "-", title, perl =  TRUE)
        } else title <- "No title found"
        of0(wr(paste("Title: ", title, sep='')), "\n")
        aliasblks <- sections == "\\alias"
        if (any(aliasblks)) {
            aliases <- unlist(Rd[aliasblks])
            sp <- grep(" ", aliases, fixed = TRUE)
            aliases[sp] <- paste("'", aliases[sp], "'", sep = "")
            of0(wr(paste("Aliases: ", paste(aliases, collapse=" "), sep="")),
                "\n")
        }
        keyblks <- sections == "\\keyword"
        if (any(keyblks)) {
            ## some people have only empty keyword blocks.
            keys <- unlist(Rd[keyblks])
            if(length(keys)) {
                keys <- gsubUTF8("^\\s+", "", keys, perl = TRUE)
                of0(wr(paste("Keywords: ",
                             paste(keys, collapse=" "), sep="")), "\n")
            }
        }
        writeLinesUTF8(c("", "### ** Examples"), con, outputEncoding)
        for (i in seq_along(ex)) render(ex[[i]])
        of1("\n\n\n")
    }
    invisible(out)
}
