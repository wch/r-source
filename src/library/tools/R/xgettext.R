xgettext <- function(dir, verbose = FALSE, asCall = TRUE)
{
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for(d in c("unix", "windows", "aqua")) {
        OSdir <- file.path(dir, d)
        if(file_test("-d", OSdir))
            R_files <- c(R_files, list_files_with_exts(OSdir, exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files

    find_strings <- function(e) {
         if(is.call(e) && is.name(e[[1]])
           && as.character(e[[1]]) %in% c("warning", "stop", "gettext")) {
             ## remove named args
             if(!is.null(names(e))) e <- e[nchar(names(e)) == 0]
             if(asCall) strings <<- c(strings, as.character(e)[-1])
             else for(i in seq(along = e)) find_strings2(e[[i]])
        } else if(is.recursive(e))
            for(i in seq(along = e)) Recall(e[[i]])
    }
    find_strings2 <- function(e) {
        if(is.character(e)) strings <<- c(strings, e)
        else if(is.call(e)) for(i in seq(along = e)) Recall(e[[i]])
    }

    for(f in R_files) {
        if(verbose) cat("parsing ", sQuote(f), "\n", sep="")
        strings <- character()
        for(e in parse(file = f)) find_strings(e)
        ## strip leading and trailing white space
        strings <- sub("^[ \t\n]*", "", strings)
        strings <- sub("[ \t\n]*$", "", strings)
        out[[f]] <- structure(unique(strings), class="xgettext")
    }

    out[sapply(out, length) > 0]
}

print.xgettext <- function(x, ...)
{
    cat(x, sep="\n")
    invisible(x)
}

xgettext2pot <- function(dir, potFile)
{
    if(missing(potFile)) potFile <- paste("R-", basename(dir), ".pot", sep="")
    tmp <- unique(unlist(xgettext(dir, asCall = FALSE)))
    tmp <- tmp[nchar(tmp) > 0]
    tmp <- shQuote(encodeString(tmp), type="cmd")  # need to quote \n, \t etc
    con <- file(potFile, "wt")
    on.exit(close(con))
    writeLines(con=con,
               c('msgid ""',
                 'msgstr ""',
                 '"Project-Id-Version: R 2.1.0\\n"',
                 '"Report-Msgid-Bugs-To: bugs@r-project.org\\n"',
                 paste('"POT-Creation-Date: ',
                       format(Sys.time(), "%Y-%m-%d %H:%M"), # %z is not portable
                       '\\n"', sep=''),
                 '"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n"',
                 '"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n"',
                 '"Language-Team: LANGUAGE <LL@li.org>\\n"',
                 '"MIME-Version: 1.0\\n"',
                 '"Content-Type: text/plain; charset=ASCII\\n"',
                 '"Content-Transfer-Encoding: 8bit\\n"', ''))
    for(e in tmp)
        writeLines(con=con, c('', paste('msgid', e), 'msgstr ""'))
}
