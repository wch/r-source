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
        find_strings2 <- function(e, suppress) {
            if(is.character(e)) {
                if(!suppress) strings <<- c(strings, e)
            } else if(is.call(e)) {
                if(is.name(e[[1]])
                   && (as.character(e[[1]])
                       %in% c("gettext", "gettextf"))) {
                    domain <- e[["domain"]]
                    suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
                }
                for(i in seq(along = e)) find_strings2(e[[i]], suppress)
            }
        }
        if(is.call(e)
           && is.name(e[[1]])
           && (as.character(e[[1]])
               %in% c("warning", "stop", "message",
                      "gettext", "gettextf"))) {
             domain <- e[["domain"]]
             suppress <- !is.null(domain) && !is.name(domain) && is.na(domain)
             ## remove named args
             if(!is.null(names(e)))
                 e <- e[!names(e) %in% c("call.", "immediate.", "domain")]
             if(asCall) {
                 if(!suppress) strings <<- c(strings, as.character(e)[-1])
             } else for(i in seq(along = e)) find_strings2(e[[i]], suppress)
        } else if(is.recursive(e))
            for(i in seq(along = e)) Recall(e[[i]])
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

print.xngettext <- function(x, ...)
{
    lapply(x, function(x)
           cat("\nmsgid        = ", x[1],
               "\nmsgid_plural = ", x[2],
               "\n", sep=""))
    invisible(x)
}

xngettext <- function(dir, verbose = FALSE)
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
           && as.character(e[[1]]) %in% "ngettext") {
             domain <- e[["domain"]]
             ## remove named domain arg
             if(!is.null(names(e))) e <- e[!names(e) %in% "domain"]
             ## for now, take second and third remaining args.
             ## <FIXME> emulate full arg-matching
             if(is.character(e[[3]]) && is.character(e[[4]]))
                 strings <<- c(strings, list(c(msg1=e[[3]], msg2=e[[4]])))
        } else if(is.recursive(e))
            for(i in seq(along = e)) Recall(e[[i]])
    }

    for(f in R_files) {
        if(verbose) cat("parsing ", sQuote(f), "\n", sep="")
        strings <- list()
        for(e in parse(file = f)) find_strings(e)
        out[[f]] <- structure(strings, class="xngettext")
    }

    out[sapply(out, length) > 0]
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
                 '"Content-Type: text/plain; charset=CHARSET\\n"',
                 '"Content-Transfer-Encoding: 8bit\\n"', ''))
    for(e in tmp)
        writeLines(con=con, c('', paste('msgid', e), 'msgstr ""'))
    tmp <- xngettext(dir)
    un <- unique(unlist(tmp, recursive=TRUE))
    for(ee in tmp)
        for(e in ee)
            if(e[1] %in% un) {
                writeLines(con=con, c('',
                           paste('msgid       ',
                                 shQuote(encodeString(e[1]), type="cmd")),
                           paste('msgid_plural',
                                 shQuote(encodeString(e[2]), type="cmd")),
                           'msgstr[0]    ""', 'msgstr[1]    ""')
                           )
                un <- un[-match(e, un)]
            }
}
