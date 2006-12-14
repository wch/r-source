packageDescription <- function(pkg, lib.loc=NULL, fields=NULL, drop=TRUE,
			       encoding = "")
{
    retval <- list()
    if(!is.null(fields)){
        fields <- as.character(fields)
        retval[fields] <- NA
    }

    if(system.file(package = pkg, lib.loc = lib.loc) == "") {
        warning(gettextf("no package '%s' was found", pkg), domain = NA)
        return(NA)
    }

    file <- system.file("DESCRIPTION", package = pkg, lib.loc = lib.loc)

    if(file != "") {
        dcf <- read.dcf(file=file)
        if(NROW(dcf) < 1)
            stop(gettextf("DESCRIPTION file of package '%s' is corrupt", pkg),
                 domain = NA)
        desc <- as.list(dcf[1,])
        ## read the Encoding field if any
        enc <- desc[["Encoding"]]
        if(!is.null(enc) && !is.na(encoding)) {
            ## Determine encoding and re-encode if necessary and possible.
            if((encoding != "" || Sys.getlocale("LC_CTYPE") != "C")
	       && capabilities("iconv")) {
                ## might have an invalid encoding ...
                newdesc <- try(lapply(desc, iconv, from=enc, to=encoding))
                if(!inherits(newdesc, "try-error")) desc <- newdesc
                else
                    warning("'DESCRIPTION' file has 'Encoding' field and re-encoding is not possible", call. = FALSE)
            } else
                warning("'DESCRIPTION' file has 'Encoding' field and re-encoding is not possible", call. = FALSE)
        }
        if(!is.null(fields)){
            ok <- names(desc) %in% fields
            retval[names(desc)[ok]] <- desc[ok]
        }
        else
            retval[names(desc)] <- desc
    }

    if((file == "") || (length(retval) == 0)){
        warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", pkg), domain = NA)
        return(NA)
    }

    if(drop & length(fields)==1)
        return(retval[[1]])

    class(retval) <- "packageDescription"
    if(!is.null(fields)) attr(retval, "fields") <- fields
    attr(retval, "file") <- file
    retval
}


print.packageDescription <- function(x, ...)
{
    write.dcf(as.data.frame.list(x))
    cat("-- File:", attr(x, "file"), "\n")
    if(!is.null(attr(x, "fields"))){
        cat("-- Fields read: ")
        cat(attr(x, "fields"), sep=", ")
        cat("\n")
    }
}

index.search <- function(topic, path, file = "AnIndex", type = "help")
    .Internal(index.search(topic, path, file, .Platform$file.sep, type))

print.packageIQR <-
function(x, ...)
{
    db <- x$results
    ## Split according to Package.
    out <- if(nrow(db) == 0)
         NULL
    else
        lapply(split(1 : nrow(db), db[, "Package"]),
               function(ind) db[ind, c("Item", "Title"),
                                drop = FALSE])
    outFile <- tempfile("RpackageIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(pkg in names(out)) {
        writeLines(paste(ifelse(first, "", "\n"), x$title,
                         " in package ", sQuote(pkg), ":\n",
                         sep = ""),
                   outConn)
        writeLines(formatDL(out[[pkg]][, "Item"],
                            out[[pkg]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        writeLines(paste("no", tolower(x$title), "found"))
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = paste("R", tolower(x$title)))
    }
    invisible(x)
}
