win.packages.html <-
    function(lib.loc=.libPaths(), docdir = R.home("doc"), libdir = .Library)
{
    f.tg <- file.path(docdir, "html", "packages.html")
    f.hd <- file.path(docdir, "html", "packages-head-utf8.html")
    if(!file.create(f.tg)) {
        warning("cannot update HTML package index")
        return(FALSE)
    }
    file.append(f.tg, f.hd)
    out <- file(f.tg, open="a")
    rh <- chartr("\\", "/", R.home())
    drive <- substring(rh, 1, 2)
    for (lib in lib.loc) {
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        ## use relative indexing for .Library
        if(is.na(pmatch(rh, lib))) {
            libname <- chartr("/", "\\", lib)
            lib0 <- if(substring(lib, 2, 2) != ":")
                paste(drive, lib, sep="") else lib
            lib0 <- paste("file:///", lib0, sep="")
        } else {
            lib0 <- "../../library"
            libname <- "the standard library"
        }
        if(length(lib.loc) > 1)
            cat("<p><h3>Packages in ", libname, "</h3>\n",
                sep = "", file = out)
        if(libname != "the standard library")
            cat("<p>Cross-links from this library to other libraries may not work.\n\n", file = out)
        cat("<p>\n<table width=\"100%\" summary=\"R Package list\">\n",
            file = out)
        for (i in  pg) {
            ## there is a bootstrapping problem using packageDescription
            file <- system.file("Meta", "package.rds", package = i,
                                    lib.loc = lib)
            title <- if(file != "") {
                txt <- .readRDS(file)
                if(is.list(txt)) txt <- txt$DESCRIPTION
                ## we may need to re-encode here.
                if("Encoding" %in% names(txt)) {
                        to <- "UTF-8"
                        tmp <- try(iconv(txt, txt["Encoding"], to, "?"))
                        if(!inherits(tmp, "try-error"))
                            txt <- tmp
                        else
                            warning("'DESCRIPTION' has 'Encoding' field and re-encoding is not possible", call.=FALSE)
                    }
                txt["Title"]
            } else NA

#             title <- utils::packageDescription(i, lib.loc = lib, field = "Title",
#                                         encoding = "UTF-8")
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib0, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}

unix.packages.html <-
    function(lib.loc=.libPaths(), docdir = R.home("doc"), libdir = .Library)
{
    f.tg <- file.path(docdir, "html", "packages.html")
    if(!file.create(f.tg)) {
        warning("cannot create HTML package index")
        return(FALSE)
    }
    ## First we need to fathom out what encoding to use.
    ## For now we assume that if we have iconv then UTF-8 is OK.
    ## It is OK if cross-building.
    useUTF8 <- capabilities("iconv")
    if(useUTF8)
        file.append(f.tg, file.path(docdir, "html",
                                    "packages-head-utf8.html"))
    else
        file.append(f.tg, file.path(docdir, "html",
                                    "packages-head.html"))
    out <- file(f.tg, open="a")
    for (lib in lib.loc) {
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        ## use relative indexing for .Library
        if(lib != libdir) {
            libname <- lib
            lib0 <- paste("file:///", lib, sep="")
        } else {
            lib0 <- "../../library"
            libname <- "the standard library"
        }
        cat("<p><h3>Packages in ", libname,
            '</h3>\n<p>\n<table width="100%" summary="R Package list">\n',
            sep = "", file=out)
        for (i in pg) {
            title <- utils::packageDescription(i, lib.loc = lib, field = "Title",
                                        encoding = ifelse(useUTF8,"UTF-8",""))
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib0, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}
