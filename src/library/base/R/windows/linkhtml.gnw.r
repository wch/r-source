link.html.help <- function(verbose=FALSE, lib.loc=.libPaths())
{
    if(!file.exists(file.path(R.home(), "doc", "html", "search")))
       return(invisible(NULL))
    if(verbose) {
        cat("updating HTML package descriptions\n")
        flush.console()
    }
    make.packages.html(lib.loc)
    make.search.html(lib.loc)
}

make.packages.html <- function(lib.loc=.libPaths())
{
    f.tg <- file.path(R.home(), "doc/html/packages.html")
    f.hd <- file.path(R.home(), "doc/html/packages-head.html")
    if(!file.create(f.tg)) {
        warning("cannot update HTML package index")
        return(FALSE)
    }
    file.append(f.tg, f.hd)
    out <- file(f.tg, open="a")
    cat('<table width=\"100%\" summary="R Package list">\n',
        file=out)
    rh <- gsub("\\\\", "/", R.home())
    drive <- substring(rh, 1, 2)
    for (lib in lib.loc) {
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        ## use relative indexing for .Library
        if(is.na(pmatch(rh, lib))) {
            libname <- gsub("/", "\\\\", lib)
            if(substring(lib, 2, 2) != ":")
                lib <- paste(drive, lib, sep="")
            lib <- paste("file:///", lib, sep="")
        } else {
            lib <- "../../library"
            libname <- "the standard library"
        }
        if(length(lib.loc) > 1)
            cat("<p><h3>Packages in ", libname, "</h3>\n",
                sep = "", file = out)
        if(libname != "the standard library")
            cat("<p>Cross-links from this library to other libraries may not work.\n\n",file = out)
        cat("<p>\n<table width=\"100%\">\n", file = out)
        for (i in  pg) {
            title <- package.description(i, field="Title")[1]
            if (is.na(title)) title <- "-- Title is missing --"
            cat('<tr align="left" valign="top">\n',
                '<td width="25%"><a href="', lib, '/', i,
                '/html/00Index.html">', i, "</a></td><td>", title,
                "</td></tr>\n", file=out, sep="")
        }
        cat("</table>\n\n", file=out)
    }
    cat("</body></html>\n", file=out)
    close(out)
    invisible(TRUE)
}

make.search.html <- function(lib.loc=.libPaths())
{
    f.tg <- file.path(R.home(), "doc/html/search/index.txt")
    out <- file(f.tg, open = "w")
    if(class(out) == "try-error") {
        warning("cannot update HTML search index")
        return()
    }
    for (lib in lib.loc) {
        rh <- gsub("\\\\", "/", R.home())
        drive <- substring(rh, 1, 2)
        pg <- sort(.packages(all.available = TRUE, lib.loc = lib))
        ## use relative indexing for .Library
        if(is.na(pmatch(rh, lib))) {
            if(substring(lib, 2, 2) != ":")
                lib <- paste(drive, lib, sep="")
            lib <- paste("URL: file:///", lib, sep="")
            sed.it <- TRUE
        } else {
            sed.it <- FALSE
        }
        for (i in pg) {
            cfile <- system.file("CONTENTS", package = i)
            if(nchar(cfile)) {
                tmp <- if(sed.it)
                    gsub("^URL: ../../../library", lib, readLines(cfile))
                else readLines(cfile)
                writeLines(tmp, out)
            }
        }
    }
    close(out)
}
