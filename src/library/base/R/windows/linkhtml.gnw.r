link.html.help <- function(verbose=FALSE, lib.loc=.Library)
{
    if(!file.exists(file.path(R.home(), "doc", "html")))
       return(invisible(NULL))
    if(verbose) {
        cat("updating HTML package descriptions\n")
        flush.console()
    }
    make.packages.html(lib.loc)
    make.search.html(lib.loc)
}


make.packages.html <- function(lib.loc=.Library)
{
    f.tg <- file.path(R.home(), "doc/html/packages.html")
    f.hd <- file.path(R.home(), "doc/html/packages-head.html")
    file.create(f.tg)
    file.append(f.tg, f.hd)
    out <- file(f.tg, open="a")
    cat('<table align="center" summary="R Package list">\n', file=out)
    pg <- sort(.packages(all.available = TRUE, lib.loc = lib.loc))
    for (i in  pg) {
        t.file <- system.file("TITLE", package = i)
        if (nchar(t.file) > 0)
            f.t <- scan(t.file, what="c", quiet=TRUE, quote="")
        else {
            title <- package.description(i, field="Title")[1]
            if (title == "NA") title <- "-- Title is missing --"
            f.t <- c(i, title)
        }
        lib <- gsub(":", "|", system.file(package=i))
        cat('<tr align="left" valign="top">\n',
            "<td><a href=\"file:///", lib, "/html/00Index.html\">",
            f.t[1], "</a></td><td>", paste(f.t[-1], collapse=" "),
            "</td></tr>\n", file=out, sep="")
    }
    cat("</table>\n</body></html>\n", file=out)
    close(out)
    invisible(pg)
}



make.search.html <- function(lib.loc=.Library)
{
    f.tg <- file.path(R.home(), "doc/html/search/index.txt")
    if(file.exists(f.tg)) unlink(f.tg)
    out <- file(f.tg, open="w")
    for (i in  .packages(all.available=TRUE, lib.loc=lib.loc)) {
        cfile <- system.file("CONTENTS", package = i)
        if(nchar(cfile)) writeLines(readLines(cfile), out)
    }
    close(out)
}
