link.html.help <- function(verbose=FALSE)
{
    if(!file.exists(file.path(R.home(), "doc", "html")))
       return(invisible(NULL))
    if(verbose) {
        cat("updating HTML package descriptions\n")
        flush.console()
    }
    make.packages.html()
    make.function.html()
    make.search.html()
}


make.packages.html <- function()
{
    f.tg <- file.path(R.home(), "doc/html/packages.html")
    f.hd <- file.path(R.home(), "doc/html/packages-head.html")
    f.ft <- file.path(R.home(), "doc/html/packages-foot.html")
    file.create(f.tg)
    file.append(f.tg, f.hd)
    cat("<P><TABLE align=center>\n", file=f.tg, append=TRUE)
    pg <- sort(.packages(all.available = TRUE, lib.loc = .Library))
    for (i in  pg) {
        t.file <- system.file("TITLE", pkg = i)
        if (nchar(t.file) > 0)
            f.t <- scan(t.file, what="c", quiet=TRUE, quote="")
        else {
            title <- package.description(i, field="Title")[1]
            if (title == "NA") title <- "-- Title is missing --"
            f.t <- c(i, title)
        }
        cat("<TR ALIGN=LEFT VALIGN=TOP>",
            "<TD><A HREF=\"../../library/", i ,"/html/00Index.html\">",
            f.t[1], "</A><TD>", paste(f.t[-1], collapse=" "),
            "</TD></TR>\n", file=f.tg, append=TRUE, sep="")
    }
    cat("</TABLE>\n", file=f.tg, append=TRUE)
    file.append(f.tg, f.ft)
    invisible(pg)
}


make.function.html <- function()
{
    f.tg <- file.path(R.home(), "/doc/html/function.html")
    f.hd <- file.path(R.home(), "/doc/html/function-head.html")
    file.create(f.tg)
    file.append(f.tg,f.hd)
    pg <- .packages(all.available=TRUE, lib.loc=.Library)
    for (p in pg) {
        f1 <- system.file("/help/AnIndex", pkg=p)
        if (f1=="") next
        b <- scan(f1, what="c", sep="\t", quiet = TRUE, quote="")
        b <- matrix(b, ncol=2, byrow=TRUE)
        f1 <- system.file("/help/00Titles", pkg = p)
        d <- scan(f1, what="c", sep="\t", quiet = TRUE, quote="")
        d <- matrix(d, ncol=2, byrow=TRUE)
        m <- match(b[,2], d[, 1])
        b <- cbind(b, d[m, 2], p)
        if (p == pg[1]) fun <- b
        else            fun <- rbind(fun, b)
    }
    for (which in 0:length(letters)) {
        i <- 0
        if (which==0) {
            i <- grep("^[^a-z,^A-Z]", fun[,1])
            tl <- "-- Operators, Global Variables, ... --"
        } else {
            cat("<a name=\"", LETTERS[which], "\">\n", file=f.tg,
                append=TRUE, sep="")
            i <- grep(paste("^[", letters[which], LETTERS[which],"]", sep=""),
                      fun[,1])
            tl <- paste("--", LETTERS[which],"--")
        }
        if (i==0) break
        cat("<h2 align=center><FONT FACE=\"Courier New,Courier\" COLOR=\"#999999\">",
            tl,"</FONT></h2>\n\n<table width=100%>\n",
            file=f.tg, append=TRUE, sep="")
        ll <- i[order(substring(fun[i,1], 1 + (which > 0)))]
        for (l in ll)
            cat("<TR><TD width=25%><A HREF=\"../../library/",
                fun[l,4], "/html/", fun[l,2], ".html\">",
                fun[l,1], "</A></TD>\n<TD>",
                fun[l,3], " (", fun[l,4], ")</TD></TR>\n",
                file=f.tg, append=TRUE, sep="")
        cat("</table>\n", file=f.tg, append=TRUE)
    }
    cat("</BODY>", file=f.tg, append=TRUE)
    invisible(fun)
}

make.search.html <- function()
{
    f.tg <- system.file("html/search/index.txt", pkg="doc", lib=R.home())
    if (f.tg != "")
        unlink(f.tg)
    else
        f.tg <- file.path(R.home(), "doc/html/search/index.txt")
    for (i in  .packages(all.available=TRUE, lib.loc=.Library)) {
        cfile <- system.file("CONTENTS", pkg = i)
        if(nchar(cfile)) {
            f.t <- scan(cfile, what="", quiet=TRUE, sep="\n", quote="")
            cat(paste(f.t, collapse="\n"), "\n", file=f.tg, append=TRUE)
        }
    }
}
