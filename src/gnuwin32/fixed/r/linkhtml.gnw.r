link.html.help <- function()
{
    make.packages.html()
    make.function.html()
    make.search.html()
}


make.packages.html <- function()
{
    f.tg <- paste(R.home(),"/doc/html/packages.html",sep="")
    f.hd <- system.file("html/packages-head.html",pkg="doc",lib=R.home())
    f.ft <- system.file("html/packages-foot.html",pkg="doc",lib=R.home())
    file.create(f.tg)
    file.append(f.tg,f.hd)
    cat("<P><TABLE align=center>\n",file=f.tg,append=TRUE)
    pg <- sort(.packages(all.available=TRUE, lib.loc=.Library))
    for (i in  pg) {
        f.t <- scan(system.file("TITLE",pkg=i),what="c",quiet=TRUE)
        cat("<TR ALIGN=LEFT VALIGN=TOP>",
            "<TD><A HREF=\"../../library/",i,"/html/00Index.html\">",
            f.t[1],"</A><TD>",paste(f.t[2:length(f.t)],collapse=" "),
            "</TD></TR>\n",file=f.tg,append=TRUE,sep="")
    }
    cat("</TABLE>\n",file=f.tg,append=TRUE)
    file.append(f.tg,f.ft)
    invisible(pg)
}


make.function.html <- function()
{
    f.tg <- paste(R.home(),"/doc/html/function.html",sep="")
    f.hd <- paste(R.home(),"/doc/html/function-head.html",sep="")
    file.create(f.tg)
    file.append(f.tg,f.hd)
    pg <- .packages(all.available=TRUE, lib.loc=.Library)
    for (p in pg) {
        f1 <- system.file("/help/AnIndex",pkg=p)
        if (f1=="") next
        b <- scan(f1,what="c",sep="\t",quiet=TRUE)
        f1 <- system.file("/help/00Titles",pkg=p)
        d <- scan(f1,what="c",sep="\t",quiet=TRUE)
        lb <- length(b)/2
        b <- cbind(t(matrix(b,2,lb)),t(matrix(d,2,lb))[,2],rep(p,lb))
        if (p==pg[1]) fun <- b
        else          fun <- rbind(fun,b)
    }
    for (which in 0:length(letters)) {
        i <- 0
        if (which==0) {
            i <- grep("^[^a-z,^A-Z]",fun[,1])
            tl <- "-- Operators, Global Variables, ... --"
        }
        else {
            cat("<a name=\"",LETTERS[which],"\">\n",file=f.tg,append=TRUE,sep="")
            i <- grep(paste("^[",letters[which],LETTERS[which],"]",sep=""),fun[,1])
            tl <- paste("--",LETTERS[which],"--")
        }
        if (i==0) break
        cat("<h2 align=center><FONT FACE=\"Courier New,Courier\"COLOR=\"#999999\">",
            tl,"</FONT></h2>\n\n<table width=100%>\n",
            file=f.tg,append=TRUE,sep="")
        ll <- i[order(substring(fun[i,1],1+(which>0)))]
        for (l in ll)
            cat("<TR><TD width=25%><A HREF=\"../../library/",
                fun[l,4],"/html/",fun[l,2],".html\">",fun[l,1],"</A></TD>\n<TD>",
                fun[l,3]," (",fun[l,4],")</TD></TR>\n",
                file=f.tg,append=TRUE,sep="")
        cat("</table>\n",file=f.tg,append=TRUE)
    }
    cat("</BODY>",file=f.tg,append=TRUE)
    invisible(fun)
}

make.search.html <- function()
{
    f.tg <- system.file("html/search/index.txt",pkg="doc",lib=R.home())
    if (f.tg != "")
        unlink(f.tg)
    else
        f.tg <- paste(R.home(),"/doc/html/search/index.txt",sep="")
    for (i in  .packages(all.available=TRUE, lib.loc=.Library)) {
        cfile <- system.file("CONTENTS",pkg=i)
        if(nchar(cfile)) {
            f.t <- scan(cfile,what="",quiet=TRUE,sep="\n")
            cat(paste(f.t, collapse="\n"), "\n", file=f.tg, append=TRUE)
        }
    }
}
