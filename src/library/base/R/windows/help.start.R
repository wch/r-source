help.start <- function(gui = "irrelevant", browser = "irrelevant")
{
    a <- system.file("rwin.html", pkg="doc/html", lib=R.home())
    if (a == "")
        a <- system.file("rwin.htm", pkg="doc/html", lib=R.home())
    if (a == "")
        stop("I can't find the html help\n")
    else {
        a <- gsub("/", "\\\\", a)
        cat("If nothing happens, you have to open `",a,"' yourself\n")
        .Internal(help.start());
    }
    invisible("")
}
