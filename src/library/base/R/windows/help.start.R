help.start <- function(gui = "irrelevant", browser = "irrelevant")
{
    a <- file.path(R.home(), "doc/html/rwin.html")
    if(!file.exists(a))
        a <- file.path(R.home(), "doc/html/rwin.htm")
    if(!file.exists(a))
        stop("I can't find the html help")
    else {
        a <- gsub("/", "\\\\", a)
        cat("If nothing happens, you have to open `",a,"' yourself\n")
        .Internal(help.start());
    }
    invisible("")
}
