system <- function(call, intern = FALSE)
    .Internal(system(call, intern))

dir.create <- function(path)
    invisible(.Internal(dir.create(path)))

tempfile <- function(pattern = "file") .Internal(tempfile(pattern))

unlink <- function(x, recursive=FALSE)
    invisible(.Internal(unlink(x, recursive)))

help.start <- function(gui = "irrelevant", browser = "irrelevant")
{
    a <- file.path(R.home(), "doc", "html", "index.html")
    if(!file.exists(a))
        stop("I can't find the html help")
    else {
        cat("If nothing happens, you have to open `", a ,"' yourself\n")
        .Internal(help.start());
    }
    invisible("")
}




applescript <-function (path , scriptname) 
{
  rc <- .Internal(applescript(path,scriptname))
  if (rc != 0) 
    warning("Error in AppleScript")
}
