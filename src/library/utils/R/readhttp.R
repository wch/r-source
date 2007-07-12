url.show <-
    function (url,  title = url, file = tempfile(),
              delete.file = TRUE, method, ...)
{
    if (download.file(url, destfile = file, method = method) != 0)
        stop("transfer failure")
    file.show(file, delete.file = delete.file, title = title, ...)
}



defaultUserAgent <- function()
{
    Rver <- paste(R.version$major, R.version$minor, sep=".")
    Rdetails <- paste(Rver, R.version$platform, R.version$arch,
                      R.version$os)
    paste("R (", Rdetails, ")", sep="")
}


makeUserAgent <- function(format = TRUE) {
    agent <- getOption("HTTPUserAgent")
    if (is.null(agent)) {
        return(NULL)
    }
    if (length(agent) != 1)
      stop(sQuote("HTTPUserAgent"),
           " option must be a length one character vector or NULL")
    if (format)
      paste("User-Agent: ", agent[1], "\r\n", sep = "")
    else
      agent[1]
}
