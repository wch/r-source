zip.file.extract <- function(file, zipname = "R.zip",
                             unzip = getOption("unzip"))
{
    if(!is.character(unzip) || length(unzip) != 1)
        stop("'unzip' must be a single character string")
    if(!nchar(unzip)) unzip <- "internal"
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        tmpd <- tempdir()
        if(unzip != "internal") {
            if(!system(paste(unzip, "-oq",
                             shQuote(file.path(path, zipname)), topic,
                             "-d", tmpd, " > /dev/null")))
                file <- file.path(tmpd, topic)
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tmpd))
            if (rc == 0)
                file <- file.path(tmpd, topic)
        }
    }
    file
}
