zip.file.extract <- function(file, zipname = "R.zip")
{
    ## somewhat system-specific.
    unzip <- getOption("unzip")
    if(!nchar(unzip)) unzip <- "internal"
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- dirname(tempfile())
        if(unzip != "internal") {
            if(!system(paste(unzip, "-o",
                             file.path(path, zipname), topic, "-d", tempdir,
                             " > /dev/null")))
                file <- file.path(tempdir, topic)
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname),
                                      topic, tempdir))
            if (rc == 0)
                file <- file.path(tempdir, topic)
        }
    }
    file
}
