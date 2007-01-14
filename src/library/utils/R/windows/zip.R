zip.file.extract <- function(file, zipname = "R.zip")
{
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        tmpd <- tempdir()
        if((unzip <- getOption("unzip")) != "internal") {
            if(!system(paste(unzip, ' -oq "',
                             file.path(path, zipname), '" ', topic,
                             " -d ", tmpd, sep=""), invisible = TRUE))
                file <- file.path(tmpd, topic)
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tmpd))
            if (rc == 0)
                file <- file.path(tmpd, topic)
        }
    }
    file
}
