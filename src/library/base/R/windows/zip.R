zip.file.extract <- function(file, zipname = "R.zip")
{
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- dirname(tempfile())
        if((unzip <- getOption("unzip")) != "internal") {
            if(!system(paste(unzip, ' -oq "',
                             file.path(path, zipname), '" ', topic,
                             " -d ", tempdir, sep=""), invisible = TRUE))
                file <- file.path(tempdir, topic)
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tempdir))
            if (rc == 0)
                file <- file.path(tempdir, topic)
        }
    }
    file
}

### the following function supports update.packages()

zip.unpack <- function(zipname, dest)
{
    if(file.exists(zipname)) {
        if((unzip <- getOption("unzip")) != "internal") {
            system(paste(unzip, "-oq", zipname, "-d", dest),
                   show = FALSE, invisible = TRUE)
        } else {
            .Internal(int.unzip(zipname, NULL, dest))
        }
    } else stop(paste("zipfile", zipname, "not found"))
}

