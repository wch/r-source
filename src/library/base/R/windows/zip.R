zip.file.extract <- function(file, zipname="R.zip")
{
    ofile <- gsub("\\\\", "/", file)
    path <- sub("[^/]*$","", ofile)
    topic <- substr(ofile, nchar(path)+1, 1000)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- sub("[^\\]*$","", tempfile())
        if((unzip <- getOption("unzip")) != "internal") {
            if(!system(paste(unzip, ' -oq "',
                             file.path(path, zipname), '" ', topic,
                             " -d ", tempdir, sep=""), invisible = TRUE))
                file <- paste(tempdir,  topic, sep="")
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tempdir))
            if (rc == 10)
                warning(paste(R.home(),
                              "unzip\\unzip32static.dll cannot be loaded", sep="\\"))
            if (rc == 0)
                file <- paste(tempdir, topic, sep="")
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
            rc <- .Internal(int.unzip(zipname, NULL, dest))
            if (rc == 10)
                warning(paste(R.home(),
                              "unzip\\unzip32static.dll cannot be loaded", sep="\\"))
            rc
        }
    } else stop(paste("zipfile", zipname, "not found"))
}

