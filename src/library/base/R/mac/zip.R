# Jago 2002
# Adapted from Windows


zip.file.extract <- function(file, zipname = "R.zip")
{
    path <- dirname(file)
    topic <- basename(file)

   if(file.exists(file.path(path, zipname))) {
        tmpd <- dirname(tempfile())
        if((unzip <- getOption("unzip")) != "internal") {
            stop("unzip not available")
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tmpd))
            if (rc == 0)
                file <- file.path(tmpd, topic)
        }
    }
    file
}

zip.unpack <- function(zipname, dest)
{
    if(file.exists(zipname)) {
        if((unzip <- getOption("unzip")) != "internal")
         stop("unzip not available")
         else {
            .Internal(int.unzip(zipname, NULL, dest))
        }
    } else stop(paste("zipfile", zipname, "not found"))
}


