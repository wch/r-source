zip.file.extract <- function(file, zipname = "R.zip")
{
    ## somewhat system-specific.
    unzip <- getOption("unzip")
    if(!length(unzip)) return(file)
    path <- dirname(file)
    topic <- basename(file)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- dirname(tempfile())
        if(!system(paste(unzip, "-o",
                         file.path(path, zipname), topic, "-d", tempdir,
                         " > /dev/null")))
            file <- file.path(tempdir, topic)
    }
    file
}
