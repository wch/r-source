zip.file.extract <- function(file, zipname="R.zip")
{
    ## somewhat system-specific.
    unzip <- getOption("unzip")
    if(!length(unzip)) return(file)
    path <- sub("[^/]*$","", file)
    topic <- substr(file, nchar(path)+1, 1000)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- sub("[^/]*$", "", tempfile())
        if(!system(paste(unzip, "-o",
                         file.path(path, zipname), topic, "-d", tempdir,
                         " > /dev/null")))
            file <- paste(tempdir,  topic, sep="")
    }
    file
}
