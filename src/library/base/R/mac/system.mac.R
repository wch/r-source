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


zip.file.extract <- function(file, zipname="R.zip")
{
    if(file.exists(file)) return(file)  # if the file exists, do not
                                        # replace it

    ofile <- gsub("::::", ":", file)
    path <- sub("[^:]*$","", ofile)  # changed "/" to ":"
    topic <- substr(ofile, nchar(path)+1, 1000)

    if(file.exists(file.path(path, zipname,fsep=""))) {

        tempdir <- sub("[^:]*$","", tempfile())
        if((unzip <- getOption("unzip")) != "mac.unzip") {
            if(!system(paste(unzip, ' -oq "',
                             file.path(path, zipname), '" ', topic,
                             " -d ", tempdir, sep=""), invisible = TRUE))
                file <- paste(tempdir,  topic, sep="")
        }
        else  {    # mac.unzip
            rc <- .Internal(int.unzip(file.path(path, zipname, fsep=""),
                                      topic, path))
            if (rc != 0) warning("Error in MacUnZip")
        }
    }
    file
}

applescript <-function (path , scriptname) 
{
  rc <- .Internal(applescript(path,scriptname))
  if (rc != 0) 
    warning("Error in AppleScript")
}
