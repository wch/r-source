dev2bitmap <- function(file, type="png256", height=6, width=6, res=72,
                   pointsize, ...)
{
    gsexe <- getenv("R_GSCMD")
    if(is.null(gsexe) || nchar(gsexe) == 0) gsexe <- "gswin32c.exe"
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE, invisible=TRUE)
    st <- grep("^Available", gshelp)
    en <- grep("^Search", gshelp)
    gsdevs <- gshelp[(st+1):(en-1)]
    devs <- c(strsplit(gsdevs, " "), recursive=TRUE)
    if(match(type, devs, 0) == 0)
        stop(paste(paste("Device ", type, "is not available"),
                   "Available devices are",
                   paste(gsdevs, collapse="\n"), sep="\n"))
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    tmp <- tempfile("Rbit")
    on.exit(unlink(tmp))
    dev.print(device=postscript, file=tmp, width=width, height=height,
              pointsize=pointsize, paper="special", horizontal=FALSE, ...)
    cmd <- paste(gsexe, " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                 " -r", res,
                 " -g", ceiling(res*width), "x", ceiling(res*height),
                 " -sOutputFile=", file, " ", tmp, sep="")
    system(cmd, invisible=TRUE)
    invisible()
}
