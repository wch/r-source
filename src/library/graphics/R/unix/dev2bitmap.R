dev2bitmap <- function(file, type="png256", height=6, width=6, res=72,
                       pointsize, ...)
{
    if(missing(file)) stop("`file' is missing with no default")
    if(!is.character(file) || nchar(file) == 0)
        stop("`file' is must be a non-empty character string")
    gsexe <- Sys.getenv("R_GSCMD")
    if(is.null(gsexe) || nchar(gsexe) == 0) {
        gsexe <- "gs"
        rc <- system(paste(gsexe, "-help > /dev/null"))
        if(rc != 0) stop("Sorry, gs cannot be found")
    }
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE)
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
    din <- par("din"); w <- din[1]; h <- din[2]
    if(missing(width) && !missing(height)) width <- w/h * height
    if(missing(height) && !missing(width)) height <- h/w * width

    current.device <- dev.cur()
    dev.off(dev.copy(device = postscript, file=tmp, width=width,
                     height=height,
                     pointsize=pointsize, paper="special",
                     horizontal=FALSE, ...))
    dev.set(current.device)
    cmd <- paste(gsexe, " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                 " -r", res,
                 " -g", ceiling(res*width), "x", ceiling(res*height),
                 " -sOutputFile=", file, " ", tmp, sep="")
    system(cmd)
    invisible()
}

bitmap <- function(file, type="png256", height=6, width=6, res=72,
                   pointsize, ...)
{
    if(missing(file)) stop("`file' is missing with no default")
    if(!is.character(file) || nchar(file) == 0)
        stop("`file' is must be a non-empty character string")
    gsexe <- Sys.getenv("R_GSCMD")
    if(is.null(gsexe) || nchar(gsexe) == 0) {
        gsexe <- "gs"
        rc <- system(paste(gsexe, "-help > /dev/null"))
        if(rc != 0) stop("Sorry, gs cannot be found")
    }
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE)
    st <- grep("^Available", gshelp)
    en <- grep("^Search", gshelp)
    gsdevs <- gshelp[(st+1):(en-1)]
    devs <- c(strsplit(gsdevs, " "), recursive=TRUE)
    if(match(type, devs, 0) == 0)
        stop(paste(paste("Device ", type, "is not available"),
                   "Available devices are",
                   paste(gsdevs, collapse="\n"), sep="\n"))
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    cmd <- paste("|", gsexe, " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                 " -r", res,
                 " -g", ceiling(res*width), "x", ceiling(res*height),
                 " -sOutputFile=", file, " -", sep="")
    postscript(file=cmd, width=width, height=height,
               pointsize=pointsize, paper="special", horizontal=FALSE, ...)
    invisible()
}

png <- function(filename = "Rplot%03d.png",
                width=480, height=480, pointsize=12,
                gamma = 1, colortype = getOption("X11colortype"),
                maxcubesize = 256, bg = "white",
                fonts = getOption("X11fonts"), res = NA)
    .Internal(X11(paste("png::", filename, sep=""),
                  width, height, pointsize, gamma,
                  colortype, maxcubesize, bg, bg, fonts, res))

jpeg <- function(filename = "Rplot%03d.jpeg",
                 width=480, height=480, pointsize=12,
                 quality = 75,
                 gamma = 1, colortype = getOption("X11colortype"),
                 maxcubesize = 256, bg = "white",
                 fonts = getOption("X11fonts"), res = NA)
    .Internal(X11(paste("jpeg::", quality, ":", filename, sep=""),
                  width, height, pointsize, gamma,
                  colortype, maxcubesize, bg, bg, fonts, res))
