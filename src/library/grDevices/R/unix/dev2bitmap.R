dev2bitmap <- function(file, type="png256", height=6, width=6, res=72,
                       units = "in", pointsize, ...,
                       method = c("postscript", "pdf"))
{
    if(missing(file)) stop("'file' is missing with no default")
    if(!is.character(file) || length(file) != 1 || !nzchar(file))
        stop("'file' must be a non-empty character string")
    method <- match.arg(method)
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    height <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * height
    width <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * width
    gsexe <- Sys.getenv("R_GSCMD")
    if(is.null(gsexe) || !nzchar(gsexe)) {
        gsexe <- "gs"
        rc <- system(paste(shQuote(gsexe), "-help > /dev/null"))
        if(rc != 0) stop("sorry, 'gs' cannot be found")
    }
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE)
    st <- grep("^Available", gshelp)
    en <- grep("^Search", gshelp)
    gsdevs <- gshelp[(st+1):(en-1)]
    devs <- c(strsplit(gsdevs, " "), recursive=TRUE)
    if(match(type, devs, 0) == 0)
        stop(gettextf("device '%s' is not available\n", type),
             gettextf("Available devices are %s",
                      paste(gsdevs, collapse="\n")),
             domain = NA)
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    tmp <- tempfile("Rbit")
    on.exit(unlink(tmp))
    din <- graphics::par("din"); w <- din[1]; h <- din[2]
    if(missing(width) && !missing(height)) width <- w/h * height
    if(missing(height) && !missing(width)) height <- h/w * width

    current.device <- dev.cur()
    if(method == "pdf")
        dev.off(dev.copy(device = pdf, file=tmp, width=width,
                         height=height,
                         pointsize=pointsize, paper="special", ...))
    else
        dev.off(dev.copy(device = postscript, file=tmp, width=width,
                         height=height,
                         pointsize=pointsize, paper="special",
                         horizontal=FALSE, ...))
    dev.set(current.device)
    cmd <- paste(shQuote(gsexe), " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                 " -r", res,
                 " -g", ceiling(res*width), "x", ceiling(res*height),
                 " -sOutputFile=", shQuote(file), " ", tmp, sep="")
    system(cmd)
    invisible()
}

bitmap <- function(file, type="png256", height=6, width=6, res=72,
                   units = "in", pointsize, ...)
{
    if(missing(file)) stop("'file' is missing with no default")
    if(!is.character(file) || length(file) != 1 || !nzchar(file))
        stop("'file' must be a non-empty character string")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    height <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * height
    width <- switch(units, "in"=1, "cm"=1/2.54, "mm"=1/25.4, "px"=1/res) * width
    gsexe <- Sys.getenv("R_GSCMD")
    if(is.null(gsexe) || !nzchar(gsexe)) {
        gsexe <- "gs"
        rc <- system(paste(gsexe, "-help > /dev/null"))
        if(rc != 0) stop("sorry, 'gs' cannot be found")
    }
    gshelp <- system(paste(gsexe, "-help"), intern=TRUE)
    st <- grep("^Available", gshelp)
    en <- grep("^Search", gshelp)
    gsdevs <- gshelp[(st+1):(en-1)]
    devs <- c(strsplit(gsdevs, " "), recursive=TRUE)
    if(match(type, devs, 0) == 0)
        stop(gettextf("device '%s' is not available\n", type),
             gettextf("Available devices are %s",
                      paste(gsdevs, collapse="\n")),
             domain = NA)
    if(missing(pointsize)) pointsize <- 1.5*min(width, height)
    cmd <- paste("|", shQuote(gsexe),
                 " -dNOPAUSE -dBATCH -q -sDEVICE=", type,
                 " -r", res,
                 " -g", ceiling(res*width), "x", ceiling(res*height),
                 " -sOutputFile=", shQuote(file), " -", sep="")
    postscript(file=cmd, width=width, height=height,
               pointsize=pointsize, paper="special", horizontal=FALSE, ...)
    invisible()
}
