getenv <- function(x)
{
    if (missing(x))
        stop("In this version of R, only getenv(aVar) is implemented\n")
    else
        structure(.Internal(getenv(x)), names = x)
}

help.start <- function(gui = "irrelevant", browser = "irrelevant")
{
    a <- system.file("index.html", pkg="doc/html", lib=R.home())
    if (a == "")
        a <- system.file("index.htm", pkg="doc/html", lib=R.home())
    if (a == "")
        stop("I can't find the html help\n")
    else {
        a <- gsub("/", "\\\\", a)
        cat("If nothing happens, you have to open `",a,"' yourself\n")
        .Internal(help.start());
    }
    invisible("")
}

system <- function(command, intern = FALSE, wait = TRUE, input = "",
                   show.output.on.console = FALSE, minimized = FALSE,
                   invisible = FALSE)
{
    f <- ""
    if (input!="") {
        f <- tempfile()
        on.exit(unlink(f))
        cat(input,file=f,sep="\n")
    }
    if (intern)
        flag <- 3
    else {
        if  (wait)
            flag <- ifelse(show.output.on.console, 2, 1)
        else
            flag <- 0
    }
    if (invisible) flag <- 20 + flag
    else if (minimized) flag <- 10 + flag
    .Internal(system(command, as.integer(flag), f))
}

unix <- function(call, intern = FALSE)
{
    .Deprecated("system")
    system(call,intern)
}

tempfile <- function(pattern = "file") .Internal(tempfile(pattern))

unlink <- function(x) invisible(.Internal(unlink(x)))

flush.console <- function() .Internal(flush.console())

shell <- function(cmd, shell, flag="/c", intern=FALSE,
                  wait=TRUE, translate=FALSE, mustWork=FALSE)
{
    if(missing(shell)) {
        shell <- getenv("R_SHELL")
        if(!nchar(shell)) shell <- getenv("SHELL")
        if(!nchar(shell)) shell <- getenv("COMSPEC")
    }
    if(missing(flag) && any(!is.na(match(c("bash", "tcsh"), shell))))
        flag <- "-c"
    if(translate) cmd <- gsub("/", "\\\\", cmd)
    if(!is.null(shell)) cmd <- paste(shell, flag, cmd)
    res <- system(cmd, intern=intern, wait=wait | intern,
                  show.output.on.console=wait)
    if(!intern && res !=0)
        if(mustWork)
            if(res == -1) stop("cmd could not be run")
            else stop(paste("cmd execution failed with error code", res))
        else
            if(res == -1) warning("cmd could not be run")
            else warning(paste("cmd execution failed with error code", res))
    if(intern) res else invisible(res)
}

zip.file.extract <- function(file, zipname="R.zip")
{
    ofile <- gsub("\\\\", "/", file)
    path <- sub("[^/]*$","", ofile)
    topic <- substr(ofile, nchar(path)+1, 1000)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- sub("[^\\]*$","", tempfile())
        if((unzip <- options()$unzip) != "internal") {
            if(!system(paste(unzip, "-oq",
                             file.path(path, zipname), topic,
                             "-d", tempdir), show = FALSE, invisible = TRUE))
                file <- paste(tempdir,  topic, sep="")
        } else {
            rc <- .Internal(int.unzip(file.path(path, zipname), topic, tempdir))
            if (rc == 10)
                warning(paste(R.home(),
                              "unzip\\unzip32.dll cannot be loaded", sep="\\"))
            if (rc == 0)
                file <- paste(tempdir, topic, sep="")
        }
    }
    file
}
