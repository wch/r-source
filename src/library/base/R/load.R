load <- function (file, envir = parent.frame())
{
    if (is.character(file)) {
        ## As zlib is available just open with gzfile, whether file
        ## is compressed or not; zlib works either way.
        con <- gzfile(file)
        on.exit(close(con))
    }
    else if (inherits(file, "connection")) con <- gzcon(file)
    else stop("bad file argument")
    if(!isOpen(con)) {
        ## code below assumes that the connection is open ...
        open(con, "rb")
    }

    magic <- readChar(con, 5)
    if(nchar(magic) == 0) {
        warning("no input is available")
        return(character(0))
    }
    if (regexpr("RD[AX]2\n", magic) == -1) {
        ## a check while we still know the args
        if(regexpr("RD[ABX][12]\r", magic) == 1)
            stop("input has been corrupted, with LF replaced by CR")
        ## Not a version 2 magic number, so try the old way.
        if (is.character(file)) {
            close(con)
            on.exit()
        }
        else stop("the input does not start with a magic number compatible with  loading from a connection")
        .Internal(load(file, envir))
    }
    else .Internal(loadFromConn(con, envir))
}

save <- function(..., list = character(0),
                 file = stop("'file' must be specified"),
                 ascii = FALSE, version = NULL, envir = parent.frame(),
                 compress = FALSE)
{
    opts <- getOption("save.defaults")
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(version)) version <- opts$version

    names <- as.character( substitute( list(...)))[-1]
    list<- c(list, names)
    if (! is.null(version) && version == 1)
        invisible(.Internal(save(list, file, ascii, version, envir)))
    else {
        if (is.character(file)) {
            if (file == "") stop("`file' must be non-empty string")
            if (compress && capabilities("libz")) con <- gzfile(file, "wb")
            else con <- file(file, "wb")
            on.exit(close(con))
        }
        else if (inherits(file, "connection"))
            con <- file
        else stop("bad file argument")
        if(isOpen(con) && summary(con)$text != "binary")
            stop("can only save to a binary connection")
        invisible(.Internal(saveToConn(list, con, ascii, version, envir)))
    }
}

save.image <- function (file = ".RData", version = NULL, ascii = FALSE,
                        compress = FALSE, safe = TRUE) {
    if (! is.character(file) || file == "")
        stop("`file' must be non-empty string")

    opts <- getOption("save.image.defaults")
    if(is.null(opts)) opts <- getOption("save.defaults")

    if (missing(safe) && ! is.null(opts$safe))
        safe <- opts$safe
    if (missing(compress) && ! is.null(opts$compress))
        compress <- opts$compress
    if (missing(ascii) && ! is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(version)) version <- opts$version

    if (safe) {
        ## find a temporary file name in the same directory so we can
        ## rename it to the final output file on success
        outfile <- paste(file, "Tmp", sep = "")
        i <- 0;
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste(file, "Tmp", i, sep = "")
        }
    }
    else outfile <- file

    on.exit(file.remove(outfile))
    save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = outfile,
         version = version, ascii = ascii, compress = compress,
         envir = .GlobalEnv)
    if (safe)
        if (! file.rename(outfile, file)) {
            on.exit()
            stop(paste("image could not be renamed and is left in", outfile))
        }
    on.exit()
}

sys.load.image <- function(name, quiet) {
    if (file.exists(name)) {
        load(name, envir = .GlobalEnv)
        if (! quiet)
	    cat("[Previously saved workspace restored]\n\n")
    }
}

sys.save.image <- function(name)
{
    ## Ensure that there is a reasonable chance that we can open a
    ## connection.
    closeAllConnections()
    save.image(name)
}

loadURL <- function (url, envir = parent.frame(), quiet = TRUE, ...)
{
    tmp <- tempfile("url")
    download.file(url, tmp, quiet = quiet, ...)
    on.exit(unlink(tmp))
    load(tmp, envir = envir)
}
