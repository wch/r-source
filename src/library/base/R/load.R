load <- function (file, envir = parent.frame()) {
    if (is.character(file)) {
        if (capabilities("libz"))
            # if zlib is available just open with gzfile, whether
            # file is compressed or not; zlib should work either way.
            con <- gzfile(file, "rb")
        else
            con <- file(file, "rb")
        on.exit(close(con))
    }
    else if (inherits(file, "connection"))
        con <- file
    else stop("bad file argument")

    magic <- readChar(con, 5)

    if (regexpr("RD[AX]2\n", magic) == -1) {
        # Not a verion 2 magic number, so try the old way.
        if (is.character(file)) {
            close(con)
            on.exit()
        }
        else stop("loading from connections not compatible with magic number")
        .Internal(load(file, envir))
    }
    else .Internal(loadFromConn(con, envir))
}

save <- function(..., list = character(0), file = "", ascii = FALSE,
	         version = NULL, envir = parent.frame(), compress = FALSE)
{
    names <- as.character( substitute( list(...)))[-1]
    list<- c(list, names)
    if (! is.null(version) && version == 1)
        invisible(.Internal(save(list, file, ascii, version, envir)))
    else {
        if (is.character(file)) {
            if (file == "") stop("`file' must be non-empty string")
            if (compress) con <- gzfile(file, "wb")
            else con <- file(file, "wb")
            on.exit(close(con))
        }
        else if (inherits(file, "connection"))
            con <- file
        else stop("bad file argument")
        invisible(.Internal(saveToConn(list, con, ascii, version, envir)))
    }
}

save.image <- function (file = ".RData", version = NULL, ascii = FALSE,
                        compress = FALSE) {
    if (! is.character(file) || file == "")
        stop("`file' must be non-empty string")
    save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = file,
         version = version, ascii = ascii, compress = compress,
         envir = .GlobalEnv)
}
