###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new, package=NULL) {
    msg <- gettextf("'%s' is deprecated.\n",
                    as.character(sys.call(sys.parent())[[1]]))
    if(!missing(new))
        msg <- c(msg, gettextf("Use '%s' instead.\n", new))
    if(!is.null(package))
        msg <- c(msg,
                 gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").", package))
    else msg <- c(msg, gettext("See help(\"Deprecated\")"))
    warning(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 2.1.0
loadURL <- function (url, envir = parent.frame(), quiet = TRUE, ...)
{
    .Deprecated("load(url())")
    tmp <- tempfile("url")
    download.file(url, tmp, quiet = quiet, ...)
    on.exit(unlink(tmp))
    load(tmp, envir = envir)
}
## </entry>
