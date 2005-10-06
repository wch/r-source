###----- NOTE:	../man/base-deprecated.Rd   must be synchronized with this file!
###		-------------------------
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
## Deprecated in 2.3.0

## <entry>
# format.char <- function(x, width = NULL, flag = "-")
# {
#     .Deprecated("format.default")
# }
## </entry>
