###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new, package=NULL) {
    msg <- sprintf(gettext("'%s' is deprecated.\n"),
                   as.character(sys.call(sys.parent())[[1]]))
    if(!missing(new))
        msg <- c(msg, sprintf(gettext("Use '%s' instead.\n"), new))
    if(!is.null(package))
        msg <- c(msg,
                 sprintf(gettext("See help(\"Deprecated\") and help(\"%s-deprecated\")."), package))
    else msg <- c(msg, gettext("See help(\"Deprecated\")"))
    warning(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 1.9.0
# tetragamma <- function(x) {
#     .Deprecated("psigamma(*, deriv=2)")
#     psigamma(x, deriv=2)
# }
## </entry>
