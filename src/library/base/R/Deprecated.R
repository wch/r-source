###----- NOTE:	../man/base-deprecated.Rd   must be synchronized with this file!
###		-------------------------
.Deprecated <- function(new, package = NULL, msg) {
    if( missing(msg) ) {
       msg <- gettextf("'%s' is deprecated.\n",
		    as.character(sys.call(sys.parent())[[1]]))
      if(!missing(new))
	msg <- c(msg, gettextf("Use '%s' instead.\n", new))
      msg <- c(msg,
	     if(!is.null(package))
	     gettextf("See help(\"Deprecated\") and help(\"%s-deprecated\").",
		      package)
	     else gettext("See help(\"Deprecated\")"))
    }
    else msg = as.character(msg)
    warning(paste(msg, collapse=""), call. = FALSE, domain = NA)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 2.5.0
Sys.putenv <- function(...) {
    .Deprecated("Sys.setenv")
    Sys.setenv(...)
}
## </entry>
