#### Regression test of translation not working outside packages for R < 4.1.0.
#### We try French (set in Makefile.common).

### First off, message translation needs to be supported.
if (!capabilities("NLS")) { ## e.g. when R was configured with --disable-nls
    message("no natural language support")
    q("no")
}

#### Report locale and charset
Sys.getlocale()
l10n_info()

#### Skip locales that do not support French (especially C)
OK <- l10n_info()[["UTF-8"]] || l10n_info()[["Latin-1"]]
if (!OK) {
    if(.Platform$OS.type == "windows") {
        OK <- l10n_info()[["codepage"]] == 28605 ## Latin-9
    } else {
        z <- l10n_info()[["codeset"]]
        ## macOS and Solaris have the first, Linux the second.
        OK <- tolower(z) %in% c("iso8859-15", "iso-8859-15", "iso885915")
    }
}
if( !OK ) {
    message("The locale encoding is not known to support French")
    q("no")
}

## Translation domain for a function not in a package: PR#17998
tryCmsg<- function(expr) tryCatch(expr, error = conditionMessage)
chk0 <- function(x) stopifnot(x == 0)
(m1 <- tryCmsg(chk0(1))) # (not translated in R < 4.1.0)
## switch back to English (if possible) for final report.
Sys.setenv(LANGUAGE="en")
m2 <- "x == 0 n'est pas TRUE"
if(m1 != m2) stop("message was not translated to French")
