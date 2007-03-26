Sys.getlocale <- function(category = "LC_ALL")
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE",
                                  "LC_MONETARY", "LC_NUMERIC", "LC_TIME",
                                  "LC_MESSAGES", "LC_PAPER", "LC_MEASUREMENT"))
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(Sys.getlocale(category))
}

Sys.setlocale <- function(category = "LC_ALL", locale = "")
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE",
                                  "LC_MONETARY", "LC_NUMERIC", "LC_TIME",
                                  "LC_MESSAGES", "LC_PAPER", "LC_MEASUREMENT"))
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(Sys.setlocale(category, locale))
}

Sys.localeconv <- function() .Internal(Sys.localeconv())
