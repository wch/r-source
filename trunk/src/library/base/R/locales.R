Sys.getlocale <- function(category = "LC_ALL")
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE",
                                  "LC_MONETARY", "LC_NUMERIC", "LC_TIME"))
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(getlocale(category))
}

Sys.setlocale <- function(category = "LC_ALL", locale = "")
{
    category <- match(category, c("LC_ALL", "LC_COLLATE", "LC_CTYPE",
                                  "LC_MONETARY", "LC_NUMERIC", "LC_TIME"))
    if(is.na(category)) stop("invalid 'category' argument")
    .Internal(setlocale(category, locale))
}

Sys.localeconv <- function() .Internal(localeconv())
