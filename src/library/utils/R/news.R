news <-
function(query, package = "R", lib.loc = NULL,
         format = NULL, reader = NULL, db = NULL)
{
    if(is.null(db)) {
        db <- if(package == "R")
            tools:::.build_news_db_from_R_NEWS()
        else
            tools:::.build_news_db(package, lib.loc, format, reader)
    }
    if(is.null(db))
        return(invisible())
            
    ## Is there a way to directly call/use subset.data.frame?
    ## E.g.,
    ##   subset(db, query)
    ## does not work.
    if(missing(query))
        return(db)

    ## For queries we really need to force Version to package_version
    ## and Date to Date ...
    ## This is tricky because we do not necessarily have valid package
    ## versions (e.g., R NEWS has "2.8.1 patched") or could have the
    ## version info missing (and package_version() does not like NAs).

    ## Manipulate fields for querying (but return the original ones).
    db1 <- db
    ## Canonicalize version entries which *start* with a valid numeric
    ## version.
    version <- db$Version
    pos <- regexpr(sprintf("^%s",
                           .standard_regexps()$valid_numeric_version),
                   version)
    if(any(ind <- (pos > -1L)))
        version[ind] <-
            substring(version[ind], 1L, attr(pos, "match.length")[ind])
    db1$Version <- numeric_version(version, strict = FALSE)
    db1$Date <- as.Date(db$Date)

    r <- eval(substitute(query), db1, parent.frame())
    ## Do something if this is not logical ...
    r <- r & !is.na(r)
    db[r, ]
}

print.news_db <-
function(x, ...)
{
    ## Simple and ugly.
    ## In principle, we could make this output news in the preferred
    ## input format:
    ##   Changes in $VERSION [($DATE)]:
    ##   [$CATEGORY$]
    ##   indented/formatted bullet list of $TEXT entries.
    
    NextMethod("print", x, right = FALSE, row.names = FALSE)
}
