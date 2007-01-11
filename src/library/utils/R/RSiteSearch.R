RSiteSearch <- function(string, restrict = c("Rhelp02a","functions","docs"),
                        format = "normal", sortby = "score",
                        matchesPerPage = 20)
{
    string <- paste("http://search.r-project.org/cgi-bin/namazu.cgi?query=",
                    gsub(" ", "+", string), sep = "")
    mpp <- paste("max=", matchesPerPage, sep = "")

    format <- charmatch(format, c("normal", "short"), nomatch = 0)
    if (format == 0) {
        warning("'format' must be \"normal\" or \"short\". Using \"normal\"",
                call. = FALSE, immediate. = TRUE)
        format <- 1
    }
    format <- paste("result=", switch(format, "normal", "short"), sep = "")

    sortby <- charmatch(sortby, c("score", "date:late", "date:early",
                                  "subject", "subject:decending",
                                  "from", "from:decending",
                                  "size", "size:decending"),
                        nomatch = 0)
    if (sortby == 0) {
        warning("wrong 'sortby' specified. Using sortby=\"score\"",
                call. = FALSE, immediate. = TRUE)
        sortby <- 1
    }
    sortby <- paste("sort=",
                    switch(sortby, "score", "date:late", "date:early",
                           "field:subject:ascending",
                           "field:subject:decending",
                           "field:from:ascending",
                           "field:from:decending",
                           "field:size:ascending",
                           "field:size:decending"),
                    sep = "")

    res <- pmatch(restrict, c("Rhelp02a", "Rhelp01", "functions","docs"),
                  nomatch = 0)
    if (all(res) == 0) {
        warning(gettextf("wrong restriction specified.\nUsing '%s'",
                         'c("Rhelp02a", "functions", "docs")'),
                call. = FALSE, immediate. = TRUE, domain = NA)
        res <- c(1, 3, 4)
    }
    res <- paste(c("idxname=Rhelp02a", "idxname=Rhelp01",
                   "idxname=functions", "idxname=docs")[res],
                 collapse = "&")

    qstring <- paste(string, mpp, format, sortby, res, sep = "&")
    browseURL(qstring)
    message(gettextf("A search query has been submitted to %s",
            "http://search.r-project.org"), domain = NA)
    message("The results page should open in your browser shortly")
    invisible(qstring)
}
