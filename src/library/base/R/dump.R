dump <-
function (list, file = "dumpdata.R", append = FALSE)
{
    digits <- options("digits")
    on.exit(options(digits))
    options(digits = 12)
    .Internal(dump(list, file, append))
}

##dump <- function (list, fileout = "dumpdata") { .Internal(dump(list, fileout)) }

