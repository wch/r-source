dump <-
function (list, fileout = "dumpdata")
{
    digits <- options("digits")
    on.exit(options(digits))
    options(digits = 12)
    .Internal(dump(list, fileout))
}
