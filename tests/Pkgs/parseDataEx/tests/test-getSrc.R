pkg <- "parseDataEx"

options(keep.source.pkgs     = TRUE, # default: FALSE
        keep.parse.data.pkgs = TRUE, #    "       "
        keep.parse.data = TRUE, keep.source = TRUE)  # just in case

library(pkg, character.only=TRUE)

## This is *NOT* about parse data !!  -------> see ./test-parseD.R <-----  ,  using getParseData()

stopifnot(exprs = {
    is.null(f())
    identical(g(1), TRUE)
    is.null(  g(0))
})

getSrcStuff <- function(x, unique=TRUE, full.names=FALSE, first=TRUE) {
    list(filename = getSrcFilename(x, full.names=full.names, unique=unique),
         dir      = getSrcDirectory(x, unique=unique),
         srcref   = getSrcref(x),
         loc      = {
             whs <- eval(formals(getSrcLocation)$which)
             ## sapply(): all integer(1); keep nms:
             sapply(whs, \(wh) getSrcLocation(x, which=wh, first=first))
         })
}

nms <- ls(paste0("package:", pkg))

srcObj <- lapply(setNames(,nms), get, envir=asNamespace(pkg))
srcAll <- lapply(srcObj, getSrcStuff)

# shows all functions in the original formatting incl comments:
(srcrefs <- lapply(srcAll, `[[`, "srcref"))
(sfiles  <- lapply(srcrefs, attr, "srcfile"))
origs <- lapply(sfiles, `[[`, "original")

stopifnot(exprs = {
    identical(sapply(srcAll, `[[`, "filename"),
              c(f = "a.R", g = "b.R", h = "b.R"))
    sapply(sfiles, is.environment) # all TRUE
    sapply(origs,  is.environment)
    length(unique(origs)) == 1L # the same concatenated file
    class(orig <- origs[[1]]) == c("srcfilecopy", "srcfile")
})
## class(.) does *not* contain "environment" ==> as.list() does *not* work:
str(as.list.environment(orig))

str(srcAll)
## (could test quite a bit more)
