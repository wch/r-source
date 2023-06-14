pkg <- "parseDataEx"

options(keep.source.pkgs     = TRUE, # {default: FALSE}
        keep.parse.data.pkgs = TRUE, # {default: FALSE}
        keep.parse.data = TRUE, keep.source = TRUE)  # just in case

sessionInfo()
library(pkg, character.only=TRUE)
packageDescription(pkg)

## now, the same 123 for all: the same *concatenated* *.R files --> the *same* 1 file
nrow(f.pd <- getParseData(f)) # gave NULL in R <= 4.3.1
nrow(g.pd <- getParseData(g)) # was ok already
nrow(h.pd <- getParseData(h)) # (ditto)

head(f.pd)
str(f.pd)
(eq.f.g <- all.equal(f.pd, g.pd))# one difference: srcfile->filename
stopifnot(exprs = {
    nrow(g.pd) > 120
    identical(nrow(f.pd), nrow(g.pd))
    identical(g.pd, h.pd) # because they are in the *same* source file ../R/b.R
    is.character(eq.f.g)
    length(eq.f.g) == 1
    sapply(c("srcfile", "filename"), grepl, x = eq.f.g, fixed = TRUE)
})
f_srcref <- getSrcref(f)
f_srcfile <- attr(f_srcref, "srcfile")
ls(f_srcfile)
f_srcfile$original
ls(f_srcfile$original)

g_srcfile <- attr(getSrcref(g), "srcfile")

(f.fn <- f_srcfile$filename)
(g.fn <- g_srcfile$filename)

basename3 <- function(f) sub(paste0("^", dirname(dirname(dirname(f)))), "", f)
pkgR <- function(f) paste0("/",pkg,"/R/", f)

basename3(attr(h.pd, "srcfile")$filename)

stopifnot(exprs = {
    identical(attr(f.pd, "srcfile"), f_srcfile)
    is.environment(f_srcfile)
    basename3(f.fn) == pkgR("a.R")
    basename3(g.fn) == pkgR("b.R")
    is.environment(f_srcfile$original)
    "parseData" %in% names(f_srcfile$original)
    "parseData" %in% names(g_srcfile$original)
    ## both `original` are identical {<==> the concatenated *.R files}:
    identical(f_srcfile$original,
              g_srcfile$original)
})
