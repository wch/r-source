## For examples skipped in testing because they are 'random'

set.seed(1)
if(.Platform$OS.type == "windows") options(pager = "console")

pdf("reg-examples1.pdf", encoding = "ISOLatin1.enc")


## base
example(Cstack_info)
example(DateTimeClasses)
example(Dates)
example(Ops.Date)
example(Random)
example(Sys.getpid)
example(Sys.sleep)
example(Sys.time)
example(as.POSIXlt)
example(difftime)
example(format.Date)
example(Reduce) # funprog.Rd
example(gc)
example(memory.profile)
paste("Today is", date()) # from paste.Rd
trunc(Sys.time(), "day") # from round.POSIXt.Rd
example(srcref)
example(strptime)
example(sys.parent)
example(system.time)
example(tempfile)
example(weekdays)
library(help="splines")

## for example(NA)
if(require("microbenchmark")) {
  x <- c(NaN, 1:10000)
  print(microbenchmark(any(is.na(x)), anyNA(x)))
} else { ## much less accurate
  x <- c(NaN, 1e6)
  nSim <- 2^13
  print(rbind(is.na = system.time(replicate(nSim, any(is.na(x)))),
              anyNA = system.time(replicate(nSim, anyNA(x)))))
}

## utils
example(news)
example(sessionInfo)

## datasets
example(JohnsonJohnson)
example(ability.cov)
example(npk)

## grDevices
if(.Platform$OS.type == "windows") {
    example(windowsFonts)
} else {
    example(X11Fonts)
    example(quartzFonts)
}

library(tools)
example(Rdutils)
example(fileutils)
## results are location- and OS-specific
example(parseLatex) # charset-specific

## part of example(buildVignettes) at one time
gVigns <- pkgVignettes("grid")
str(gVigns) # contains paths

vind <- system.file(package = "grid", "doc", "index.html")
if(nzchar(vind)) { # so vignettes have been installed
    `%=f=%` <- function(a, b) normalizePath(a) == normalizePath(b)
    with(gVigns,
         stopifnot(engines == "utils::Sweave",
                   pkgdir %=f=% system.file(package="grid"),
                   dir    %=f=% system.file(package = "grid", "doc"),
                   (n. <- length(docs)) >= 12, # have 13
                   n. == length(names), n. == length(engines),
                   length(msg) == 0) ) # as it is a 'base' package
    stopifnot("grid" %in% gVigns$names, inherits(gVigns, "pkgVignettes"))
}

proc.time()
