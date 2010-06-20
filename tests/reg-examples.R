## For examples skipped in testing because they are 'random'

if(.Platform$OS.type == "windows") options(pager = "console")

## base
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


## utils
example(packageDescription)
example(sessionInfo)

## stats
example(nlminb)
example(optim)
example(uniroot)

library(tools)
example(Rdutils)
example(fileutils)
## results are location- and OS-specific
example(parseLatex) # charset-specific

## grDevices
if(.Platform$OS.type == "windows") {
    example(windowsFonts)
} else {
    example(X11Fonts)
    example(quartzFonts)
}
