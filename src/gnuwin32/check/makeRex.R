PKG <- "@PKG@"
RLIB <- "@RLIB@"
exloc <- file.path(RLIB, PKG, "R-ex")
if(!file.exists(exloc)) stop("no examples found")
list.of.files <- list.files(exloc, ".*\\.R")
file <- paste(PKG, "-Ex.R", sep="")
file.create(file)
cat(file=file, append=T, 'attach(NULL, name = ".CheckExEnv")\n',
'assign(".CheckExEnv", pos.to.env(2), pos = length(search()))\n',
'assign("ptime",proc.time(), env=.CheckExEnv)\n')
cat(file=file, append=T,
    paste('postscript("', PKG, '-Examples.ps")\n', sep=""))
cat(file=file, append=T,
    'assign("par.postscript", par(no.readonly = TRUE), env=.CheckExEnv)\n')
cat(file=file, append=T, 'options(contrasts = c(unordered = "contr.treatment", ordered =  "contr.poly"))\n')
cat(file=file, append=T, 'options(pager="console")\n')
if(PKG != "base")
    cat(file=file, append=T, paste('library(', PKG, ', lib.loc="', RLIB, '")', "\n", sep=""))
for(f in list.of.files) {
    cat(file=file, append=T,
        'rm(list = ls(all=TRUE)); .Random.seed <- c(0,rep(7654,3))\n')
    file.append(file, file.path(exloc, f))
    cat(file=file, append=T, 'par(get("par.postscript",env=.CheckExEnv))\n')
    cat(file=file, append=T, 'options(contrasts = c(unordered="contr.treatment", ordered = "contr.poly"))\n')
}
cat(file=file, append=T,
    'cat("Time elapsed: ", proc.time() - get("ptime", env=.CheckExEnv),"\\n")\n')
cat(file=file, append=T, 'dev.off(); quit("no")\n')

