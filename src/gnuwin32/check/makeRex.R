PKG <- "@PKG@"
exloc <- file.path("../../../../library", PKG, "R-ex")
if(!file.exists(exloc)) stop("no examples found")
list.of.files <- list.files(exloc, ".*\\.R")
file <- paste(PKG, "-ex.R", sep="")
file.create(file)
cat(file=file, append=T, '..CheckEnv <- new.env()\n',
'assign("ptime",proc.time(), env=..CheckEnv)\n')
cat(file=file, append=T,
    paste('postscript("', PKG, '-Examples.ps")\n', sep=""))
cat(file=file, append=T,
    'assign("par.postscript", par(no.readonly = TRUE), env=..CheckEnv)\n')
cat(file=file, append=T, 'options(contrasts = c(unordered = "contr.treatment", ordered =  "contr.poly"))\n')
cat(file=file, append=T, 'options(pager="console")\n')
if(PKG != "base")
    cat(file=file, append=T, paste('library(', PKG, ')', "\n"), sep="")
for(f in list.of.files) {
    cat(file=file, append=T,
        'll <- ls(all=TRUE);rm(list= ll[ll != "..CheckEnv"]); .Random.seed <- c(0,rep(7654,3))\n')
    file.append(file, file.path(exloc, f))
    cat(file=file, append=T, 'par(get("par.postscript",env=..CheckEnv))\n')
    cat(file=file, append=T, 'options(contrasts = c(unordered="contr.treatment", ordered = "contr.poly"))\n')
}
cat(file=file, append=T,
    'cat("Time elapsed: ", proc.time() - get("ptime",env=..CheckEnv),"\\n")\n')
cat(file=file, append=T, 'dev.off(); quit("no")\n')

