checkVignettes <- function(package, dir, lib.loc = NULL,
                           tangle=TRUE, weave=TRUE, usetmpdir=TRUE,
                           keepfiles = !usetmpdir)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument 'package' must be of length 1")
        docdir <- file.path(.find.package(package, lib.loc), "doc")
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file.exists(dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on @code{dir}
            docdir <- file.path(dirname(dir), basename(dir), "inst", "doc")
    }
    
    if(!file.exists(docdir)) return()

    exts <- outer(c("r", "s", "R", "S"), c("nw","tex"), paste, sep="")
    files <- .listFilesWithExts(docdir, exts)
    
    outConn <- textConnection("out", "w")
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    wd <- getwd()
    if(usetmpdir){
        tmpd <- tempfile("Sweave")
        dir.create(tmpd)
        setwd(tmpd)
    }
        
    on.exit({sink(type = "output")
             sink(type = "message")
             setwd(wd)
             if(!keepfiles) unlink(tmpd, recursive=TRUE)
         })

    result <- list(tangle=list(), weave=list(),
                   source=list())

    
    for(f in files){
        if(tangle){
            yy <- try(Stangle(f, quiet=TRUE))
            if(inherits(yy, "try-error"))
                result$tangle[[f]] <- yy
        }
        
        if(weave){
            yy <- try(Sweave(f, quiet=TRUE))
            if(inherits(yy, "try-error"))
                result$weave[[f]] <- yy                
        }
    }

    if(tangle){
        rfiles <- .listFilesWithExts(getwd(), c("r", "s", "R", "S"))
        for(f in rfiles){
            yy <- try(source(f))
            if(inherits(yy, "try-error"))
                result$source[[f]] <- yy                                
        }
    }

    class(result) <- "checkVignettes"
    result
}
    
    
print.checkVignettes <- function(x, ...)
{

    mycat <- function(y, title){    
        if(length(y)>0){
            cat("\n", title, "\n\n", sep="")
            for(k in 1:length(y)){
                cat("File", names(y)[k], ":\n")
                cat(as.character(y[[k]]), "\n")
            }
        }
    }

    mycat(x$weave,  "*** Weave Errors ***")
    mycat(x$tangle, "*** Tangle Errors ***")
    mycat(x$source, "*** Source Errors ***")    
}    
