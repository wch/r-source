### * checkVignettes
###
### Run a tangle+source and a weave on all vignettes of a package.

checkVignettes <-
function(package, dir, lib.loc = NULL,
         tangle=TRUE, weave=TRUE,
         workdir=c("tmp", "src", "cur"),
         keepfiles = FALSE)
{
    vigns <- pkgVignettes(package=package, dir=dir, lib.loc=lib.loc)
    if(is.null(vigns)) return(NULL)
    
    workdir <- match.arg(workdir)
    wd <- getwd()
    if(workdir=="tmp"){
        tmpd <- tempfile("Sweave")
        dir.create(tmpd)
        setwd(tmpd)
    }
    else{
        keepfiles <- TRUE
        if(workdir=="src") setwd(vigns$dir)
    }
    
    outConn <- textConnection("out", "w")
    sink(outConn, type = "output")
    sink(outConn, type = "message")
    
    on.exit({sink(type = "output")
             sink(type = "message")
             setwd(wd)
             if(!keepfiles) unlink(tmpd, recursive=TRUE)
         })

    result <- list(tangle=list(), weave=list(), source=list())
    
    for(f in vigns$docs){
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
    
print.checkVignettes <-
function(x, ...)
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

    invisible(x)
}    

### * pkgVignettes
###
### Get an object of class pkgVignettes which contains a list of Sweave
### files and the name of the directory which contains them.

pkgVignettes <- function(package, dir, lib.loc = NULL)
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
        if(!.fileTest("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on @code{dir}
            docdir <- file.path(dirname(dir), basename(dir), "inst", "doc")
    }
    
    if(!.fileTest("-d", docdir)) return(NULL)

    docs <- .listFilesWithType(docdir, "vignette")

    z <- list(docs=docs, dir=docdir)
    class(z) <- "pkgVignettes"
    z
}

### * buildVignettes
###
### Run a weave and pdflatex on all vignettes of a package and try to
### remove all temporary files that were created.

buildVignettes <-function(package, dir, lib.loc = NULL)
{
    vigns <- pkgVignettes(package=package, dir=dir, lib.loc=lib.loc)
    if(is.null(vigns)) return(NULL)
    
    wd <- getwd()
    setwd(vigns$dir)

    on.exit(setwd(wd))

    origfiles <- list.files()
    have.makefile <- "makefile" %in% tolower(origfiles)

    pdfs <- character(0)
    for(f in vigns$docs){
            
        f <- basename(f)
        bf <- sub("\\..[^\\.]*$", "", f)
        bft <- paste(bf, ".tex", sep="")
        pdfs <- c(pdfs, paste(bf, ".pdf", sep=""))
            
        yy <- try(Sweave(f, quiet=TRUE))
        if(inherits(yy, "try-error")) stop(yy)
        if(!have.makefile){
            yy <- system(paste(file.path(R.home(), "bin", "texi2dvi"),
                               "--quiet --pdf", bft))
            if(yy>0)
                stop(paste("running texi2dvi on", bft, "failed"))
        }
    }
    
    if(have.makefile) {
        yy <- system(Sys.getenv("MAKE"))
        if(yy>0) stop("running make failed")
    }
    else {
        f <- list.files()
        f <- f[!(f %in% c(pdfs, origfiles))]
        unlink(f)
    }
    invisible(NULL)
}

### * .buildVignetteIndex

.buildVignetteIndex <-
function(vignetteDir)
{
    if(!.fileTest("-d", vignetteDir))
        stop(paste("directory", sQuote(vignetteDir), "does not exist"))
    vignetteFiles <- .listFilesWithType(vignetteDir, "vignette")
    vignetteIndexEntryRE <-
        "[[:space:]]*%+[[:space:]]*\\\\VignetteIndexEntry\{([^}]*)\}"
    vignetteTitles <-
        sapply(vignetteFiles,
               function(file) {
                   lines <- grep(vignetteIndexEntryRE, readLines(file),
                                 value = TRUE)
                   c(gsub(vignetteIndexEntryRE, "\\1", lines), "")[1]
               })
    vignetteFiles <-
        paste(basename(gsub("\\.[[:alpha:]]+$", "", vignetteFiles)),
              ".pdf", sep = "")
    vignetteIndex <- cbind(vignetteFiles, "")
    ## <FIXME>
    ## Replace this by
    ##   vignetteIndex <- cbind(vignetteFiles, vignetteTitles)
    ## for 1.8.
    ## Compatibility code for transition from old-style to new-style
    ## indexing.  If we have @file{00Index.dcf}, use it when computing
    ## the vignette index, but let the index entries in the vignettes
    ## override the ones from the index file.
    if(.fileTest("-f",
                 INDEX <- file.path(vignetteDir, "00Index.dcf"))) {
        vignetteEntries <- try(read.dcf(INDEX))
        if(inherits(vignetteEntries, "try-error"))
            warning(paste("cannot read index information in file",
                          sQuote(INDEX)))
        else
            vignetteEntries <-
                cbind(colnames(vignetteEntries), c(vignetteEntries))
        idx <- match(vignetteFiles, vignetteEntries[ , 1], 0)
        vignetteIndex[which(idx != 0), 2] <- vignetteEntries[idx, 2]
    }
    idx <- which(vignetteTitles != "")
    vignetteIndex[idx, 2] <- vignetteTitles[idx]
    ## </FIXME>    
    vignetteIndex
}

### * .checkVignetteIndex

.checkVignetteIndex <-
function(vignetteDir)
{
    if(!.fileTest("-d", vignetteDir))
        stop(paste("directory", sQuote(vignetteDir), "does not exist"))
    vignetteIndex <- .buildVignetteIndex(vignetteDir)
    badEntries <-
        vignetteIndex[grep("^[[:space:]]*$", vignetteIndex[, 2]), 1]
    class(badEntries) <- "checkVignetteIndex"
    badEntries
}

print.checkVignetteIndex <-
function(x, ...)
{
    if(length(x) > 0) {
        writeLines(paste("Vignettes with missing or empty",
                         "\\VignetteIndexEntry:"))
        print(gsub("\\.[[:alpha:]]+$", "", unclass(x)), ...)
    }
    invisible(x)
}

### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
