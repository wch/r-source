collater <- function(pkgDir, pkg=basename(pkgDir), lccollate="C",
                     destDir=pkg) {
    cDir <- getwd()
    on.exit(setwd(cDir), add=TRUE)

    ## Attempt to set the LC_COLLATE locale
    curLocale <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", curLocale), add=TRUE)
    if (Sys.setlocale("LC_COLLATE", lccollate) != lccollate)
        stop("Failure in setting LC_COLLATE value")

    ## Check to see if the package exists
    if ((file.exists(pkgDir)) && (file.info(pkgDir)$isdir)) {
        setwd(pkgDir)
        if (file.exists("DESCRIPTION"))
            pkgDesc <- read.dcf("DESCRIPTION")
        else
            stop(paste("Package directory",pkgDir,
                       "does not have a DESCRIPTION file"))

        ## Now check to see if the package has a 'R' directory
        pkgRDir <- "R"
        if ((file.exists(pkgRDir)) && (file.info(pkgRDir)$isdir))
            setwd(pkgRDir)
        else
            stop(paste("Package directory",pkgDir,
                       "does not have a R directory"))
    }
    else
        stop(paste("Package directory",pkgDir,"does not exist"))

    ## Get the full pathname of any valid R file
    rFilePaths <- listFilesWithType(".","code")
    ## rFilePaths will have a './' in front of everything due to
    ## the way it collects the file names.  This will cause problems
    ## later so remove them.  Use sub so that it will only kill the
    ## first instance
    rFilePaths <- sub("\\.\\/","", rFilePaths)

    collationField <- "Collate"

    osField <- paste(collationField,.Platform$OS.type,sep=".")

    if (osField %in% colnames(pkgDesc))
       collationField <- osField

    tempColFile <- tempfile()

    ## If the collation file exists, need to handle that as appropriate
    if (collationField %in% colnames(pkgDesc)) {
        ## The collation file will just have the filenames, not the
        ## file paths, so we need to get those from the direcotry listing
        colFiles <- pkgDesc[,collationField]
        ## colFiles right now is one long string, comma separated -
        ## split it out into individual entries
        colFiles <- strsplit(colFiles,",[[:space:]]*")[[1]]

        ## See which files are listed in the collation file but don't
        ## exist
        bad <- which(!rFilePaths %in% colFiles)
        if (length(bad) > 0) {
                out <- paste("File",rFilePaths[bad],
                             "exists but is not listed in the",
                             collationField,"field",
                             collapse="\n")
                out <- paste("\n",out,sep="")
                stop(out)
           }
        ## See which files exist but aren't listed in the collation file
        bad <- which(!colFiles %in% rFilePaths)
        if (length(bad) > 0) {
                out <- paste("File",colFiles[bad],
                             "is listed in the",
                             collationField,
                             "field but does not exist",
                             collapse="\n")
                out <- paste("\n",out,sep="")
                stop(out)
            }
        collatedLines <- unlist(lapply(colFiles, readLines))
    }
    else {
        ## Collate files by simply catting them all together
        collatedLines <- unlist(lapply(rFilePaths, readLines))
    }
    writeLines(collatedLines,file.path(destDir,pkg))

    ## Success
    invisible()
}
