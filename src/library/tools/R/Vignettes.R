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
        if (!dir.create(tmpd)) stop("unable to create temp directory ",tmpd)
        setwd(tmpd)
    }
    else{
        keepfiles <- TRUE
        if(workdir=="src") setwd(vigns$dir)
    }

    outConn <- textConnection("out", "w")
    sink(outConn, type = "output")
    sink(outConn, type = "message")

    on.exit({
        sink(type = "output")
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
        rfiles <- list_files_with_exts(getwd(), c("r", "s", "R", "S"))
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
            stop(.wrong_args("package", "must be of length 1"))
        docdir <- file.path(.find.package(package, lib.loc), "doc")
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(paste("directory", sQuote(dir), "does not exist"))
        else
            ## maybe perform tilde expansion on @code{dir}
            docdir <- file.path(dirname(dir), basename(dir), "inst", "doc")
    }

    if(!file_test("-d", docdir)) return(NULL)

    docs <- list_files_with_type(docdir, "vignette")

    z <- list(docs=docs, dir=docdir)
    class(z) <- "pkgVignettes"
    z
}

### * buildVignettes
###
### Run a weave and pdflatex on all vignettes of a package and try to
### remove all temporary files that were created.

buildVignettes <-function(package, dir, lib.loc = NULL, quiet=TRUE)
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

        yy <- try(Sweave(f, quiet=quiet))
        if(inherits(yy, "try-error")) stop(yy)
        if(!have.makefile){
            texi2dvi(file=bft, pdf=TRUE, clean=FALSE, quiet=quiet)
        }
    }

    if(have.makefile) {
        yy <- system(Sys.getenv("MAKE"))
        if(yy>0) stop("running make failed")
    }
    else {
        f <- list.files()
        f <- f %w/o% c(pdfs, origfiles)
        file.remove(f)
    }
    invisible(NULL)
}

### * .build_vignette_index

vignetteMetaRE <- function(tag)
    paste("[[:space:]]*%+[[:space:]]*\\\\Vignette", tag,
          "\\{([^}]*)\\}", sep = "")

vignetteInfo <- function(file) {
    lines <- readLines(file)
    ## \VignetteIndexEntry
    vignetteIndexEntryRE <- vignetteMetaRE("IndexEntry")
    title <- grep(vignetteIndexEntryRE, lines, value = TRUE)
    title <- c(gsub(vignetteIndexEntryRE, "\\1", title), "")[1]
    ## \VignetteDepends
    vignetteDependsRE <- vignetteMetaRE("Depends")
    depends <- grep(vignetteDependsRE, lines, value = TRUE)
    depends <- gsub(vignetteDependsRE, "\\1", depends)
    if(length(depends) > 0)
        depends <- unlist(strsplit(depends[1], ", *"))
    ## \VignetteKeyword and old-style \VignetteKeywords
    vignetteKeywordsRE <- vignetteMetaRE("Keywords")
    keywords <- grep(vignetteKeywordsRE, lines, value = TRUE)
    keywords <- gsub(vignetteKeywordsRE, "\\1", keywords)
    keywords <- if(length(keywords) == 0) {
        ## No old-style \VignetteKeywords entries found.
        vignetteKeywordRE <- vignetteMetaRE("Keyword")
        keywords <- grep(vignetteKeywordRE, lines, value = TRUE)
        gsub(vignetteKeywordRE, "\\1", keywords)
    }
    else
        unlist(strsplit(keywords[1], ", *"))
    list(file = file, title = title, depends = depends,
         keywords = keywords)
}

.build_vignette_index <-
function(vignetteDir)
{
    if(!file_test("-d", vignetteDir))
        stop(paste("directory", sQuote(vignetteDir), "does not exist"))
    vignetteFiles <-
        path.expand(list_files_with_type(vignetteDir, "vignette"))

    if(length(vignetteFiles) == 0)
        return(data.frame(File = I(character(0)),
                          Title = I(character(0)),
                          Depends = I(list()),
                          Keywords = I(list()),
                          PDF = I(character())))

    contents <- vector("list", length = length(vignetteFiles) * 4)
    dim(contents) <- c(length(vignetteFiles), 4)
    for(i in seq(along = vignetteFiles))
        contents[i, ] <- vignetteInfo(vignetteFiles[i])
    colnames(contents) <- c("File", "Title", "Depends", "Keywords")

    ## (Note that paste(character(0), ".pdf") does not do what we want.)
    vignettePDFs <- sub("$", ".pdf", file_path_sans_ext(vignetteFiles))

    vignetteTitles <- unlist(contents[, "Title"])

    ## Compatibility code for transition from old-style to new-style
    ## indexing.  If we have @file{00Index.dcf}, use it when computing
    ## the vignette index, but let the index entries in the vignettes
    ## override the ones from the index file.
    if(file_test("-f",
                 INDEX <- file.path(vignetteDir, "00Index.dcf"))) {
        vignetteEntries <- try(read.dcf(INDEX))
        if(inherits(vignetteEntries, "try-error"))
            warning(paste("cannot read index information in file",
                          sQuote(INDEX)))
        else
            vignetteEntries <-
                cbind(colnames(vignetteEntries), c(vignetteEntries))
        pos <- match(basename(vignettePDFs), vignetteEntries[ , 1], 0)
        idx <- which(vignetteTitles == "")
        vignetteTitles[which(pos != 0) & idx] <-
            vignetteEntries[pos, 2][idx]
    }

    vignettePDFs[!file_test("-f", vignettePDFs)] <- ""
    vignettePDFs <- basename(vignettePDFs)

    data.frame(File = I(unlist(contents[, "File"])),
               Title = I(vignetteTitles),
               Depends = I(contents[, "Depends"]),
               Keywords = I(contents[, "Keywords"]),
               PDF = I(vignettePDFs),
               row.names = NULL) # avoid trying to compute row names
}

### * .check_vignette_index

.check_vignette_index <-
function(vignetteDir)
{
    if(!file_test("-d", vignetteDir))
        stop(paste("directory", sQuote(vignetteDir), "does not exist"))
    vignetteIndex <- .build_vignette_index(vignetteDir)
    badEntries <-
        vignetteIndex[grep("^[[:space:]]*$", vignetteIndex[, "Title"]),
                      "File"]
    class(badEntries) <- "check_vignette_index"
    badEntries
}

print.check_vignette_index <-
function(x, ...)
{
    if(length(x) > 0) {
        writeLines(paste("Vignettes with missing or empty",
                         "\\VignetteIndexEntry:"))
        print(basename(file_path_sans_ext(unclass(x))), ...)
    }
    invisible(x)
}


### * .writeVignetteHtmlIndex

.writeVignetteHtmlIndex <- function(pkg, con, vignetteIndex=NULL)
{
    html <- c(paste("<html><head><title>R:", pkg, "vignettes</title>"),
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"../../R.css\">",
              "</head><body>",
              paste("<h2>Vignettes of package", pkg,"</h2>"))

    if(is.null(vignetteIndex) || nrow(vignetteIndex)==0){
        html <- c(html, "Sorry, the package contains no vignette meta-information or index.",
                  "Please browse the <a href=\".\">directory</a>.")
    }
    else{
        html <- c(html, "<dl>")
        for(k in seq(1, nrow(vignetteIndex))){
            html <- c(html,
                      paste("<dt><a href=\"", vignetteIndex[k, "PDF"], "\">",
                            vignetteIndex[k, "PDF"], "</a>:", sep=""),
                      paste("<dd>", vignetteIndex[k, "Title"]))
        }
        html <- c(html, "</dl>")
    }
    html <- c(html, "</body></html>")
    writeLines(html, con=con)
}

vignetteDepends <- function(vignette, recursive=TRUE, reduce=TRUE,
                            local=TRUE, lib.loc=NULL) {
    if (length(vignette) != 1)
        stop("argument 'vignette' must be of length 1")
    if (!file.exists(vignette))
        stop("file: ", sQuote(vignette), " not found.")

    vigDeps <- vignetteInfo(vignette)$depends

    depMtrx <- getVigDepMtrx(vigDeps)
    instPkgs <- utils::installed.packages(lib.loc=lib.loc)
    getDepList(depMtrx, instPkgs, recursive, local, reduce,
               lib.loc)
}

getVigDepMtrx <- function(vigDeps) {
    ## Taken almost directly out of 'package.dependencies'
    if (length(vigDeps) > 0) {
        z <- unlist(strsplit(vigDeps, ",", fixed=TRUE))
        z <- sub("^[[:space:]]*(.*)", "\\1", z)
        z <- sub("(.*)[[:space:]]*$", "\\1", z)
        pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
        depMtrx <- cbind(sub(pat, "\\1", z),
                         sub(pat, "\\2", z),
                         NA)
        noversion <- depMtrx[, 1] == depMtrx[, 2]
        depMtrx[noversion, 2] <- NA
        pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
        depMtrx[!noversion, 2:3] <-
            c(sub(pat, "\\1", depMtrx[!noversion, 2]),
              sub(pat, "\\2", depMtrx[!noversion, 2]))
        depMtrx
    }
    else
        NA
}



### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
