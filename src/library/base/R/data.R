data <-
function(..., list = character(0),
         package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"), envir = .GlobalEnv)
{
    fileExt <- function(x) sub(".*\\.", "", x)

    names <- c(as.character(substitute(list(...))[-1]), list)

    ## Find the directories of the given packages and maybe the working
    ## directory.
    if(!is.null(package)) {
        if(any(package %in% "base"))
            warning("datasets have been moved from package ",
                    sQuote("base"), " to package ", sQuote("datasets"))
        if(any(package %in% "stats"))
           warning("datasets have been moved from package ",
                    sQuote("stats"), " to package ", sQuote("datasets"))
        package[package %in% c("base", "stats")] <- "datasets"
    }
    paths <- .find.package(package, lib.loc, verbose = verbose)
    if(is.null(lib.loc))
        paths <- c(.path.package(package, TRUE),
                   if(is.null(package)) getwd(),
                   paths)
    paths <- unique(paths[file.exists(paths)])

    ## Find the directories with a 'data' subdirectory.
    paths <- paths[tools::file_test("-d", file.path(paths, "data"))]
    ## Earlier versions remembered given packages with no 'data'
    ## subdirectory, and warned about them.

    dataExts <- tools:::.make_file_exts("data")

    if(length(names) == 0) {
        ## List all possible data sets.

        ## Build the data db.
        db <- matrix(character(0), nr = 0, nc = 4)
        noindex <- character(0)
        for(path in paths) {
            entries <- NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if(tools::file_test("-f",
                                    file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds', then for '00Index'.
            ## Earlier versions also used to check for 'index.doc'.
            if(tools::file_test("-f",
                                INDEX <-
                                file.path(path, "Meta", "data.rds"))) {
                entries <- .readRDS(INDEX)
            }
            else if(tools::file_test("-f",
                                     INDEX <-
                                     file.path(path, "data", "00Index")))
                entries <- read.00Index(INDEX)
            else {
                ## No index: check whether subdir 'data' contains data
                ## sets.  Easy if data files were not collected into a
                ## zip archive ... in any case, as data sets found are
                ## available for loading, we also list their names.
                dataDir <- file.path(path, "data")
                entries <- tools::list_files_with_type(dataDir, "data")
                if((length(entries) == 0)
                   && all(tools::file_test("-f",
                                           file.path(dataDir,
                                                     c("Rdata.zip",
                                                       "filelist"))))) {
                    entries <- readLines(file.path(dataDir, "filelist"))
                    entries <- entries[fileExt(entries) %in% dataExts]
                }
                if(length(entries) > 0) {
                    entries <-
                        unique(tools::file_path_sans_ext(basename(entries)))
                    entries <- cbind(entries, "")
                }
                else
                    noindex <- c(noindex, packageName)
            }
            if(NROW(entries) > 0) {
                db <- rbind(db,
                            cbind(packageName, dirname(path),
                                  entries))
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        if(length(noindex) > 0) {
            if(!missing(package) && (length(package) > 0)) {
                ## Warn about given packages which do not have a data
                ## index.
                packagesWithNoIndex <- package[package %in% noindex]
                if(length(packagesWithNoIndex) > 0)
                    warning(paste("packages with data sets",
                                  "but no index:",
                                  paste(sQuote(packagesWithNoIndex),
                                        collapse = ",")))
            }
        }

        footer <- if(missing(package))
            paste("Use ",
                  sQuote(paste("data(package =",
                               ".packages(all.available = TRUE))")),
                  "\n",
                  "to list the data sets in all *available* packages.",
                  sep = "")
        else
            NULL
        y <- list(title = "Data sets", header = NULL, results = db,
                  footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    paths <- file.path(paths, "data")
    for(name in names) {
	if (name == "CO2") name <- "zCO2"
        found <- FALSE
        for(p in paths) {
            ## does this package have "Rdata" databases?
            if(tools::file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- .readRDS(file.path(p, "Rdata.rds"))
                if(name %in% names(rds)) {
                    ## found it, so copy objects from database
                    found <- TRUE
                    if(verbose)
                        cat("name=", name, ":\t found in Rdata.rdb\n")
                    thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                    thispkg <- sub("_.*$", "", thispkg) # versioned installs.
                    thispkg <- paste("package:", thispkg, sep="")
                    objs <- rds[[name]] # guaranteed an exact match
                    lazyLoad(file.path(p, "Rdata"), envir = envir,
                             filter = function(x) x %in% objs)
                    break
                }
            }
            ## check for zipped data dir
            if(tools::file_test("-f", file.path(p, "Rdata.zip"))) {
                if(tools::file_test("-f",
                                    fp <- file.path(p, "filelist")))
                    files <- file.path(p, scan(fp, what="", quiet = TRUE))
                else {
                    warning(paste(sQuote("filelist"), "is missing for dir",
                                  sQuote(p)))
                    next
                }
            } else {
                files <- list.files(p, full = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if(length(files) > 1) {
                ## more than one candidate
                o <- match(fileExt(files), dataExts, nomatch = 100)
                paths0 <- dirname(files)
                paths0 <- factor(paths0, levels=paths0)
                files <- files[order(paths0, o)]
            }
            if(length(files) > 0) {
                ## have a plausible candidate (or more)
                for(file in files) {
                    if(verbose)
                        cat("name=", name, ":\t file= ...",
                            .Platform$file.sep, basename(file), "::\t",
                            sep = "")
                    ext <- fileExt(file)
                    ## make sure the match is really for 'name.ext'
                    if(basename(file) != paste(name, ".", ext, sep = ""))
                        found <- FALSE
                    else {
                        found <- TRUE
                        zfile <- zip.file.extract(file, "Rdata.zip")
                        if(zfile != file) on.exit(unlink(zfile))
                        switch(ext,
                               R = , r =
                               sys.source(zfile, chdir = TRUE,
                                          envir = envir),
                               RData = , rdata = , rda =
                               load(zfile, envir = envir),
                               TXT = , txt = , tab =
                               assign(name,
                                      read.table(zfile, header = TRUE),
                                      envir = envir),
                               CSV = , csv =
                               assign(name,
                                      read.table(zfile, header = TRUE,
                                                 sep = ";"),
                                      envir = envir),
                               found <- FALSE)
                    }
                    if (found) break # from files
                }
                if(verbose) cat(if(!found) "*NOT* ", "found\n")
            }
            if (found) break # from paths
        }

        if(!found)
            warning(paste("Data set", sQuote(name), "not found"))
    }
    invisible(names)
}
