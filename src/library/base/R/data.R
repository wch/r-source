## Was in `system.unix.R'.  Now system-independent, thanks to Guido's
## .Platform$show.data() idea.
data <-
function (..., list = character(0),
          package = c(.packages(), .Autoloaded),
          lib.loc = .lib.loc, verbose = .Options$verbose)
{
    names <- c(as.character(substitute(list(...))[-1]), list)
    if (!missing(package))
        if (is.name(y <- substitute(package)))
            package <- as.character(y)
    found <- FALSE
    fsep <- .Platform$file.sep
    if (length(names) == 0) {
        if(!missing(package))
            show.data(package, lib.loc)
        else
            show.data(lib.loc = lib.loc)
    } else for (name in names) {
        ## don't make this a single call: list.files() sorts all the
        ## entries. 
        paths <- system.file("data", pkg = package, lib = lib.loc)
        files <- unlist(lapply(paths, FUN = list.files, full = TRUE))
        files <- files[grep(name, files)]
        found <- FALSE
        if (length(files) > 0) {
            subpre <- paste(".*", fsep, sep = "")
            for (file in files) {
                if (verbose)
                    cat("name=", name, ":\t file= ...", fsep,
                        sub(subpre, "", file), "::\t", sep = "")
                if (found)
                    break
                found <- TRUE
                ext <- sub(".*\\.", "", file)
                ## make sure the match is really for `name.ext'
                ## otherwise
                if (sub(subpre, "", file) != paste(name, ".", ext, sep = ""))
                    found <- FALSE
                else switch(ext,
                            R = ,
                            r = source(file, chdir = TRUE),
                            RData = ,
                            rdata = ,
                            rda = load(file, envir = .GlobalEnv),
                            TXT = ,
                            txt = ,
                            tab = assign(name, read.table(file, header = TRUE),
                            env = .GlobalEnv), CSV = ,
                            csv = assign(name,
                            read.table(file, header = TRUE, sep = ";"),
                            env = .GlobalEnv), found <- FALSE)
                if (verbose)
                    cat(if (!found)
                        "*NOT* ", "found\n")
            }
        }
        if (!found)
            warning(paste("Data set `", name, "' not found", sep = ""))
    }
    invisible(names)
}

show.data <-
  function (package = c(.packages(), .Autoloaded), lib.loc = .lib.loc)
{
    ## give `index' of all possible data sets
    file <- tempfile("R.")
    file.create(file)
    first <- TRUE
    nodata <- noindex <- character(0)
    for (lib in lib.loc) for (pkg in package) {
        if(!file.exists(file.path(lib, pkg))) next
        if(!file.exists(file.path(lib, pkg, "data"))) {
            nodata <- c(nodata, pkg)
            next
        }
        INDEX <- system.file("data", "00Index", pkg = pkg, lib = lib)
        if(INDEX == "")
            INDEX <- system.file("data", "index.doc", pkg = pkg, lib = lib)
        if (INDEX != "") {
            cat(paste(ifelse(first, "", "\n"), "Data sets in package `",
                      pkg, "':\n\n", sep = ""), file = file, append = TRUE)
            file.append(file, INDEX)
            first <- FALSE
        } else {
            ## no index: check for datasets
            files <- list.files(system.file("data", pkg = pkg, lib = lib))
            if(length(files) > 0) noindex <- c(noindex, pkg)
        }
    }
    if (first) {
        unlink(file)
        warning("no data listings found")
    } else file.show(file, delete.file = TRUE, title = "R data sets")
    if(!missing(package)) {
        if(length(nodata) > 1)
            warning(paste("packages `", paste(nodata, collapse=", "),
                          "' contain no datasets", sep=""))
        else if(length(nodata) == 1)
            warning(paste("package `", nodata,
                          "' contains no datasets", sep=""))
    }
    if(length(noindex) > 1)
        warning(paste("packages `", paste(noindex, collapse=", "),
                      "' contain datasets but no index", sep=""))
    else if(length(noindex) == 1)
        warning(paste("package `", noindex,
                      "' contains datasets but no index", sep=""))
    invisible(character(0))
}
