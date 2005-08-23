write_PACKAGES <-
function(dir, fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = FALSE)
{
    if(missing(type) && .Platform$OS.type == "windows")
        type <- "win.binary"
    type <- match.arg(type)

    desc <- .build_repository_package_db(dir, fields, type, verbose)

    if(length(desc)) {
        fields <- colnames(desc[[1]])
        desc <- matrix(unlist(desc), ncol = length(fields), byrow = TRUE)
        colnames(desc) <- fields
        ## bundles do not have a Package entry in the DESCRIPTION,
        ## hence replace non-existing Package entries by Bundle entries
        noPack <- is.na(desc[,"Package"])
        desc[noPack, "Package"] <- desc[noPack, "Bundle"]

        ## Writing PACKAGES file from matrix desc linewise in order to
        ## omit NA entries appropriately:
        out <- file(file.path(dir, "PACKAGES"), "wt")
        outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
        for(i in seq(length = nrow(desc))){
            desci <- desc[i, !(is.na(desc[i, ]) | (desc[i, ] == "")),
                          drop = FALSE]
            write.dcf(desci, file = out)
            write.dcf(desci, file = outgz)
        }
        close(out)
        close(outgz)
        invisible(nrow(desc))
    }
    else invisible(0)
}

.build_repository_package_db <-
function(dir, fields = NULL,
         type = c("source", "mac.binary", "win.binary"),
         verbose = getOption("verbose"))
{
    type <- match.arg(type)

    package_pattern <- switch(type,
                              "source" = "\\.tar\\.gz$",
                              "mac.binary" = "\\.tgz$",
                              "win.binary" = "\\.zip$")
    files <- list.files(dir, pattern = package_pattern)

    if(!length(files))
        return(list())
    
    ## Add the standard set of fields required to build a repository's
    ## PACKAGES file:
    fields <- unique(c("Package", "Bundle", "Priority", "Version",
                       "Depends", "Suggests", "Imports", "Contains", 
                       fields))
    packages <- sapply(strsplit(files, "_"), "[", 1)
    files <- file.path(dir, files)
    db <- vector(length(files), mode = "list")
    ## Many (roughly length(files)) warnings are *expected*, hence
    ## suppressed.
    op <- options(warn = -1)
    on.exit(options(op))
    if(verbose) message("Processing packages:")
    if(type == "win.binary") {
        for(i in seq(along = files)) {
            if(verbose) message(paste(" ", files[i]))
            ## for bundles:
            con <- unz(files[i], "DESCRIPTION")
            temp <- try(read.dcf(con, fields = fields), silent = TRUE)
            if(inherits(temp, "try-error")) {
                close(con)
                ## for regular packages:
                con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
                temp <- try(read.dcf(con, fields = fields), silent = TRUE)
                if(inherits(temp, "try-error")) {
                    close(con)
                    next
                }
            }
            db[[i]] <- temp
            close(con)
        }
    } else {
        dir <- file_path_as_absolute(dir)
        files <- list.files(dir, pattern = package_pattern,
                            full.names = TRUE)
        cwd <- getwd()
        td <- tempfile("PACKAGES")
        if(!dir.create(td)) stop("unable to create ", td)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)
        setwd(td)
        for(i in seq(along = files)) {
            if(verbose) message(paste(" ", files[i]))
            p <- file.path(packages[i], "DESCRIPTION")
            temp <- try(system(paste("tar zxf", files[i], p)))
            if(!inherits(temp, "try-error")) {
                temp <- try(read.dcf(p, fields = fields), silent = TRUE)
                if(!inherits(temp, "try-error"))
                    db[[i]] <- temp
            }
            unlink(packages[i], recursive = TRUE)
        }
        setwd(cwd)
    }
    if(verbose) message("done")

    db
}
    
