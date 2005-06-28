write_PACKAGES <-
    function(dir, fields,
             type = c("source", "mac.binary", "win.binary"),
             verbose = FALSE)
{
    if(missing(type) && .Platform$OS.type == "windows") type <- "win.binary"
    type <- match.arg(type)
    files <- list.files(dir,
                        pattern = switch(type,
                        "source" = "\\.tar\\.gz$",
                        "mac.binary" = "\\.tgz$",
                        "win.binary" = "\\.zip$")
                        )
    if(length(files)) {
        ## Standard set of fields required to build a
        ## repository's PACKAGES file:
        if(missing(fields))
            fields <- c("Package", "Bundle", "Priority", "Version",
                        "Depends", "Suggests", "Imports", "Contains")
        packages <- sapply(strsplit(files, "_"), "[", 1)
        files <- file.path(dir, files)
        desc <- vector(length(files), mode = "list")
        ## many (roughly length(files))
        ## warnings are *expected*, hence suppressed
        op <- options(warn = -1)
        if(verbose) cat(gettext("Processing packages:\n"))
        if(type == "win.binary") {
            for(i in seq(along = files)) {
                if(verbose) cat("  ", files[i], "\n", sep ="")
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
                desc[[i]] <- temp
                close(con)
            }
        } else {
            dir <- file_path_as_absolute(dir)
            files <- list.files(dir,
                                pattern=switch(type,
                                "source" = "\\.tar\\.gz$",
                                "mac.binary" = "\\.tgz$"),
                                full.names=TRUE)
            cwd <- getwd()
            td <- tempfile("PACKAGES")
            if(!dir.create(td)) stop("unable to create ", td)
            on.exit(unlink(td, recursive = TRUE))
            setwd(td)
            for(i in seq(along = files)) {
                if(verbose) cat("  ", files[i], "\n", sep ="")
                p <- file.path(packages[i], "DESCRIPTION")
                temp <- try(system(paste("tar zxf", files[i], p)))
                if(!inherits(temp, "try-error")) {
                    temp <- try(read.dcf(p, fields = fields), silent = TRUE)
                    if(!inherits(temp, "try-error"))
                        desc[[i]] <- temp
                }
                unlink(packages[i], recursive = TRUE)
            }
            setwd(cwd)
        }
        if(verbose) cat(gettext("done\n"))
        options(op)                  # change warning level back again
        desc <- matrix(unlist(desc), ncol = length(fields), byrow = TRUE)
        colnames(desc) <- fields
        ## bundles do not have a Package entry in the DESCRIPTION,
        ## hence replace non-existing Package entries by Bundle entries
        noPack <- is.na(desc[,"Package"])
        desc[noPack, "Package"] <- desc[noPack, "Bundle"]

        ## Writing PACKAGES file from matrix desc linewise in order to omit
        ## NA entries appropriately:
        out <- file(file.path(dir, "PACKAGES"), "wt")
        for(i in seq(length = nrow(desc)))
            write.dcf(desc[i, !(is.na(desc[i, ]) | (desc[i, ] == "")),
                           drop = FALSE], file = out)
        close(out)
        invisible(nrow(desc))
    }
    else invisible(0)
}
