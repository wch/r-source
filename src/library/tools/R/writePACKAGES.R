write_PACKAGES <- function(dir, fields, type = "winBinary")
{
    type <- match.arg(type)
    files <- list.files(dir, pattern = "\.zip$")
    if(length(files)) {
        ## Standard set of fields required to build a
        ## Windows repository's PACKAGES file:
        if(missing(fields))
            fields <- c("Package", "Bundle", "Priority", "Version",
                        "Depends", "Suggests", "Imports", "Contains")
        packages <- sapply(strsplit(files, "_"), "[", 1)
        files <- file.path(dir, files)
        desc <- vector(length(files), mode = "list")
        ## many (roughly length(files))
        ## warnings are *expected*, hence suppressed
        op <- options(warn = -1)
        for(i in seq(along = files)){
            ## for bundles:
            con <- unz(files[i], "DESCRIPTION")
            temp <- try(read.dcf(con, fields = fields), silent = TRUE)
            if(identical(class(temp), "try-error")) {
                close(con)
                ## for regular packages:
                con <- unz(files[i], file.path(packages[i], "DESCRIPTION"))
                temp <- try(read.dcf(con, fields = fields), silent = TRUE)
                if(identical(class(temp), "try-error")) {
                    close(con)
                    next
                }
            }
            desc[[i]] <- temp
            close(con)
        }
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
