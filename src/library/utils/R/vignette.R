vignette <-
function(topic, package = NULL, lib.loc = NULL)
{
    if(is.null(package))
        package <- .packages(all.available = TRUE, lib.loc)
    paths <- .find.package(package, lib.loc)

    ## Find the directories with a 'doc' subdirectory *possibly*
    ## containing vignettes.

    paths <- paths[tools::file_test("-d", file.path(paths, "doc"))]

    vignettes <-
        lapply(paths,
               function(dir) {
                   tools::list_files_with_type(file.path(dir, "doc"),
                                               "vignette")
               })

    if(!missing(topic)) {
        topic <- topic[1]               # Just making sure ...
        vignettes <- as.character(unlist(vignettes))
        idx <-
            which(tools::file_path_sans_ext(basename(vignettes)) == topic)
        if(length(idx)) {
            f <- sub("\\.[[:alpha:]]+$", ".pdf", vignettes[idx])
            f <- f[tools::file_test("-f", f)]
            if(length(f) > 1) {
                ## <FIXME>
                ## Should really offer a menu to select from.
                f <- f[1]
                warning(paste("vignette ", sQuote(topic),
                              " found more than once,\n",
                              "using the one found in ",
                              sQuote(dirname(f)),
                              sep = ""),
                        call. = FALSE)
                ## </FIXME>
            }
            if(length(f)) {
                ## <FIXME>
                ## Should really abstract this into a BioC style
                ## openPDF() along the lines of browseURL() ...
                if(.Platform$OS == "windows")
                    shell.exec(f)
                else
                    system(paste(Sys.getenv("R_PDFVIEWER"), f, "&"))
                ## </FIXME>
            }
            else
                warning(paste("vignette", sQuote(topic), "has no PDF"),
                        call. = FALSE)
        }
        else
            warning(paste("vignette", sQuote(topic), "*not* found"),
                    call. = FALSE)
    }

    if(missing(topic)) {
        ## List all possible vignettes.

        vDB <- matrix(character(0), nr = 0, nc = 4)
        colnames(vDB) <- c("Dir", "File", "Title", "PDF")

        for(db in vignettes[sapply(vignettes, length) > 0]) {
            dir <- dirname(dirname(db[1]))
            entries <- NULL
            ## Check for new-style 'Meta/vignette.rds' ...
            if(file.exists(INDEX <-
                           file.path(dir, "Meta", "vignette.rds")))
                entries <- .readRDS(INDEX)
            if(NROW(entries) > 0)
                vDB <-
                    rbind(vDB,
                          cbind(Dir = I(dir),
                                entries[c("File", "Title", "PDF")]))
        }

        ## Now compute info on available PDFs ...
        title <- if(NROW(vDB) > 0) {
            paste(vDB[, "Title"],
                  paste(rep.int("(source", NROW(vDB)),
                        ifelse(vDB[, "PDF"] != "", ", pdf", ""),
                        ")",
                        sep = ""))
        }
        else
            character()
        ## ... and rewrite into the form used by packageIQR.
        db <- cbind(Package = basename(vDB[, "Dir"]),
                    LibPath = dirname(vDB[, "Dir"]),
                    Item = tools::file_path_sans_ext(basename(vDB[, "File"])),
                    Title = title)

        y <- list(type = "vignette", title = "Vignettes", header = NULL,
                  results = db, footer = NULL)
        class(y) <- "packageIQR"
        return(y)
    }
}
