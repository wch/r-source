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
        vidx <- (tools::file_path_sans_ext(basename(vignettes)) == topic)
        if(any(vidx)) {
            
            pdf <- sub("\\.[[:alpha:]]+$", ".pdf", vignettes)
            pidx <- tools::file_test("-f", pdf)
            ok <- vidx & pidx

            if(any(ok)){
                idx <- min(which(ok))
                if(sum(ok)>1){
                    ## <FIXME>
                    ## Should really offer a menu to select from.
                    warning(paste("vignette ", sQuote(topic),
                                  " found more than once,\n",
                                  "using the one found in ",
                                  sQuote(dirname(pdf[idx])),
                                  sep = ""),
                            call. = FALSE)
                    ## </FIXME>
                }

                z <- list(file=vignettes[idx], pdf=pdf[idx])
            }
            else{
                z <- list(file=vignettes[vidx][1], pdf=character(0))
            }
            z$topic <- topic
            class(z) <- "vignette"
            return(z)
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

print.vignette <- function(x, ...){

    if(length(x$pdf)){
        ## <FIXME>
        ## Should really abstract this into a BioC style
        ## openPDF() along the lines of browseURL() ...
        if(.Platform$OS == "windows")
            shell.exec(x$pdf)
        else
            system(paste(getOption("pdfviewer"), x$pdf, "&"))
        ## </FIXME>
    }
    else{
        warning(paste("vignette", sQuote(x$topic), "has no PDF"),
                call. = FALSE)
    }
}

edit.vignette <- function(name, ...){

    f <- paste(tempfile(name$topic), ".R", sep="")
    Stangle(name$file, output=f, quiet=TRUE)
    file.edit(file=f, ...)
}
