load <- function(file,envir = parent.frame())
    .Internal(load(file,envir))

save <- function(..., list = character(0), file = "", ascii = FALSE,
	         version = NULL, envir = parent.frame())
{
    names <- as.character( substitute( list(...)))[-1]
    list<- c(list, names)
    invisible(.Internal(save(list, file, ascii, version, envir)))
}

save.image <- function (file = ".RData")
    save(list = ls(all.names = TRUE), file = file, envir = .GlobalEnv)
