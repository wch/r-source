load <- function(file,envir = sys.frame(sys.parent()))
    .Internal(load(file,envir))

save <- function(..., list = character(0), file = "", ascii = FALSE) {
    names <- as.character( substitute( list(...)))[-1]
    list<- c(list, names)
    invisible(.Internal(save( list, file, ascii)))
}

save.image <- function (f = ".RData")
    eval(substitute(save(list = ls(all.names=TRUE), file = f)), .GlobalEnv)
