load <- function(file,envir = sys.frame(sys.parent()))
    .Internal(load(file,envir))

save <- function(..., list = character(0), file = "",
                 ascii = FALSE, oldstyle = FALSE)
{
    if(oldstyle)
        warning("`oldstyle=TRUE' is deprecated and will be removed in R 1.2")
    names <- as.character( substitute( list(...)))[-1]
    list<- c(list, names)
    invisible(.Internal(save(list, file, ascii, oldstyle)))
}

save.image <- function (file = ".RData", oldstyle = FALSE)
    eval(substitute(save(list = ls(all.names = TRUE), file = file,
                         oldstyle = oldstyle)),
         .GlobalEnv)
