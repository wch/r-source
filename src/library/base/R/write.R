write <- function(x, file="data",ncolumns=if(is.character(x)) 1 else 5, append=FALSE)
    cat(x, file=file, sep=c(rep.int(" ",ncolumns-1), "\n"), append=append)
