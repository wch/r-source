loadRconsole <- function(file=choose.files(file.path(Sys.getenv("R_USER"), "Rconsole")))
    invisible(.Internal(loadRconsole(file)))
