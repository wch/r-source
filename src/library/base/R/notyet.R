.NotYetImplemented <- function() {
  stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
             "is not implemented yet", sep = ""))
}
