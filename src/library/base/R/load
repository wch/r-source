load <- function(file) 
	.Internal(load(file))

save <- function(..., list = character(0), file = "", ascii = FALSE) {
	names <- as.character( substitute( list(...)))[-1]
	list<- c(list, names)
	invisible(.Internal(save( list, file, ascii)))
}

save.image <- function()
  save(list = ls(), file = ".RData")
