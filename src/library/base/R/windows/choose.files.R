choose.files <- function(mask = '*.*', caption = 'Select files') {
	list <- .Internal(chooseFiles(mask,caption))
	if (length(list) < 2) list
	else {
		path <- list[1]
		list <- list[-1]
		full <- rep(FALSE, length(list))
		full[grep(':',list)] <- TRUE
		full[substr(list,1,1) %in% c('/','\\')] <- TRUE
		ifelse(full,list,paste(path,list,sep='\\'))
	}
}
