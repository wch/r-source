cat <- function(...,file="",sep=" ", fill=FALSE, labels=NULL,append=FALSE)
    .Internal(cat(list(...),file,sep,fill,labels,append))
