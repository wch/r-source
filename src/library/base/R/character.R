#nchar <- function(x) {
#	x<-as.character(x)
#	.Internal(nchar(x))
#}

substr <- function(x,start,stop) {
	x<-as.character(x)
	.Internal(substr(x,as.integer(start),as.integer(stop)))
}

strsplit <- function(x,split) {
	x<-as.character(x)
	split<-as.character(split)
	.Internal(strsplit(x,split))
}

substring <- function(text,first,last=1000000)
{
        storage.mode(text) <- "character"
        n <- max(length(text), length(first), length(last))
        text <- rep(text, length = n)
        first <- rep(first, length = n)
        last <- rep(last, length = n)
        substr(text, first, last)
}
abbreviate<-function(names.arg, minlength = 4, use.classes = T, dot = F)
{
        #we just ignore use.classes
        if(minlength<=0)
                return(rep("",length(names.arg)))
        names.arg<-as.character(names.arg)
        dups<-duplicated(names.arg)
        old<-names.arg
        if(any(dups))
                names.arg<-names.arg[!dups]
        dup2<-rep(T,length(names.arg))
        x<-these<-names.arg
        repeat {
                ans<-.Internal(abbreviate(these,minlength,use.classes))
                x[dup2]<-ans
                dup2<-duplicated(x)
                if(!any(dup2))
                        break
                minlength<-minlength+1
                dup2 <- dup2 | match(x, x[duplicated(x)], 0)
                these<-names.arg[dup2]
        }
        if(any(dups))
                x<-x[match(old,names.arg)]
        if(dot)
                x<-paste(x,".",sep="")
        names(x)<-old
        x
}

make.names <- function(names, unique=FALSE)
{
	names <- .Internal(make.names(as.character(names)))
	if(unique) {
		while(any(dups <- duplicated(names))) {
			names[dups] <- paste(names[dups], seq(length = sum(dups)), sep = "")
		}
        }
        names
}
