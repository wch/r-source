strsplit <- function(x,split)
  .Internal(strsplit(as.character(x),as.character(split)))

substr <- function(x,start,stop)
  .Internal(substr(x,as.integer(start),as.integer(stop)))
substring <- function(text,first,last=1000000)
{
    storage.mode(text) <- "character"
    n <- max(lt <- length(text), length(first), length(last))
    if(lt < n) text <- rep(text, length = n)
    substr(text, first, last)
}

abbreviate <-
    function(names.arg, minlength = 4, use.classes = TRUE, dot = FALSE)
{
    ## we just ignore use.classes
    if(minlength<=0)
	return(rep("",length(names.arg)))
    names.arg <- as.character(names.arg)
    dups <- duplicated(names.arg)
    old <- names.arg
    if(any(dups))
	names.arg <- names.arg[!dups]
    dup2 <- rep(TRUE, length(names.arg))
    x <- these <- names.arg
    repeat {
	ans <- .Internal(abbreviate(these,minlength,use.classes))
	x[dup2] <- ans
	dup2 <- duplicated(x)
	if(!any(dup2))
	    break
	minlength <- minlength+1
	dup2 <- dup2 | match(x, x[duplicated(x)], 0)
	these <- names.arg[dup2]
    }
    if(any(dups))
	x <- x[match(old,names.arg)]
    if(dot)
	x <- paste(x,".",sep="")
    names(x) <- old
    x
}

make.names <- function(names, unique=FALSE)
{
    names <- .Internal(make.names(as.character(names)))
    if(unique) {
	while(any(dups <- duplicated(names))) {
	    names[dups] <- paste(names[dups],
				 seq(length = sum(dups)), sep = "")
	}
    }
    names
}

chartr <- function(old, new, x) .Internal(chartr(old, new, x))
tolower <- function(x) .Internal(tolower(x))
toupper <- function(x) .Internal(toupper(x))
