
ls.base <- ls(pos=length(search()))#- something more elegant ?
base.is.f <- sapply(ls.base, function(x) is.function(get(x)))
bi <- ls.base[base.is.f]
cat("\nNumber of base objects:\t\t", length(ls.base),
    "\nNumber of builtin functions:\t", sum(base.is.f),
    "\n\t starting with 'is.' :\t ",
    length(is.bi <- bi[substring(bi,1,3) == "is."]), "\n")
## 0.14 : 31
## 0.50 : 33
## 0.60 : 34
## 0.62 : 35

## This can be useful:	Which of the  builtin functions are "primitive" ?
is.primitive <- function(obj)  is.function(obj) && is.null(args(obj))

is.ALL <- function(obj, func.names = ls(pos=length(search())),
		   not.using = c("is.single", "is.na.data.frame",
		   "is.loaded", "is.empty.model"),
		   true.only = FALSE, debug = FALSE)
{
    ## Purpose: show many 'attributes' of  R object __obj__
    ## -------------------------------------------------------------------------
    ## Arguments: obj: any R object
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  6 Dec 96, 15:23

    is.fn <- func.names[substring(func.names,1,3) == "is."]
    use.fn <- is.fn[ is.na(match(is.fn, not.using))]

    r <- if(true.only) character(0)
    else structure(vector("list", length= length(use.fn)), names= use.fn)
    for(f in use.fn) {
	if(any(f == c("is.na", "is.finite"))) {
	    if(!is.list(obj) && !is.vector(obj) && !is.array(obj)) {
		if(!true.only) r[[f]] <- NA
		next
	    }
	}
	if(debug) cat(f,"")
	fn <- get(f)
	rr <- if(is.primitive(fn) || length(formals(fn))>0)  fn(obj) else fn()
	if(!is.logical(rr)) cat("f=",f," --- rr	 is NOT logical	 = ",rr,"\n")
	##if(1!=length(rr))   cat("f=",f," --- rr NOT of length 1; = ",rr,"\n")
	if(true.only && length(rr)==1 && rr) r <- c(r, f)
	else if(!true.only) r[[f]] <- rr
    }
    if(debug)cat("\n")
    if(is.list(r)) structure(r, class = "isList") else r
}

print.isList <- function(r, ...)
{
    ## Purpose:	 print METHOD  for  'isList' objects
    ## -------------------------------------------------------------------------
    ## Arguments:
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 12 Mar 97, 15:07
    ## >>>>> needs  cmp.logical <<
    if(is.list(r)) {
	nm <- format(names(r))
	rr <- lapply(r,cmp.logical)
	for(i in seq(along=r)) cat(nm[i],":",rr[[i]],"\n", ...)
    } else NextMethod("print", ...)
}

cmp.logical <- function(log.v, empty.dimnames= 2)
{
    ## Purpose: compact printing of logicals
    ## -------------------------------------------------------------------------
    ## Arguments: log.v : logical vector
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 13 Dec 1996, Sept 98.
    if(!is.logical(log.v)) {
	warning("coercing argument to logical")
	mode(log.v) <- "logical"
    }
    r <- if((lv <- length(log.v))) c(".","|")[ 1+ log.v] else  "()"
    if(is.array(log.v)) {
        dim(r) <- d <- dim(log.v)
        rk <- length(d)
        do.empty <- is.logical(empty.dimnames)
        if(do.empty) {
            do.empty <- empty.dimnames
            empty.dimnames <- if(do.empty) 1:rk else integer(0)
        }
        dimnames(r) <-
            if(!do.empty && !is.null(n <- dimnames(log.v)))
                lapply(n, abbreviate, minlength=1)
            else if(length(empty.dimnames)) {
                n <- vector(mode="list", rk)
                for(i in empty.dimnames)
                    n[[i]] <- rep("", d[i])
                n
            }
    } else if(lv && !is.null(n <- names(log.v)))
        names(r) <- abbreviate(n,minlength=1)

    class(r) <- "noquote"
    r
}


is.ALL(NULL)
##fails: is.ALL(NULL, not.using = c("is.single", "is.loaded"))
is.ALL(NULL,   true.only = TRUE)
is.ALL(list(), true.only = TRUE)

is.ALL(1:5)
is.ALL(array(1:24, 2:4))
is.ALL(1 + 3)
e13 <- expression(1 + 3)
is.ALL(e13)
## fails (0.50-a) [is.loaded]
##   is.ALL(e13, not.using=c("is.single", "is.finite", "is.na"))
is.ALL(y ~ x) #--> (0.49):  NA	for 'is.na' (& is.finite)


is0 <- is.ALL(numeric(0))
is0.ok <- 1 == (lis0 <- sapply(is0, length))
is0[!is0.ok]
is0 <- unlist(is0)
is0
ispi <- unlist(is.ALL(pi))
all(ispi[is0.ok] == is0)

is.ALL(numeric(0), true=TRUE)
is.ALL(array(1,1:3), true=TRUE)
is.ALL(cbind(1:3), true=TRUE)

is.ALL(structure(1:7, names = paste("a",1:7,sep="")))
is.ALL(structure(1:7, names = paste("a",1:7,sep="")), true.only = TRUE)

x <- 1:20 ; y <- 5 + 6*x + rnorm(20) ; lm.xy <- lm(y ~ x)
					#now (0.62) fails: is.ALL(lm.xy)
is.ALL(structure(1:7, names = paste("a",1:7,sep="")))
is.ALL(structure(1:7, names = paste("a",1:7,sep="")), true.only = TRUE)
