
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
## 0.63 : 37

## This can be useful:	Which of the  builtin functions are "primitive" ?
is.primitive <- function(obj)  is.function(obj) && is.null(args(obj))

is.ALL <- function(obj, func.names = ls(pos=length(search())),
		   not.using = c("is.single", "is.na.data.frame",
                   "is.na.POSIXlt",
		   "is.loaded", "is.empty.model", "is.R", "is.element"),
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
	if(debug) cat(f,"")
	fn <- get(f)
	rr <-
	    if(any(f == c("is.na", "is.nan")) &&
	       ((!is.atomic(obj) && !is.list(obj)) || is.null(obj)))
		NA
	    else
		if(is.primitive(fn) || length(formals(fn))) fn(obj) else fn()
	if(!is.logical(rr)) cat("f=",f," --- rr	 is NOT logical	 = ",rr,"\n")
	##if(1!=length(rr))   cat("f=",f," --- rr NOT of length 1; = ",rr,"\n")
	if(true.only && length(rr)==1 && !is.na(rr) && rr) r <- c(r, f)
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
    if(is.list(r)) {
	nm <- format(names(r))
	rr <- lapply(r, symnum)
	for(i in seq(along=r)) cat(nm[i],":",rr[[i]],"\n", ...)
    } else NextMethod("print", ...)
}


is.ALL(NULL)
is.ALL(NULL,   true.only = TRUE)
all.equal(NULL, pairlist())
## list() != NULL == pairlist() :
is.ALL(list(), true.only = TRUE)

(pl <- is.ALL(pairlist(1,    list(3,"A")), true.only = TRUE))
(ll <- is.ALL(    list(1,pairlist(3,"A")), true.only = TRUE))
all.equal(pl[pl != "is.pairlist"],
          ll[ll != "is.vector"])## TRUE

is.ALL(1:5)
is.ALL(array(1:24, 2:4))
is.ALL(1 + 3)
e13 <- expression(1 + 3)
is.ALL(e13)
is.ALL(substitute(expression(a + 3), list(a=1)), true.only = TRUE)
is.ALL(y ~ x)

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
is.ALL(lm.xy, true.only = TRUE)
is.ALL(structure(1:7, names = paste("a",1:7,sep="")))
is.ALL(structure(1:7, names = paste("a",1:7,sep="")), true.only = TRUE)

