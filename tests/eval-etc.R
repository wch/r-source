####  eval / parse / deparse / substitute  etc

set.seed(2017-08-24) # as we will deparse all objects *and* use *.Rout.save
.proctime00 <- proc.time() # start timing

##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Subject: Re: source() / eval() bug ??? (PR#96)
##- Date: 20 Jan 1999 14:56:24 +0100
e1 <- parse(text='c(F=(f <- .3), "Tail area" = 2 * if(f < 1) 30 else 90)')[[1]]
e1
str(eval(e1))
mode(e1)

( e2 <- quote(c(a=1,b=2)) )
names(e2)[2] <- "a b c"
e2
parse(text=deparse(e2))

##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Date: 22 Jan 1999 11:47

( e3 <- quote(c(F=1,"tail area"=pf(1,1,1))) )
eval(e3)
names(e3)

names(e3)[2] <- "Variance ratio"
e3
eval(e3)


##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Date: 2 Sep 1999

## The first failed in 0.65.0 :
attach(list(x=1))
evalq(dim(x) <- 1,as.environment(2))
dput(get("x", envir=as.environment(2)), control="all")

e <- local({x <- 1;environment()})
evalq(dim(x) <- 1,e)
dput(get("x",envir=e), control="all")

### Substitute, Eval, Parse, etc

## PR#3 : "..." matching
## Revised March 7 2001 -pd
A <- function(x, y, ...) {
    B <- function(a, b, ...) { match.call() }
    B(x+y, ...)
}
(aa <- A(1,2,3))
all.equal(as.list(aa),
          list(as.name("B"), a = expression(x+y)[[1]], b = 3))
(a2 <- A(1,2, named = 3)) #A(1,2, named = 3)
all.equal(as.list(a2),
          list(as.name("B"), a = expression(x+y)[[1]], named = 3))

CC <- function(...) match.call()
DD <- function(...) CC(...)
a3 <- DD(1,2,3)
all.equal(as.list(a3),
          list(as.name("CC"), 1, 2, 3))

## More dots issues: March 19 2001 -pd
## Didn't work up to and including 1.2.2

f <- function(...) {
	val <- match.call(expand.dots=FALSE)$...
        x <- val[[1]]
	eval.parent(substitute(missing(x)))
}
g <- function(...) h(f(...))
h <- function(...) list(...)
k <- function(...) g(...)
X <- k(a=)
all.equal(X, list(TRUE))

## Bug PR#24
f <- function(x,...) substitute(list(x,...))
deparse(f(a, b)) == "list(a, b)" &&
deparse(f(b, a)) == "list(b, a)" &&
deparse(f(x, y)) == "list(x, y)" &&
deparse(f(y, x)) == "list(y, x)"

tt <- function(x) { is.vector(x); deparse(substitute(x)) }
a <- list(b=3); tt(a$b) == "a$b" # tends to break when ...


## Parser:
1 <
    2
2 <=
    3
4 >=
    3
3 >
    2
2 ==
    2
## bug till ...
1 !=
    3

all(NULL == NULL)

## PR #656 (related)
u <- runif(1);	length(find(".Random.seed")) == 1

MyVaR <<- "val";length(find("MyVaR")) == 1
rm(MyVaR);	length(find("MyVaR")) == 0


## Martin Maechler: rare bad bug in sys.function() {or match.arg()} (PR#1409)
callme <- function(a = 1, mm = c("Abc", "Bde")) {
    mm <- match.arg(mm); cat("mm = "); str(mm) ; invisible()
}
## The first two were as desired:
callme()
callme(mm="B")
mycaller <- function(x = 1, callme = pi) { callme(x) }
mycaller()## wrongly gave `mm = NULL'  now = "Abc"


## Garbage collection  protection problem:
if(FALSE) ## only here to be run as part of  'make test-Gct'
    gctorture() # <- for manual testing
x <- c("a", NA, "b")
fx <- factor(x, exclude="")
ST <- if(interactive()) system.time else invisible
ST(r <- replicate(20, capture.output(print(fx))))
table(ok. <- r[2,] == "Levels: a b <NA>") # want all TRUE
stopifnot(ok.) # in case of failure, see
r[2,] ## the '<NA>' levels part would be wrong occasionally


## withAutoprint() : must *not* evaluate twice *and* do it in calling environment:
CO <- utils::capture.output
stopifnot(
    identical(
	## ensure it is only evaluated _once_ :
	CO(withAutoprint({ x <- 1:2; cat("x=",x,"\n") }))[1],
	paste0(getOption("prompt"), "x <- 1:2"))
   ,
    ## need "enough" deparseCtrl for this:
    grepl("1L, NA_integer_", CO(withAutoprint(x <- c(1L, NA_integer_, NA))))
   ,
    identical(CO(r1 <- withAutoprint({ formals(withAutoprint); body(withAutoprint) })),
	      CO(r2 <- source(expr = list(quote(formals(withAutoprint)),
					  quote(body(withAutoprint)) ),
			      echo=TRUE))),
    identical(r1,r2)
)
## partly failed in R 3.4.0 alpha
rm(r1,r2) # they fail in parse(.. deparse(..)) below


### Checking parse(* deparse()) "inversion property" ----------------------------
## Hopefully typically the identity():
id_epd <- function(expr, control = c("all","digits17"), ...)
    eval(parse(text = deparse(expr, control=control, ...)))
dPut <- function(x, control = c("all","digits17")) dput(x, control=control)
hasReal <- function(x) {
    if(is.double(x) || is.complex(x))
	any(x != round(x))
    else if(is.logical(x) || is.integer(x) ||
	    is.symbol(x) || is.call(x) || is.environment(x) || is.character(x))
	FALSE
    else if(is.recursive(x)) # recurse :
	any(vapply(x, hasReal, NA))
    else FALSE
}
isMissObj <- function(obj) identical(obj, alist(a=)[[1]])
check_EPD <- function(obj, show = !hasReal(obj)) {
    if(show) dPut(obj)
    if(is.environment(obj) ||
       (is.pairlist(obj) && any(vapply(obj, isMissObj, NA))))
    {
        cat("__ not parse()able __\n")
        return(invisible(obj)) # cannot parse it
    }
    ob2 <- id_epd(obj)
    if(!identical(obj, ob2, ignore.environment=TRUE,
                  ignore.bytecode=TRUE, ignore.srcref=TRUE)) {
        ae <- all.equal(obj, ob2, tolerance = 0)
        cat("not identical(*, ignore.env=T),",
            if(isTRUE(ae)) "but all.equal(*,*, tol = 0)",
            "\n")
        if(!isTRUE(ae)) stop("Not equal: all.equal(*,*, tol = 0) giving\n", ae)
    }
    invisible(obj)
}

xn <- setNames(pi^(1:3), paste0("pi^",1:3))
dPut(xn)
stopifnot(identical(xn, id_epd(xn)))

library(methods)
example(new) # creating t1 & t2 at least

if(require("Matrix")) { cat("Trying some Matrix objects, too\n")
    D5. <- Diagonal(x = 5:1)
    D5N <- D5.; D5N[5,5] <- NA
    example(Matrix)
    example(sparseMatrix)
}

for(nm in ls(env=.GlobalEnv)) {
    cat(nm,": ", sep="")
    if(!any(nm == "r1")) ## 'r1' fails
        check_EPD(obj = (x <- .GlobalEnv[[nm]]))
    if(is.function(x)) {
        cat("checking body(.):\n"   ); check_EPD(   body(x))
        cat("checking formals(.):\n"); check_EPD(formals(x))
    }
    cat("--=--=--=--=--\n")
}
summary(warnings())
## "dput    may be incomplete"
## "deparse may be incomplete"


## at the very end
cat('Time elapsed: ', proc.time() - .proctime00,'\n')
