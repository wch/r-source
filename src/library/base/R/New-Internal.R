geterrmessage <- function() .Internal(geterrmessage())

try <- function(expr, silent = FALSE)
{
    if (! exists("first", inherits = FALSE)) {
        first <- FALSE
        # turn on the restart bit of the current context, push an
        # error handler on the condition handler stack, and push
        # a tryRestart restart on the restart stack
        .Internal(.addTryHandlers())
        if (silent) {
            op <- options("show.error.messages")
            on.exit(options(op))
            options(show.error.messages = FALSE)
        }
        expr
    }
    else invisible(structure(.Internal(geterrmessage()), class = "try-error"))
}


comment <- function(x).Internal(comment(x))
"comment<-" <- function(x,value).Internal("comment<-"(x,value))

round <- function(x, digits = 0).Internal(round(x,digits))
signif <- function(x, digits = 6).Internal(signif(x,digits))
logb <- log <- function(x, base=exp(1))
    if(missing(base)).Internal(log(x)) else .Internal(log(x,base))
log1p <- function(x).Internal(log1p(x))
expm1 <- function(x).Internal(expm1(x))

atan2 <- function(y, x).Internal(atan2(y, x))

beta <- function(a, b).Internal( beta(a, b))
lbeta <- function(a, b).Internal(lbeta(a, b))

gamma <- function(x).Internal( gamma(x))
lgamma <- function(x).Internal(lgamma(x))
digamma <- function(x).Internal(   digamma(x))
trigamma <- function(x).Internal(  trigamma(x))
psigamma <- function(x, deriv=0) .Internal(psigamma(x, deriv))
## tetragamma, pentagamma : deprecated in 1.9.0

factorial <- function(x) gamma(x + 1)
lfactorial <- function(x) lgamma(x + 1)

choose <- function(n,k).Internal(choose(n,k))
lchoose <- function(n,k).Internal(lchoose(n,k))

##-- 2nd part --
# Machine <- function().Internal(Machine())
R.Version <- function().Internal(Version())
commandArgs <- function() .Internal(commandArgs())

args <- function(name).Internal(args(name))

##=== Problems here [[	attr(f, "class") <- "factor"  fails in factor(..)  ]]:
##- attr <- function(x, which).Internal(attr(x, which))
##- "attr<-" <- function(x, which, value).Internal("attr<-"(x, which, value))

cbind <- function(..., deparse.level=1) {
    if(deparse.level != 1) .NotYetUsed("deparse.level != 1")
    .Internal(cbind(...))
}
rbind <- function(..., deparse.level=1) {
    if(deparse.level != 1) .NotYetUsed("deparse.level != 1")
    .Internal(rbind(...))
}

# convert deparsing options to bitmapped integer

.deparseOpts <- function(control) {
    opts <- pmatch(as.character(control), c("keepInteger", "quoteExpressions",
      "showAttributes", "useSource", "warnIncomplete", "all", "delayPromises"))
    if (any(is.na(opts))) stop(paste("deparse options ",
                               paste('"',control[is.na(opts)],'"', sep=""),
     			       collapse=" "), " not recognized", call. = FALSE)
    if (any(opts == 6)) {
	if (length(opts) != 1)
	    stop("all can not be used with other deparse options",
	       	call. = FALSE)
	else
	    return(31)
    } else return(sum(2^(opts-1)))
}

deparse <- function(expr, width.cutoff = 60,
	     backtick = mode(expr) %in% c("call","expression","("),
	     control = "showAttributes") {
    opts <- .deparseOpts(control)
    .Internal(deparse(expr, width.cutoff, backtick, opts))
}

do.call <- function(what,args).Internal(do.call(what,args))
drop <- function(x).Internal(drop(x))
format.info <- function(x, nsmall=0).Internal(format.info(x, nsmall))
gc <- function(verbose = getOption("verbose"))
{
    res <-.Internal(gc(verbose))/c(1, 1, 10, 10, 1, 1, rep(10,4))
    res <- matrix(res, 2, 5,
                  dimnames = list(c("Ncells","Vcells"),
                  c("used", "(Mb)", "gc trigger", "(Mb)", "limit (Mb)")))
    if(all(is.na(res[, 5]))) res[, -5] else res
}
gcinfo <- function(verbose).Internal(gcinfo(verbose))
gctorture <- function(on=TRUE)invisible(.Internal(gctorture(on)))

is.unsorted <- function(x, na.rm = FALSE) {
    if(is.null(x)) return(FALSE)
    if(!is.atomic(x) ||
       (!na.rm && any(is.na(x))))
	return(NA)
    ## else
    if(na.rm && any(ii <- is.na(x)))
	x <- x[!ii]
    .Internal(is.unsorted(x))
}

mem.limits <- function(nsize=NA, vsize=NA)
{
    structure(.Internal(mem.limits(as.integer(nsize), as.integer(vsize))),
              names=c("nsize", "vsize"))
}

nchar <- function(x).Internal(nchar(x))

polyroot <- function(z).Internal(polyroot(z))

readline <- function(prompt="").Internal(readline(prompt))
search <- function().Internal(search())
searchpaths <- function()
{
    s <- search()
    paths <-
        lapply(1:length(s), function(i) attr(as.environment(i), "path"))
    paths[[length(s)]] <- system.file()
    m <- grep("^package:", s)
    if(length(m)) paths[-m] <- as.list(s[-m])
    unlist(paths)
}

sprintf <- function(fmt, ...) .Internal(sprintf(fmt, ...))

##-- DANGER ! ---   substitute(list(...))  inside functions !!!
##substitute <- function(expr, env=NULL).Internal(substitute(expr, env))

t.default <- function(x).Internal(t.default(x))
typeof <- function(x).Internal(typeof(x))


memory.profile <- function() .Internal(memory.profile())

capabilities <- function(what = NULL)
{
    z  <- .Internal(capabilities())
    if(is.null(what)) return(z)
    nm <- names(z)
    i <- pmatch(what, nm)
    if(is.na(i)) logical(0) else z[i]
}

inherits <- function(x, what, which = FALSE)
	.Internal(inherits(x, what, which))

NextMethod <- function(generic=NULL, object=NULL, ...)
    .Internal(NextMethod(generic, object,...))

data.class <- function(x) {
    if (length(cl <- oldClass(x)))
	cl[1]
    else {
	l <- length(dim(x))
	if (l == 2)	"matrix"
	else if (l > 0)	"array"
	else mode(x)
    }
}

is.numeric.factor <- function(x) FALSE
is.integer.factor <- function(x) FALSE

## base has no S4 generics
.noGenerics <- TRUE
