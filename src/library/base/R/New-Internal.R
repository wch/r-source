##vector <- function(mode = "logical", length = 0).Internal(vector(mode,length))

warning <- function(message = NULL).Internal(warning(message))
restart <- function(on = TRUE).Internal(restart(on))
geterrmessage <- function() .Internal(geterrmessage())
try <- function(expr, first = TRUE)
{
    restart(first)
    if(is.logical(first) && first) {
        first <- FALSE
        expr
    } else
       invisible(structure(.Internal(geterrmessage()), class="try-error"))
}


comment <- function(x).Internal(comment(x))
"comment<-" <- function(x,value).Internal("comment<-"(x,value))

round <- function(x, digits = 0).Internal(round(x,digits))
signif <- function(x, digits = 6).Internal(signif(x,digits))
log <- function(x, base=exp(1))
    if(missing(base)).Internal(log(x)) else .Internal(log(x,base))
log1p <- function(x).Internal(log1p(x))

atan2 <- function(y, x).Internal(atan2(y, x))

beta <- function(a, b).Internal( beta(a, b))
lbeta <- function(a, b).Internal(lbeta(a, b))

gamma <- function(x).Internal( gamma(x))
lgamma <- function(x).Internal(lgamma(x))
digamma <- function(x).Internal(   digamma(x))
trigamma <- function(x).Internal(  trigamma(x))
tetragamma <- function(x).Internal(tetragamma(x))
pentagamma <- function(x).Internal(pentagamma(x))

choose <- function(n,k).Internal(choose(n,k))
lchoose <- function(n,k).Internal(lchoose(n,k))

##-- 2nd part --
D <- function(expr, namevec).Internal(D(expr, namevec))

Machine <- function().Internal(Machine())
R.Version <- function().Internal(Version())
Version <- function() { .Deprecated("R.Version"); R.Version() }
machine <- function().Internal(machine())
colors <- function().Internal(colors())
colours <- .Alias(colors)
commandArgs <- function() .Internal(commandArgs())

args <- function(name).Internal(args(name))

##=== Problems here [[	attr(f, "class") <- "factor"  fails in factor(..)  ]]:
##- attr <- function(x, which).Internal(attr(x, which))
##- "attr<-" <- function(x, which, value).Internal("attr<-"(x, which, value))

cbind <- function(..., deparse.level=1) {
    if(deparse.level != 1) stop("cbind(.) does not accept deparse.level in R.")
    .Internal(cbind(...))
}
rbind <- function(..., deparse.level=1) {
    if(deparse.level != 1) stop("rbind(.) does not accept deparse.level in R.")
    .Internal(rbind(...))
}

dataentry <- function (data, modes) {
    if(!is.list(data) || !length(data) || !all(md <- sapply(data, is.vector)))
        stop("invalid data argument")
    if(!is.list(modes) ||
       (length(modes) && !all(mm <- sapply(modes, is.character))))
        stop("invalid modes argument")
    .Internal(dataentry(data, modes))
}

deparse <-
    function(expr, width.cutoff = 60).Internal(deparse(expr, width.cutoff))


do.call <- function(what,args).Internal(do.call(what,args))
drop <- function(x).Internal(drop(x))
duplicated <- function(x, incomparables = FALSE) {
    if(!is.logical(incomparables) || incomparables)
	stop("duplicated(.. incomparables != FALSE) not yet available in R.")
    .Internal(duplicated(x))
}
format.info <- function(x).Internal(format.info(x))
gc <- function(verbose = getOption("verbose"))
    matrix(.Internal(gc(verbose))/c(1,1,1,1,10,10),2,3,
           dimnames = list(c("Ncells","Vcells"),c("free","total", "(Mb)")))
gcinfo <- function(verbose).Internal(gcinfo(verbose))
gctorture <- function(on=TRUE)invisible(.Internal(gctorture(on)))
gray <- function(level).Internal(gray(level))
grey <- .Alias(gray)

nchar <- function(x).Internal(nchar(x))

##=== FAILS: [	format(pi, dig=2) doesn't work afterwards ]
##- on.exit <- function(expression, add = FALSE) {
##-   if(!is.logical(add) || add)
##-	stop("on.exit(.., add != FALSE) does not yet work in R.")
##-  .Internal(on.exit(expression))
##- }

plot.window <- function(xlim, ylim, log = "", asp = NA, ...)
    .Internal(plot.window(xlim, ylim, log, asp, ...))
polyroot <- function(z).Internal(polyroot(z))
rank <- function(x, na.last = TRUE) {
    if(!is.logical(na.last) || !na.last)
	stop("rank(.., na.last != TRUE) does not yet work in R.")
    .Internal(rank(x))
}
readline <- function(prompt="").Internal(readline(prompt))
search <- function().Internal(search())
searchpaths <- function()
{
    s <- search()
    paths <- lapply(1:length(s), function(i) attr(pos.to.env(i), "path"))
    paths[[length(s)]] <- system.file()
    m <- grep("^package:", s)
    if(length(m)) paths[-m] <- as.list(s[-m])
    unlist(paths)
}

sink <- function(file=NULL, append = FALSE)
    .Internal(sink(file, append))

##-- DANGER ! ---   substitute(list(...))  inside functions !!!
##substitute <- function(expr, env=NULL).Internal(substitute(expr, env))

t.default <- function(x).Internal(t.default(x))
typeof <- function(x).Internal(typeof(x))

unique <- function(x){
    z<-.Internal(unique(x))
    if (is.factor(x))
	z <- factor(z,levels=1:nlevels(x),labels=levels(x))
    z
}

memory.profile <- function().Internal(memory.profile())
