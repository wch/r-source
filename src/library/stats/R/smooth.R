## do.ends = TRUE  is compatible with older behavior in R
## --------------  but *NOT*  with Colin Goodalls "smoother" "spl()"

smooth <- function(x, kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"),
                   twiceit = FALSE,
                   endrule = "Tukey", do.ends = FALSE)
{
    if(!is.numeric(x))
	stop("attempt to smooth non-numeric values")
    if(any(is.na(x)))
	stop("attempt to smooth NA values")
    rules <- c("copy","Tukey")#- exact order matters!
    if(is.na(iend <- pmatch(endrule, rules)))
        stop("wrong endrule")
    n <- length(x)
    kind <- match.arg(kind)
    if(substr(kind,1,3) == "3RS" && !do.ends)
        iend <- -iend
    else if(kind == "S")
        iend <- as.logical(do.ends)
    ## same number and type of arguments for all:
    smo <- .C(paste("Rsm", kind, sep="_"),
              as.double(x),
              y = double(n),
              n, iend,
              iter = integer(1),
              DUP=FALSE,
	      PACKAGE = "stats")[c("y","iter")]

    if(any(kind == c("R", "S"))) { # `iter' really was `changed'
        smo$iter <- as.logical(smo$iter)
        names(smo)[names(smo) == "iter"] <- "changed"
    }

    if(twiceit) {
        ## c2 <- match.call() and re-call with twiceit = FALSE
        r <- smooth(x - smo$y, kind = kind, twiceit = FALSE,
                    endrule = endrule, do.ends = do.ends)
        smo$y <- smo$y + r
        if(!is.null(smo$iter))
            smo$iter <- smo$iter + attr(r,"iter")
        if(!is.null(smo$changed))
            smo$changed <- smo$changed || attr(r,"changed")
    }
    if(is.ts(x))
	smo$y <- ts(smo$y, start=start(x), frequency=frequency(x))

    structure(smo$y, kind = kind, twiced = twiceit,
              iter = smo$iter, changed = smo$changed,
              endrule = if(substr(kind,1,1) == "3") rules[iend],
              call = match.call(),
              class = c("tukeysmooth",if(is.ts(x)) "ts"))
}

print.tukeysmooth <- function(x, ...) {
    cat(attr(x,"kind"), "Tukey smoother resulting from ",
	deparse(attr(x, "call")),"\n")
    if(attr(x,"twiced"))		cat(" __twiced__ ")
    if(!is.null(it <- attr(x,"iter")))		cat(" used", it, "iterations\n")
    if(!is.null(ch <- attr(x,"changed")))	cat(if(!ch)"NOT", "changed\n")
    if(length(oldClass(x)) > 1)
	NextMethod()
    else {
	y <- x
	attributes(y) <- NULL
	print(y, ...)
	invisible(x)
    }
}

summary.tukeysmooth <- function(object, ...) {
    cat(attr(object,"kind"), "Tukey smoother resulting from\n",
	deparse(attr(object, "call")),";  n =", length(object),"\n")
    if(attr(object,"twiced"))		cat(" __twiced__ ")
    if(!is.null(it <- attr(object,"iter")))	cat(" used", it, "iterations\n")
    if(!is.null(ch <- attr(object,"changed")))	cat(if(!ch)" NOT", "changed\n")
    if(length(oldClass(object)) > 1)
	NextMethod()
    else {
	y <- object
	attributes(y) <- NULL
	summary(y, ...)
    }
}


