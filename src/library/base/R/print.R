print <- function(x, ...)UseMethod("print")

##- Need '...' such that it can be called as  NextMethod("print", ...):
print.default <-
    function(x,digits=NULL,quote=TRUE,na.print=NULL,print.gap=NULL, ...)
    .Internal(print.default(x,digits,quote,na.print,print.gap))

print.atomic <- function(x,quote=TRUE,...) print.default(x,quote=quote)

print.matrix <- function (x, rowlab = dn[[1]], collab = dn[[2]],
                          quote = TRUE, right = FALSE,
                          na.print=NULL, print.gap=NULL) {
    x <- as.matrix(x)
    dn <- dimnames(x)
    if(!is.null(print.gap)) warning("'print.gap' is not yet used")
    ## and 'na.print' could be done in .Internal(.) as well:
    if(!is.null(na.print) && any(ina <- is.na(x)))
        x[ina] <- na.print
    .Internal(print.matrix(x, rowlab, collab, quote, right))
}
prmatrix <- .Alias(print.matrix)

## This is not used anymore [replaced by  print.anova() -> ./anova.R ]
print.tabular <-
    function(x, digits = max(3, .Options$digits - 3), na.print = "")
{
    cat("\n", if(!is.null(x$title))
	x$title else "Analysis of Variance:", "\n\n", sep="")
    if(!is.null(x$topnote))
	cat(paste(x$topnote, collapse="\n"), "\n\n", sep="")
    print.default(x$table, digits=digits, na = "", print.gap = 2)
    if(!is.null(x$botnote))
	cat("\n", paste(x$botnote, collapse="\n"), sep="")
    cat("\n")
}

noquote <- function(obj) {
    ## constructor for a useful "minor" class
    if(!inherits(obj,"noquote")) class(obj) <- c(class(obj),"noquote")
    obj
}

"[.noquote" <- function (x, ...) {
    attr <- attributes(x,"legend")
    r <- unclass(x)[...]
    attributes(r) <- c(attributes(r),
		       attr[is.na(match(names(attr),c("dim","dimnames")))])
    r
}

print.noquote <- function(obj,...) {
    ## method for (character) objects of class 'noquote'
    cl <- class(obj)
    if(!is.null(cl)) class(obj) <- cl[cl != "noquote"]
    NextMethod("print", obj, quote = FALSE, ...)
}
## used for version:
print.simple.list <- function(x, ...)
    print(noquote(cbind("_simple.list_"=unlist(x))), ...)

print.coefmat <-
    function(x, digits = max(3, .Options$digits - 2),
	     signif.stars= .Options$show.signif.stars,
	     dig.tst = max(1, min(5, digits - 1)),
	     cs.ind = 1:k, tst.ind = k+1,
	     zap.ind = integer(0),
	     has.Pvalue = d[2] >= 4 && substr(colnames(x)[d[2]],1,3) == "Pr(",
	     ...)
{
    ## For printing ``coefficient matrices'' as they are in summary.xxx(.) where
    ## xxx in {lm, glm, aov, ..}. (Note: summary.aov(.) gives a class "anova").

    ## By Default
    ## Assume: x is a matrix-like numeric object.
    ## ------  with *last* column = P-values  --iff-- has.Pvalue (== TRUE)
    ##	  columns {cs.ind}= numbers, such as coefficients & std.err  [def.: 1:k]
    ##	  columns {tst.ind}= test-statistics (as "z", "t", or "F")  [def.: k+1]

    if(is.null(d <- dim(x)) || length(d) != 2)
	stop("1st arg. 'x' must be coefficient matrix/d.f./...")
    nc <- d[2]
    k <- nc - (if(missing(tst.ind)) 1 else length(tst.ind)) - has.Pvalue
    ##if(!missing(cs.ind)) && length(cs.ind) > k) stop("wrong k / cs.ind")

    Cf <- array("", dim=d, dimnames = dimnames(x))
    xm <- as.matrix(x)
    if(length(cs.ind)>0) {
	acs <- abs(coef.se <- xm[, cs.ind, drop=FALSE])# = abs(coef. , stderr)
	## #{digits} BEFORE decimal point -- for min/max. value:
	digmin <- 1+floor(log10(range(acs[acs != 0], na.rm= TRUE)))
	Cf[,cs.ind] <- format(round(coef.se,max(1,digits-digmin)),digits=digits)
    }
    if(length(tst.ind)>0)
	Cf[, tst.ind]<- format(round(xm[, tst.ind], dig=dig.tst), digits=digits)
    if(length(zap.ind)>0)
	Cf[, zap.ind]<- format(zapsmall(xm[,zap.ind], dig=digits),digits=digits)
    if(any(r.ind <- !((1:nc) %in% c(cs.ind, tst.ind, zap.ind))))#Remaining ind.
	Cf[, r.ind] <- format(xm[, r.ind], digits=digits)
    if(any((not.both.0 <- (c(xm)==0)!=(as.numeric(Cf)==0)),na.rm=TRUE)) {
	## not.both.0==TRUE:  xm !=0, but Cf[] is: --> fix these:
	Cf[not.both.0] <- format(xm[not.both.0], digits= max(1,digits-1))
    }
    if(has.Pvalue) {
	Cf[, nc] <- format.pval(xm[, nc], digits = dig.tst)
        if(signif.stars) {
            Signif <- symnum(xm[, nc], corr = FALSE,
                             cutpoints = c(0,  .001,.01,.05, .1, 1),
                             symbols   =  c("***","**","*","."," "))
            Cf <- cbind(Cf, Signif)
        }
    } else signif.stars <- FALSE
    print.matrix(Cf, quote = FALSE, right = TRUE, ...)
    if(signif.stars) cat("---\nSignif. codes: ",attr(Signif,"legend"),"\n")
    invisible(x)
}

print.anova <- function(x, digits = max(.Options$digits - 2, 3),
			signif.stars= .Options$show.signif.stars, ...)
{
    if (!is.null(heading <- attr(x, "heading")))
        cat(heading, sep = "\n")
    nc <- (d <- dim(x))[2]
    if(is.null(cn <- colnames(x))) stop("anova object must have colnames(.)!")
    has.P <- substr(cn[nc],1,3) == "Pr(" # P-value as last column
    zap.i <- 1:(if(has.P) nc-1 else nc)
    tst.i <- if(length(i <- which(substr(cn,2,7) == " Value"))) {
        zap.i <- zap.i[zap.i != i]
        i
    } else integer(0)
    if(length(i <- which(substr(cn,1,2) == "Df")))
        zap.i <- zap.i[zap.i != i]
    print.coefmat(x, digits=digits, signif.stars=signif.stars, has.Pvalue=has.P,
                  cs.ind = NULL, zap.ind = zap.i, tst.ind= tst.i,
                  na.print = "", # not yet in print.matrix:  print.gap = 2,
                  ...)
    invisible(x)
}


