print <- function(x, ...)UseMethod("print")

##- Need '...' such that it can be called as  NextMethod("print", ...):
print.default <-
function(x,digits=NULL,quote=TRUE,na.print=NULL,print.gap=NULL, ...)
	.Internal(print.default(x,digits,quote,na.print,print.gap))

print.atomic <- function(x,quote=TRUE,...) print.default(x,quote=quote)

print.matrix <- function (x, rowlab = character(0), collab =
                          character(0), quote = TRUE, right = FALSE) {
  x <- as.matrix(x)
  .Internal(print.matrix(x, rowlab, collab, quote, right))
}
prmatrix <- .Alias(print.matrix)

## This should be replaced by  print.anova() [ currently in ./anova.R ], soon..
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
print.simple.list <-
function(x, ...) print(noquote(cbind("_"=unlist(x))), ...)

print.coefmat <-
function(x, digits = max(3, .Options$digits - 2),
         signif.stars= .Options$show.signif.stars,
         dig.tst = max(1, min(5, digits - 1)),
         cs.ind = 1:k, tst.ind = k+1,
         zap.ind = integer(0),
         has.Pvalue = d[2] >= 4 && substr(colnames(x)[d[2]],1,3) == "Pr(", ...)
{
  ## For printing ``coefficient matrices'' as they are in summary.xxx(.) where
  ## xxx in {lm, glm, aov, ..}. (Note: summary.aov(.) gives a class "anova").

  ## By Default
  ## Assume: x is a matrix-like numeric object.
  ## ------  with *last* column = P-values  --iff-- has.Pvalue (== TRUE)
  ##      columns {cs.ind}= numbers, such as coefficients & std.err  [def.: 1:k]
  ##      columns {tst.ind}= test-statistics (as "z", "t", or "F")  [def.: k+1]

  if(is.null(d <- dim(x)) || length(d) != 2)
    stop("1st arg. 'x' must be coefficient matrix/d.f./...")
  k <- d[2] - (if(missing(tst.ind)) 1 else length(tst.ind)) - has.Pvalue
  ##if(!missing(cs.ind)) && length(cs.ind) > k) stop("wrong k / cs.ind")

  Coefs <- array("", dim=d, dimnames = dimnames(x))
  if(length(cs.ind)>0) {
    acs <- abs(coef.se <- x[, cs.ind, drop=FALSE])# = abs(coef. , stderr)
    ## #{digits} BEFORE decimal point -- for min/max. value:
    digmin <- 1+floor(log10(range(acs[acs != 0], na.rm= TRUE)))
    Coefs[, cs.ind] <- format(round(coef.se,max(1,digits-digmin)),digits=digits)
  }
  if(length(tst.ind)>0)
    Coefs[, tst.ind]<- format(round(x[, tst.ind], dig=dig.tst), digits=digits)
  if(length(zap.ind)>0)
    Coefs[, zap.ind]<- format(zapsmall(x[, zap.ind], dig=digits), digits=digits)
  if(has.Pvalue)
    Coefs[, d[2]] <- format.pval(x[, d[2]], digits = dig.tst)
  if(any(r.ind <- !(1:(k+1) %in% c(cs.ind, tst.ind, zap.ind)))) # Remaining ind.
    Coefs[, r.ind] <- format(x[, r.ind], digits=digits)
  if(any(not.both.0 <- (c(x)==0)!=(as.numeric(Coefs)==0),na.rm=TRUE)) {
    ## not.both.0==T:  x !=0, but Coefs[] is: --> fix these:
    Coefs[not.both.0] <- format(x[not.both.0], digits= max(1,digits-1))
  }
  if(!has.Pvalue)
    signif.stars <- FALSE
  else if(signif.stars) {
    Signif <- symnum(x[, d[2]], corr = FALSE,
                     cutpoints = c(0,  .001,.01,.05, .1, 1),
                     symbols   =  c("***","**","*","."," "))
    Coefs <- cbind(Coefs, Signif)
  }
  print(Coefs, quote = FALSE, right = TRUE, ...)
  if(signif.stars) cat("---\nSignif. codes: ",attr(Signif,"legend"),"\n")
  invisible(x)
}

