printCoefmat <-
    function(x, digits = max(3, getOption("digits") - 2),
	     signif.stars = getOption("show.signif.stars"),
             signif.legend = signif.stars,
	     dig.tst = max(1, min(5, digits - 1)),
	     cs.ind = 1:k, tst.ind = k+1, zap.ind = integer(0),
	     P.values = NULL,
	     has.Pvalue = nc >= 4 && substr(colnames(x)[nc],1,3) == "Pr(",
             eps.Pvalue = .Machine$double.eps,
	     na.print = "NA", ...)
{
    ## For printing ``coefficient matrices'' as they are in summary.xxx(.) where
    ## xxx in {lm, glm, aov, ..}. (Note: summary.aov(.) gives a class "anova").

    ## By Default
    ## Assume: x is a matrix-like numeric object.
    ## ------  with *last* column = P-values  --iff-- P.values (== TRUE)
    ##	  columns {cs.ind}= numbers, such as coefficients & std.err  [def.: 1:k]
    ##	  columns {tst.ind}= test-statistics (as "z", "t", or "F")  [def.: k+1]

    if(is.null(d <- dim(x)) || length(d) != 2)
	stop("1st arg. 'x' must be coefficient matrix/d.f./...")
    nc <- d[2]
    if(is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if(!is.logical(scp) || is.na(scp)) {
            warning("option ", sQuote("show.coef.Pvalues"),
                    " is invalid: assuming TRUE")
            scp <- TRUE
        }
	P.values <- has.Pvalue && scp
    }
    else if(P.values && !has.Pvalue)
	stop("'P.values is TRUE, but has.Pvalue is not")

    if(has.Pvalue && !P.values) {# P values are there, but not wanted
	d <- dim(xm <- data.matrix(x[,-nc , drop = FALSE]))
	nc <- nc - 1
	has.Pvalue <- FALSE
    } else xm <- data.matrix(x)

    k <- nc - has.Pvalue - (if(missing(tst.ind)) 1 else length(tst.ind))
    if(!missing(cs.ind) && length(cs.ind) > k) stop("wrong k / cs.ind")

    Cf <- array("", dim=d, dimnames = dimnames(xm))

    ok <- !(ina <- is.na(xm))
    if(length(cs.ind)>0) {
	acs <- abs(coef.se <- xm[, cs.ind, drop=FALSE])# = abs(coef. , stderr)
        if(any(is.finite(acs))) {
            ## #{digits} BEFORE decimal point -- for min/max. value:
            digmin <- 1+floor(log10(range(acs[acs != 0], na.rm= TRUE)))
            Cf[,cs.ind] <- format(round(coef.se, max(1,digits-digmin)),
                                  digits=digits)
        }
    }
    if(length(tst.ind)>0)
	Cf[, tst.ind]<- format(round(xm[, tst.ind], dig=dig.tst), digits=digits)
    if(length(zap.ind)>0)
	Cf[, zap.ind]<- format(zapsmall(xm[,zap.ind], dig=digits),digits=digits)
    if(any(r.ind <- !((1:nc) %in% c(cs.ind,tst.ind,zap.ind, if(has.Pvalue)nc))))
	Cf[, r.ind] <- format(xm[, r.ind], digits=digits)
    okP <- if(has.Pvalue) ok[, -nc] else ok
    x0 <- (xm[okP]==0) != (as.numeric(Cf[okP])==0)
    if(length(not.both.0 <- which(x0 & !is.na(x0)))) {
	## not.both.0==TRUE:  xm !=0, but Cf[] is: --> fix these:
	Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits= max(1,digits-1))
    }
    if(any(ina)) Cf[ina] <- na.print
    if(P.values) {
        if(!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option ", sQuote("show.signif.stars"),
                    " is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
	pv <- xm[, nc]
	if(any(okP <- ok[,nc])) {
	    Cf[okP, nc] <- format.pval(pv[okP],
                                       digits = dig.tst, eps = eps.Pvalue)
	    signif.stars <- signif.stars && any(pv[okP] < .1)
	    if(signif.stars) {
		Signif <- symnum(pv, corr = FALSE, na = FALSE,
				 cutpoints = c(0,  .001,.01,.05, .1, 1),
				 symbols   =  c("***","**","*","."," "))
		Cf <- cbind(Cf, format.char(Signif)) #format.ch: right=TRUE
	    }
	} else signif.stars <- FALSE
    } else signif.stars <- FALSE
    print.default(Cf, quote = FALSE, right = TRUE, na.print=na.print, ...)
    if(signif.stars) cat("---\nSignif. codes: ",attr(Signif,"legend"),"\n")
    invisible(x)
}

print.anova <- function(x, digits = max(getOption("digits") - 2, 3),
                        signif.stars = getOption("show.signif.stars"), ...)
{
    if (!is.null(heading <- attr(x, "heading")))
	cat(heading, sep = "\n")
    nc <- dim(x)[2]
    if(is.null(cn <- colnames(x))) stop("anova object must have colnames")
    has.P <- substr(cn[nc],1,3) == "Pr(" # P-value as last column
    zap.i <- 1:(if(has.P) nc-1 else nc)
    i <- which(substr(cn,2,7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if(length(i))
	zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if(length(i <- grep("Df$", cn)))
	zap.i <- zap.i[!(zap.i %in% i)]

    printCoefmat(x, digits = digits, signif.stars = signif.stars,
                 has.Pvalue = has.P, P.values = has.P,
                 cs.ind = NULL, zap.ind = zap.i, tst.ind= tst.i,
                 na.print = "", # not yet in print.matrix:  print.gap = 2,
                 ...)
    invisible(x)
}

