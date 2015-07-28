#  File src/library/stats/R/anova.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## utility for anova.FOO(), FOO in "lmlist", "glm", "glmlist"
## depending on the ordering of the models this might get called with
## negative deviance and df changes.
stat.anova <-
    function(table, test=c("Rao","LRT","Chisq", "F", "Cp"), scale, df.scale, n)
{
    test <- match.arg(test)
    dev.col <- match("Deviance", colnames(table))
    if (test == "Rao") dev.col <- match("Rao", colnames(table))
    if (is.na(dev.col)) dev.col <- match("Sum of Sq", colnames(table))
    switch(test,
	   "Rao" = ,"LRT" = ,"Chisq" = {
               dfs <- table[, "Df"]
               vals <- table[, dev.col]/scale * sign(dfs)
	       vals[dfs %in% 0] <- NA
               vals[!is.na(vals) & vals < 0] <- NA # rather than p = 0
	       cbind(table,
                     "Pr(>Chi)" = pchisq(vals, abs(dfs), lower.tail = FALSE)
                     )
	   },
	   "F" = {
               dfs <- table[, "Df"]
	       Fvalue <- (table[, dev.col]/dfs)/scale
	       Fvalue[dfs %in% 0] <- NA
               Fvalue[!is.na(Fvalue) & Fvalue < 0] <- NA # rather than p = 0
	       cbind(table,
                     F = Fvalue,
		     "Pr(>F)" = pf(Fvalue, abs(dfs), df.scale, lower.tail = FALSE)
                     )
	   },
	   "Cp" = { # depends on the type of object.
               if ("RSS" %in% names(table)) { # an lm object
                   cbind(table, Cp = table[, "RSS"] +
                         2*scale*(n - table[, "Res.Df"]))
               } else { # a glm object
                   cbind(table, Cp = table[, "Resid. Dev"] +
                         2*scale*(n - table[, "Resid. Df"]))
               }
	   })
}

printCoefmat <-
    function(x, digits = max(3L, getOption("digits") - 2L),
	     signif.stars = getOption("show.signif.stars"),
             signif.legend = signif.stars,
	     dig.tst = max(1L, min(5L, digits - 1L)),
	     cs.ind = 1:k, tst.ind = k+1, zap.ind = integer(),
	     P.values = NULL,
	     has.Pvalue = nc >= 4 && substr(colnames(x)[nc], 1, 3) == "Pr(",
             eps.Pvalue = .Machine$double.eps,
	     na.print = "NA", ...)
{
    ## For printing ``coefficient matrices'' as they are in summary.xxx(.) where
    ## xxx in {lm, glm, aov, ..}. (Note: summary.aov(.) gives a class "anova").

    ## By Default
    ## Assume: x is a matrix-like numeric object.
    ## ------  with *last* column = P-values  --iff-- P.values (== TRUE)
    ##	  columns {cs.ind}= numbers, such as coefficients & std.err  [def.: 1L:k]
    ##	  columns {tst.ind}= test-statistics (as "z", "t", or "F")  [def.: k+1]

    if(is.null(d <- dim(x)) || length(d) != 2L)
	stop("'x' must be coefficient matrix/data frame")
    nc <- d[2L]
    if(is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if(!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
	P.values <- has.Pvalue && scp
    }
    else if(P.values && !has.Pvalue)
	stop("'P.values' is TRUE, but 'has.Pvalue' is not")

    if(has.Pvalue && !P.values) {# P values are there, but not wanted
	d <- dim(xm <- data.matrix(x[,-nc , drop = FALSE]))
	nc <- nc - 1
	has.Pvalue <- FALSE
    } else xm <- data.matrix(x)

    k <- nc - has.Pvalue - (if(missing(tst.ind)) 1 else length(tst.ind))
    if(!missing(cs.ind) && length(cs.ind) > k) stop("wrong k / cs.ind")

    Cf <- array("", dim=d, dimnames = dimnames(xm))

    ok <- !(ina <- is.na(xm))
    ## zap before deciding any formats
    for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
    if(length(cs.ind)) {
	acs <- abs(coef.se <- xm[, cs.ind, drop=FALSE])# = abs(coef. , stderr)
	if(any(ia <- is.finite(acs))) {
	    ## #{digits} BEFORE decimal point -- for min/max. value:
	    digmin <- 1 + if(length(acs <- acs[ia & acs != 0]))
		floor(log10(range(acs[acs != 0], finite = TRUE))) else 0
            Cf[,cs.ind] <- format(round(coef.se, max(1L, digits - digmin)),
                                  digits = digits)
        }
    }
    if(length(tst.ind))
	Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
                                digits = digits)
    if(any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if(has.Pvalue) nc))))
	for(i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
    ok[, tst.ind] <- FALSE
    okP <- if(has.Pvalue) ok[, -nc] else ok
    ## we need to find out where Cf is zero.  We can't use as.numeric
    ## directly as OutDec could have been set.
    ## x0 <- (xm[okP]==0) != (as.numeric(Cf[okP])==0)
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if(dec != ".") x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if(length(not.both.0 <- which(x0 & !is.na(x0)))) {
	## not.both.0==TRUE:  xm !=0, but Cf[] is: --> fix these:
	Cf[okP][not.both.0] <- format(xm[okP][not.both.0],
                                      digits = max(1L, digits - 1L))
    }
    if(any(ina)) Cf[ina] <- na.print
    if(P.values) {
        if(!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
	if(any(okP <- ok[,nc])) {
	pv <- as.vector(xm[, nc]) # drop names
	    Cf[okP, nc] <- format.pval(pv[okP],
                                       digits = dig.tst, eps = eps.Pvalue)
	    signif.stars <- signif.stars && any(pv[okP] < .1)
	    if(signif.stars) {
		Signif <- symnum(pv, corr = FALSE, na = FALSE,
				 cutpoints = c(0,  .001,.01,.05, .1, 1),
				 symbols   =  c("***","**","*","."," "))
		Cf <- cbind(Cf, format(Signif)) #format.ch: right=TRUE
	    }
	} else signif.stars <- FALSE
    } else signif.stars <- FALSE
    print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, ...)
    if(signif.stars && signif.legend) {
	if((w <- getOption("width")) < nchar(sleg <- attr(Signif,"legend")))# == 46
	    sleg <- strwrap(sleg, width = w - 2, prefix = "  ")
	##"FIXME": Double space __ is for reproducibility, rather than by design
	cat("---\nSignif. codes:  ", sleg, sep = "",
	    fill = w+4 + max(nchar(sleg,"bytes") - nchar(sleg)))# +4: "---"
    }
    invisible(x)
}

print.anova <- function(x, digits = max(getOption("digits") - 2L, 3L),
                        signif.stars = getOption("show.signif.stars"), ...)
{
    if (!is.null(heading <- attr(x, "heading")))
	cat(heading, sep = "\n")
    nc <- dim(x)[2L]
    if(is.null(cn <- colnames(x))) stop("'anova' object must have colnames")
    has.P <- grepl("^(P|Pr)\\(", cn[nc]) # P-value as last column
    zap.i <- 1L:(if(has.P) nc-1 else nc)
    i <- which(substr(cn,2,7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if(length(i)) zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if(length(i <- grep("Df$", cn))) zap.i <- zap.i[!(zap.i %in% i)]

    printCoefmat(x, digits = digits, signif.stars = signif.stars,
                 has.Pvalue = has.P, P.values = has.P,
                 cs.ind = NULL, zap.ind = zap.i, tst.ind = tst.i,
                 na.print = "", # not yet in print.matrix:  print.gap = 2,
                 ...)
    invisible(x)
}

