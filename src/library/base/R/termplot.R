termplot <- function(model, data=model.frame(model), partial.resid=FALSE,
		     rug=FALSE, terms=NULL, se=FALSE, xlabs=NULL, ylab=NULL,
                     main = NULL, col.term = 2, lwd.term = 1.5,
                     col.se = "orange", lty.se = 2, lwd.se = 1,
                     col.res= "gray", cex = 1, pch = par("pch"),
                     ask = interactive() && nb.fig < n.tms &&
                           .Device != "postscript",
                     ...)
{
    terms <- ## need if(), since predict.coxph() has non-NULL default terms :
	if (is.null(terms))
	    predict(model, type="terms", se=se)
	else
	    predict(model, type="terms", se=se, terms=terms)
    n.tms <- ncol(tms <- as.matrix(if(se) terms$fit else terms))
    mf <- model.frame(model)
    nmt <- colnames(tms)
    cn <- parse(text=nmt)
    ## Defaults:
    if (is.null(xlabs))
	xlabs <- nmt
    if (is.null(ylab))
	ylab <- substitute(link(foo),
                           list(foo=formula(model)[[2]]))
    if (is.null(main))
        main <- ""
    else if(is.logical(main))
        main <- if(main) deparse(model$call) else ""
    else if(!is.character(main))
        stop("`main' must be TRUE, FALSE, NULL or character (vector).")
    main <- rep(main, length = n.tms) # recycling
    pf <- parent.frame()
    carrier <- function(term) { # used for non-factor ones
	if (length(term) > 1)
	    carrier(term[[2]])
	else
	    eval(term, data, enclos = pf)
    }
    if (partial.resid)
	pres <- residuals(model, "partial")
    is.fac <- sapply(nmt, function(i) is.factor(mf[,i]))

    se.lines <- function(x, iy, i, ff = 2) {
        tt <- ff * terms$se.fit[iy,i]
        lines(x, tms[iy,i] + tt, lty=lty.se, lwd=lwd.se, col=col.se)
        lines(x, tms[iy,i] - tt, lty=lty.se, lwd=lwd.se, col=col.se)
    }

    nb.fig <- prod(par("mfcol"))
    if (ask) {
        op <- par(ask = TRUE)
        on.exit(par(op))
    }
    ##---------- Do the individual plots : ----------

    for (i in 1:n.tms) {
	ylims <- range(tms[,i], na.rm=TRUE)
	if (se)
	    ylims <- range(ylims,
			   tms[,i] + 1.05*2*terms$se.fit[,i],
			   tms[,i] - 1.05*2*terms$se.fit[,i], na.rm=TRUE)
	if (partial.resid)
	    ylims <- range(ylims, pres[,i], na.rm=TRUE)
	if (rug)
	    ylims[1] <- ylims[1]-0.07*diff(ylims)

	if (is.fac[i]) {
	    ff <- mf[,nmt[i]]
	    ll <- levels(ff)
	    xlims <- range(seq(along=ll)) + c(-.5, .5)
            xx <- codes(ff) ##need if rug or partial
	    if(rug) {
		xlims[1] <- xlims[1]-0.07*diff(xlims)
		xlims[2] <- xlims[2]+0.03*diff(xlims)
	    }
	    plot(1,0, type = "n", xlab = xlabs[i], ylab = ylab,
                 xlim = xlims, ylim = ylims, main = main[i], ...)
	    for(j in seq(along=ll)) {
		ww <- which(ff==ll[j])[c(1,1)]
		jf <- j + c(-.4, .4)
		lines(jf,tms[ww,i], col=col.term, lwd=lwd.term, ...)
		if(se) se.lines(jf, iy=ww, i=i)
	    }
	}
	else { ## continuous carrier
	    xx <- carrier(cn[[i]])
	    xlims <- range(xx,na.rm=TRUE)
	    if(rug)
		xlims[1] <- xlims[1]-0.07*diff(xlims)
	    oo <- order(xx)
	    plot(xx[oo], tms[oo,i], type = "l", xlab = xlabs[i], ylab = ylab,
		 xlim = xlims, ylim = ylims, main = main[i],
                 col=col.term, lwd=lwd.term, ...)
            if(se) se.lines(xx[oo], iy=oo, i=i)
	}
	if (partial.resid)
	    points(xx, pres[,i], cex = cex, pch = pch, col = col.res)
	if (rug) {
            n <- length(xx)
            ## Fixme: Isn't this a kludge for segments() ?
	    lines(rep(jitter(xx), rep(3,n)),
                  rep(ylims[1] + c(0,0.05,NA)*diff(ylims), n))
	    if (partial.resid)
		lines(rep(xlims[1] + c(0,0.05,NA)*diff(xlims), n),
                      rep(pres[,i],rep(3,n)))
	}
    }
    invisible(n.tms)
}
