termplot <- function(model, data=model.frame(model), partial.resid=FALSE,
		     rug=FALSE,terms=NULL, se=FALSE, xlabs=NULL, ylab=NULL,
                     main = deparse(model$call), ...)
{
    terms <- ## need if(), since predict.coxph() has non-NULL default terms :
	if (is.null(terms))
	    predict(model, type="terms", se=se)
	else
	    predict(model, type="terms", se=se, terms=terms)

    mf <- model.frame(model)
    tms <- as.matrix(if(se) terms$fit else terms)
    nmt <- colnames(tms)
    cn <- parse(text=nmt)
    ## Defaults:
    if (is.null(xlabs))
	xlabs <- nmt
    if (is.null(ylab)) # "link(.)" really is only for  glm() alikes...
	ylab <- substitute(link(foo),
                           list(foo=formula(model)[[2]]))

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

    for (i in 1:ncol(tms)) {
	ylims <- range(tms[,i], na.rm=TRUE)
	if (se)
	    ylims <- range(ylims,
			   tms[,i] + 2*terms$se.fit[,i],
			   tms[,i] - 2*terms$se.fit[,i], na.rm=TRUE)
	if (partial.resid)
	    ylims <- range(ylims,pres[,i], na.rm=TRUE)
	if (rug)
	    ylims[1] <- ylims[1]-0.07*diff(ylims)

	if (is.fac[i]) {
	    ff <- mf[,nmt[i]]
	    ll <- levels(ff)
	    xlims <- range(seq(along=ll)) + c(-.5, .5)
	    if(rug) {
		xlims[1] <- xlims[1]-0.07*diff(xlims)
		xlims[2] <- xlims[2]+0.03*diff(xlims)
		xx <- codes(ff)
	    }
	    plot(1,0, type = "n", xlab = xlabs[i], ylab = ylab,
                 xlim = xlims, ylim = ylims, main = main, ...)
	    for(j in seq(along=ll)) {
		ww <- which(ff==ll[j])[c(1,1)]
		jf <- j + c(-.4, .4)
		lines(jf,tms[ww,i],...)
		if (se) {
		    lines(jf, tms[ww,i] + 1.96*terms$se.fit[ww,i], lty=2)
		    lines(jf, tms[ww,i] - 1.96*terms$se.fit[ww,i], lty=2)
		}
	    }
	}
	else { ## continuous carrier
	    xx <- carrier(cn[[i]])
	    xlims <- range(xx,na.rm=TRUE)
	    if(rug)
		xlims[1] <- xlims[1]-0.07*diff(xlims)
	    oo <- order(xx)
	    plot(xx[oo], tms[oo,i], type = "l", xlab = xlabs[i], ylab = ylab,
		 xlim = xlims, ylim = ylims, main = main, ...)
	    if (se) {
		lines(xx[oo], tms[oo,i] + 1.96*terms$se.fit[oo,i], lty=2)
		lines(xx[oo], tms[oo,i] - 1.96*terms$se.fit[oo,i], lty=2)
	    }
	}
	if (partial.resid)
	    points(xx,pres[,i])
	if (rug) {
            n <- length(xx)
	    lines(rep(jitter(xx), rep(3,n)),
                  rep(ylims[1] + c(0,0.05,NA)*diff(ylims), n))
	    if (partial.resid)
		lines(rep(xlims[1] + c(0,0.05,NA)*diff(xlims), n),
                      rep(pres[,i],rep(3,n)))
	}
    }
}
