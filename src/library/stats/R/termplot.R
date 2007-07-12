termplot <- function(model, data = NULL,envir = environment(formula(model)),
                     partial.resid = FALSE,
		     rug = FALSE, terms = NULL, se = FALSE,
                     xlabs = NULL, ylabs = NULL,
                     main = NULL, col.term = 2, lwd.term = 1.5,
                     col.se = "orange", lty.se = 2, lwd.se = 1,
                     col.res= "gray", cex = 1, pch = par("pch"),
                     col.smth = "darkred",lty.smth = 2,span.smth = 2/3,
                     ask = dev.interactive() && nb.fig < n.tms,
                     use.factor.levels = TRUE, smooth = NULL,
                     ylim = "common", ...)
{
    which.terms <- terms
    terms <- ## need if(), since predict.coxph() has non-NULL default terms :
	if (is.null(terms))
	    predict(model, type = "terms", se.fit = se)
	else
	    predict(model, type = "terms", se.fit = se, terms = terms)
    n.tms <- ncol(tms <- as.matrix(if(se) terms$fit else terms))
    mf <- model.frame(model)
    if (is.null(data))
        data <- eval(model$call$data, envir)
    if (is.null(data))
        data <- mf
    if (NROW(tms)<NROW(data)){
        use.rows <- match(rownames(tms), rownames(data))
      } else use.rows <- NULL
    nmt <- colnames(tms)
    cn <- parse(text = nmt)
    ## Defaults:
    if (!is.null(smooth))
      smooth <- match.fun(smooth)
    if (is.null(ylabs))
	ylabs <- paste("Partial for",nmt)
    if (is.null(main))
        main <- ""
    else if(is.logical(main))
        main <- if(main) deparse(model$call, 500) else ""
    else if(!is.character(main))
        stop("'main' must be TRUE, FALSE, NULL or character (vector).")
    main <- rep(main, length.out = n.tms) # recycling
    pf <- envir
    carrier <- function(term) { # used for non-factor ones
	if (length(term) > 1)
	    carrier(term[[2]])
	else
	    eval(term, data, enclos = pf)
    }
    carrier.name <- function(term){
      	if (length(term) > 1)
	    carrier.name(term[[2]])
	else
	    as.character(term)
    }
    if (is.null(xlabs))
        xlabs <- unlist(lapply(cn,carrier.name))

    if (partial.resid || !is.null(smooth)){
	pres <- residuals(model, "partial")
        if (!is.null(which.terms)) pres <- pres[, which.terms, drop = FALSE]
      }
    is.fac <- sapply(nmt, function(i) is.factor(mf[, i]))

    se.lines <- function(x, iy, i, ff = 2) {
        tt <- ff * terms$se.fit[iy, i]
        lines(x, tms[iy, i] + tt, lty = lty.se, lwd = lwd.se, col = col.se)
        lines(x, tms[iy, i] - tt, lty = lty.se, lwd = lwd.se, col = col.se)
    }

    nb.fig <- prod(par("mfcol"))
    if (ask) {
        op <- par(ask = TRUE)
        on.exit(par(op))
    }

    ylims <- ylim
    if(identical(ylims, "common")) {
        ylims <- if(!se) range(tms, na.rm = TRUE)
        else range(tms + 1.05*2*terms$se.fit,
                   tms - 1.05*2*terms$se.fit,
                   na.rm = TRUE)
        if (partial.resid) ylims <- range(ylims, pres, na.rm = TRUE)
        if (rug) ylims[1] <- ylims[1] - 0.07*diff(ylims)
    }

    ##---------- Do the individual plots : ----------

    for (i in 1:n.tms) {
        if(identical(ylim, "free")) {
            ylims <- range(tms[, i], na.rm = TRUE)
            if (se)
                ylims <- range(ylims,
                               tms[, i] + 1.05*2*terms$se.fit[, i],
                               tms[, i] - 1.05*2*terms$se.fit[, i],
                               na.rm = TRUE)
            if (partial.resid)
                ylims <- range(ylims, pres[, i], na.rm = TRUE)
            if (rug)
                ylims[1] <- ylims[1] - 0.07*diff(ylims)
        }
	if (is.fac[i]) {
	    ff <- mf[,nmt[i]]
            if (!is.null(model$na.action))
              ff <- naresid(model$na.action, ff)
	    ll <- levels(ff)
	    xlims <- range(seq_along(ll)) + c(-.5, .5)
            xx <- as.numeric(ff) ##need if rug or partial
	    if(rug) {
		xlims[1] <- xlims[1] - 0.07*diff(xlims)
		xlims[2] <- xlims[2] + 0.03*diff(xlims)
	    }
	    plot(1, 0, type = "n", xlab = xlabs[i], ylab = ylabs[i],
                 xlim = xlims, ylim = ylims, main = main[i], xaxt="n", ...)
            if (use.factor.levels)
                axis(1, at = seq_along(ll), labels = ll, ...)
            else
                axis(1)
	    for(j in seq_along(ll)) {
		ww <- which(ff==ll[j])[c(1,1)]
		jf <- j + c(-.4, .4)
		lines(jf,tms[ww, i], col = col.term, lwd = lwd.term, ...)
		if(se) se.lines(jf, iy = ww, i = i)
	    }
	}
	else { ## continuous carrier
	    xx <- carrier(cn[[i]])
            if (!is.null(use.rows)) xx <- xx[use.rows]
	    xlims <- range(xx, na.rm = TRUE)
	    if(rug)
		xlims[1] <- xlims[1] - 0.07*diff(xlims)
	    oo <- order(xx)
	    plot(xx[oo], tms[oo, i], type = "l",
                 xlab = xlabs[i], ylab = ylabs[i],
		 xlim = xlims, ylim = ylims, main = main[i],
                 col = col.term, lwd = lwd.term, ...)
            if(se) se.lines(xx[oo], iy = oo, i = i)
	}
	if (partial.resid){
          if (!is.fac[i] && !is.null(smooth)){
            smooth(xx,pres[, i], lty = lty.smth,
                   cex = cex, pch = pch, col = col.res,
                   col.smooth = col.smth, span = span.smth)
          } else
          points(xx, pres[, i], cex = cex, pch = pch, col = col.res)
        }
	if (rug) {
            n <- length(xx)
            ## Fixme: Isn't this a kludge for segments() ?
	    lines(rep.int(jitter(xx), rep.int(3, n)),
                  rep.int(ylims[1] + c(0, 0.05, NA)*diff(ylims), n))
	    if (partial.resid)
		lines(rep.int(xlims[1] + c(0, 0.05, NA)*diff(xlims), n),
                      rep.int(pres[, i], rep.int(3, n)))
	}
    }
    invisible(n.tms)
}
