## Function exists in S-plus

## Differences:
## 1) R has `type = "p"' argument
##    Idea: use "b" for n <= 10, else "p" as default, allow "text" / "labels" !
## 2) R uses `main', not `head' {consistency!}
## 3) R has `oma' and `...' args
## 4) R has  ask = par("ask") where S-plus has  ask = FALSE,
## ....

lag.plot <- function(x, lags = 1, layout = NULL, set.lags = 1:lags,
                     main = NULL, asp = 1,
                     font.main = par("font.main"), cex.main = par("cex.main"),
                     diag = TRUE, diag.col = "gray", type = "p", oma = NULL,
                     ask = NULL, do.lines = (n <= 150), labels = do.lines, ...)
{
    xnam <- deparse(substitute(x))
    is.mat <- !is.null(ncol(x))
    nser <- ncol(x <- as.ts(as.matrix(x)))
    n <- nrow(x)

    if(is.null(oma)) {
        oma <- rep(2, 4)
        if (!is.null(main)) oma[3] <- oma[3] + 3*cex.main
    }
    if(missing(lags) && !missing(set.lags))
        lags <- length(set.lags <- as.integer(set.lags))
    tot.lags <- nser * lags

    if(is.null(ask))
        ask <-
            if(is.null(layout)) par("ask")## FALSE, since will have big layout
            else (interactive() && .Device != "postscript" &&
                  prod(layout) < tot.lags)
    if(is.null(layout))
        layout <-
            if(prod(pmf <- par("mfrow")) >= tot.lags) pmf
            else n2mfrow(tot.lags)

    ## Plotting
    opar <- par(mfrow = layout, mar = c(1.1,1.1, .5,.5) + is.mat*c(0,.5,0,.5),
                oma = oma, ask = ask)
    on.exit(par(opar))
    nR <- layout[1]
    nC <- layout[2]

    ii <- jj <- 0 ## current row and column in the layout
    for(i in 1:nser) {
        X <-  x[,i]
        xl <- range(X)
        nam <- if(is.mat) dimnames(x)[[2]][i] else xnam
        newX <- is.mat

        for (ll in set.lags) {
            jj <- 1 + jj %% nC
            if(jj == 1) #  new row
                ii <- 1 + ii %% nR
            ##  plot.ts(x,y) *does* a lag plot -> text, ...
            plot(lag(X,ll), X, xlim = xl, ylim = xl, asp = asp,
                 xlab = paste("lag",ll), ylab = nam, mgp = c(0,0,0),
                 axes = FALSE, type = type,
                 xy.lines = do.lines, xy.labels = labels,
                 col.lab = if(newX) "red",
                 font.lab = if(newX) 2, ...)
            box()
            if(diag) abline(c(0,1), lty = 2, col = diag.col)

            if (jj ==  1 && ii %% 2 == 1 && !newX)
                axis(2, xpd=NA)
            if (ii ==  1 && jj %% 2 == 1)
                axis(3, xpd=NA)

            do.4 <- (ii %% 2 == 0 && (jj == nC ||
                           ## very last one:
                           (i == nser && ll == set.lags[lags])))
            if (do.4) axis(4, xpd=NA)
            if (jj %% 2 == 0 && ii == nR)
                axis(1, xpd=NA)

            if(newX) {
                newX <- FALSE
                if(!do.4) axis(4, xpd = NA, mgp = c(0,.6,0))
            }

            if (!is.null(main) &&
                (jj == nC && ii == nR)  || ll == set.lags[lags])
                mtext(main, 3, 3, outer = TRUE, at = 0.5,
                      cex = cex.main, font = font.main)
        }
    }
    invisible(NULL)
}
