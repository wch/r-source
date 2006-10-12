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
                     diag = TRUE, diag.col = "gray", type = "p", oma = NULL,
                     ask = NULL, do.lines = (n <= 150), labels = do.lines, ...)
{
    lAxis <- function(side , ..., mgp, xpd, panel, Mgp)
        if(missing(Mgp)) axis(side, ..., xpd = NA)
        else axis(side, ..., xpd = NA, mgp = Mgp)

    xnam <- deparse(substitute(x))
    is.mat <- !is.null(ncol(x))
    nser <- ncol(x <- as.ts(as.matrix(x)))
    n <- nrow(x)

    if(missing(lags) && !missing(set.lags))
        lags <- length(set.lags <- as.integer(set.lags))
    tot.lags <- nser * lags

    if(is.null(ask)) {
        if (.Device == "null device") get(getOption("device"))()
        ask <-
            if(is.null(layout)) par("ask") ## FALSE, since will have big layout
            else (dev.interactive() && prod(layout) < tot.lags)
    }
    if(is.null(layout))
        layout <-
            if(prod(pmf <- par("mfrow")) >= tot.lags) pmf
            else grDevices::n2mfrow(tot.lags)

    ## Plotting
    ## avoid resetting mfrow and using outer margins for just one plot
    mlayout <- any(layout > 1)
    if(mlayout) {
        dots <- list(...)
        cex.main <- dots$cex.main
        if(is.null(cex.main)) cex.main <- par("cex.main")
        if(is.null(oma)) {
            oma <- rep(2, 4)
            if (!is.null(main)) oma[3] <- oma[3] + 3*cex.main
        }
        opar <- par(mfrow = layout,
                    mar = c(1.1, 1.1, 0.5, 0.5) + is.mat*c(0, 0.5, 0, 0.5),
                    oma = oma, ask = ask)
        on.exit(par(opar))
    }
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
            if(mlayout) {
                plot(lag(X, ll), X, xlim = xl, ylim = xl, asp = asp,
                     xlab = paste("lag", ll), ylab = nam,
                     mgp = if(mlayout) c(0,0,0),
                     axes = FALSE, type = type,
                     xy.lines = do.lines, xy.labels = labels,
                     col.lab = if(newX) "red",
                     font.lab = if(newX) 2,
                     ...)
                box(...) # pass bty along
                if (jj ==  1 && ii %% 2 == 1 && !newX)
                    lAxis(2, ...)
                if (ii ==  1 && jj %% 2 == 1)
                    lAxis(3, ...)

                do.4 <- (ii %% 2 == 0 && (jj == nC ||
                               ## very last one:
                               (i == nser && ll == set.lags[lags])))
                if (do.4) lAxis(4, ...)
                if (jj %% 2 == 0 && ii == nR) lAxis(1, ...)

                if(newX) {
                    newX <- FALSE
                    if(!do.4) lAxis(4, Mgp = c(0,.6,0), ...)
                }
            } else  {
                plot(lag(X, ll), X, xlim = xl, ylim = xl, asp = asp,
                     xlab = paste("lag", ll), ylab = nam,
                     type = type,
                     xy.lines = do.lines, xy.labels = labels,
                     main = main, ...)
            }
            if(diag) abline(c(0,1), lty = 2, col = diag.col)

            if (mlayout && !is.null(main)) {
                font.main <- dots$font.main
                if(is.null(font.main)) font.main <- par("font.main")
                if ((jj == nC && ii == nR)  || ll == set.lags[lags])
                    mtext(main, 3, 3, outer = TRUE, at = 0.5,
                          cex = cex.main, font = font.main)
            }
        }
    }
    invisible(NULL)
}
