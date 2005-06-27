pairs <- function(x, ...) UseMethod("pairs")

pairs.formula <-
function(formula, data = NULL, ..., subset, na.action = stats::na.pass)
{
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action # force in even if  default
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    pairs(mf, ...)
}

#################################################
## some of the changes are from code
## Copyright 1999 Dr. Jens Oehlschlaegel-Akiyoshi
## Others are by BDR and MM
#################################################

pairs.default <-
function (x, labels, panel = points, ..., main = NULL, oma = NULL,
          font.main = par("font.main"), cex.main = par("cex.main"),
          lower.panel = panel, upper.panel = panel,
          diag.panel = NULL, text.panel = textPanel,
          label.pos = 0.5 + has.diag/3,
          cex.labels = NULL, font.labels = 1,
          row1attop = TRUE, gap=1)
{
    textPanel <-
        function(x = 0.5, y = 0.5, txt, cex, font)
            text(x, y, txt, cex = cex, font = font)

    localAxis <- function(side, xpd, bg, ...) axis(side, xpd = NA, ...)

    if (!is.matrix(x)) x <- data.matrix(x)
    if (!is.numeric(x)) stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if((has.diag  <- !is.null( diag.panel)) && !missing( diag.panel))
        diag.panel <- match.fun( diag.panel)

    if(row1attop) {
        tmp <- lower.panel; lower.panel <- upper.panel; upper.panel <- tmp
        tmp <- has.lower; has.lower <- has.upper; has.upper <- tmp
    }

    nc <- ncol(x)
    if (nc < 2) stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels)) labels <- paste("var", 1:nc)
    }
    else if(is.null(labels)) has.labs <- FALSE
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main)) oma[3] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))

    for (i in if(row1attop) 1:nc else nc:1)
        for (j in 1:nc) {
            plot(x[, j], x[, i], xlab = "", ylab = "",
                 axes = FALSE, type = "n", ...)
            if(i == j || (i < j && has.lower) || (i > j && has.upper) ) {
                box()
                if(i == 1  && (!(j %% 2) || !has.upper || !has.lower ))
                    localAxis(1 + 2*row1attop, ...)
                if(i == nc && (  j %% 2  || !has.upper || !has.lower ))
                    localAxis(3 - 2*row1attop, ...)
                if(j == 1  && (!(i %% 2) || !has.upper || !has.lower ))
                    localAxis(2, ...)
                if(j == nc && (  i %% 2  || !has.upper || !has.lower ))
                    localAxis(4, ...)
                mfg <- par("mfg")
                if(i == j) {
                    if (has.diag) diag.panel(as.vector(x[, i]))
                    if (has.labs) {
                        par(usr = c(0, 1, 0, 1))
                        if(is.null(cex.labels)) {
                            l.wid <- strwidth(labels, "user")
                            cex.labels <- max(0.8, min(2, .9 / max(l.wid)))
                        }
                        text.panel(0.5, label.pos, labels[i],
                                   cex = cex.labels, font = font.labels)
                    }
                } else if(i < j)
                    lower.panel(as.vector(x[, j]), as.vector(x[, i]), ...)
                else
                    upper.panel(as.vector(x[, j]), as.vector(x[, i]), ...)
                if (any(par("mfg") != mfg))
                    stop("the 'panel' function made a new plot")
            } else par(new = FALSE)

        }
    if (!is.null(main))
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    invisible(NULL)
}
