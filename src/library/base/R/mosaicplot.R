## Copyright (C) 1998 John W. Emerson


mosaicplot <- function(x, ...) UseMethod("mosaicplot")

### Changes by MM:
## - NULL instead of NA for default arguments, etc  [R / S convention]
## - plotting at end; cosmetic
## - mosaic.cell():

mosaicplot.default <- function(X, main = NULL, xlab = NULL, ylab = NULL,
                               sort = NULL, off = NULL,
                               dir = NULL, color = FALSE) {

    mosaic.cell <- function(X, x1, y1, x2, y2,
                            off, dir, color, lablevx, lablevy,
                            maxdim, currlev, label)
    {
        ## Recursive function doing `the job'
        ##
        ## explicitely relying on (1,1000)^2 user coordinates.
        p <- ncol(X)
        if (dir[1] == "v") {            # split here on the X-axis.
            xdim <- maxdim[1]
            XP <- rep(0, xdim)
            for (i in 1:xdim) {
                XP[i] <- sum(X[X[,1]==i,p]) / sum(X[,p])
            }
            white <- off[1] * (x2 - x1) / max(1, xdim-1)
            x.l <- x1
            x.r <- x1 + (1 - off[1]) * XP[1] * (x2 - x1)
            if (xdim > 1) {
                for (i in 2:xdim) {
                    x.l <- c(x.l, x.r[i-1] + white)
                    x.r <- c(x.r, x.r[i-1] + white +
                             (1 - off[1]) * XP[i] * (x2 - x1))
                }
            }
            if (lablevx > 0) {
                this.lab <-
                    if (is.null(label[[1]][1])) {
                        paste(rep(as.character(currlev),
                                  length(currlev)),
                              as.character(1:xdim), sep=".")
                    } else label[[1]]
                text(x= x.l + (x.r - x.l) / 2,
                     y= 965 + 22 * (lablevx - 1),
                     srt=0, adj=.5, cex=.66, this.lab)
            }
            if (p > 2) {          # recursive call.
                for (i in 1:xdim) {
                    if (XP[i] > 0) {
                        mosaic.cell(as.matrix(X[X[,1]==i, 2:p]),
                                    x.l[i], y1, x.r[i], y2,
                                    off[2:length(off)],
                                    dir[2:length(dir)],
                                    color, lablevx-1, (i==1)*lablevy,
                                    maxdim[2:length(maxdim)],
                                    currlev+1, label[2:p])
                    } else {
                        segments(rep(x.l[i],3), y1+(y2-y1)*c(0,2,4)/5,
                                 rep(x.l[i],3), y1+(y2-y1)*c(1,3,5)/5)
                    }
                }
            } else { # ncol(X) <= 1 : final split polygon and segments.
                for (i in 1:xdim) {
                    if (XP[i] > 0) {
                        polygon(c(x.l[i], x.r[i], x.r[i], x.l[i]),
                                c(y1, y1, y2, y2), col=color[i])
                        segments(c(rep(x.l[i],3),x.r[i]),
                                 c(y1,y1,y2,y2),
                                 c(x.r[i],x.l[i],x.r[i],x.r[i]),
                                 c(y1,y2,y2,y1))
                    } else {
                        segments(rep(x.l[i],3), y1+(y2-y1)*c(0,2,4)/5,
                                 rep(x.l[i],3), y1+(y2-y1)*c(1,3,5)/5)
                    }
                }
            }
        } else { ## dir[1] - "horizontal" : split here on the Y-axis.
            ydim <- maxdim[1]
            YP <- rep(0, ydim)
            for (j in 1:ydim) {
                YP[j] <- sum(X[X[,1]==j,p]) / sum(X[,p])
            }
            white <- off[1] * (y2 - y1) / (max(1, ydim - 1))
            y.b <- y2 - (1 - off[1]) * YP[1] * (y2 - y1)
            y.t <- y2
            if (ydim > 1) {
                for (j in 2:ydim) {
                    y.b <- c(y.b, y.b[j-1] - white -
                             (1 - off[1]) * YP[j] * (y2 - y1))
                    y.t <- c(y.t, y.b[j-1] - white)
                }
            }
            if (lablevy > 0) {
                this.lab <-
                    if (is.null(label[[1]][1])) {
                        paste(rep(as.character(currlev),
                                  length(currlev)),
                              as.character(1:ydim), sep=".")
                    } else label[[1]]
                text(x= 35 - 20 * (lablevy - 1),
                     y= y.b + (y.t - y.b) / 2,
                     srt=90, adj=.5, cex=.66, this.lab)
            }
            if (p > 2) {          # recursive call.
                for (j in 1:ydim) {
                    if (YP[j] > 0) {
                        mosaic.cell(as.matrix(X[X[,1]==j,2:p]),
                                    x1, y.b[j], x2, y.t[j],
                                    off[2:length(off)],
                                    dir[2:length(dir)], color,
                                    (j==1)*lablevx, lablevy-1,
                                    maxdim[2:length(maxdim)],
                                    currlev+1, label[2:p])
                    } else {
                        segments(x1+(x2-x1)*c(0,2,4)/5, rep(y.b[j],3),
                                 x1+(x2-x1)*c(1,3,5)/5, rep(y.b[j],3))
                    }
                }
            } else {  # ncol(X) <= 1: final split polygon and segments.
                for (j in 1:ydim) {
                    if (YP[j] > 0) {
                        polygon(c(x1,x2,x2,x1),
                                c(y.b[j],y.b[j],y.t[j],y.t[j]), col=color[j])
                        segments(c(x1,x1,x1,x2),
                                 c(y.b[j],y.b[j],y.t[j],y.t[j]),
                                 c(x2,x1,x2,x2),
                                 c(y.b[j],y.t[j],y.t[j],y.b[j]))
                    } else {
                        segments(x1+(x2-x1)*c(0,2,4)/5, rep(y.b[j],3),
                                 x1+(x2-x1)*c(1,3,5)/5, rep(y.b[j],3))
                    }
                }
            }
        }
    }

    ##-- Begin main function
    if(is.null(dim(X)))
        X <- as.array(X)
    else if(is.data.frame(X))
        X <- data.matrix(X)
    dimd <- length(dX <- dim(X))
    if(dimd == 0 || any(dX == 0))
        stop("`X' must not have 0 dimensionality")
    ##-- Set up `Ind' matrix : to contain indices and data
    Ind <- 1:dX[1]
    if(dimd > 1) {
        Ind <- rep(Ind, prod(dX[2:dimd]))
        for (i in 2:dimd) {
            Ind <- cbind(Ind,
                         c(matrix(1:dX[i], byrow=TRUE,
                                  nr = prod(dX[1:(i-1)]),
                                  nc = prod(dX[i:dimd]))))
        }
    }
    Ind <- cbind(Ind, c(X))
    ## The next four may all be NULL:
    label <- dimnames(X)
    nam.dn <- names(label)
    if(is.null(xlab)) xlab <- nam.dn[1]
    if(is.null(ylab)) ylab <- nam.dn[2]

    if (is.null(off) || length(off) != dimd) { # Initialize spacing.
        off <- rep(10, length=dimd)
    }
    if (is.null(dir) || length(dir) != dimd) {# Initialize directions
        dir <- rep(c("v","h"), length=dimd)
    }
    if (!is.null(sort)) {
        if(length(sort) != dimd)
            stop("length(sort) doesn't conform to dim(X)")
        ## Sort columns.
        Ind <- Ind[,c(sort,dimd+1)]
        off <- off[sort]
        dir <- dir[sort]
        label <- label[sort]
    }
    ncolors <- length(tabulate(Ind[,dimd]))
    if (is.null(color) || length(color) != ncolors)
        color <- if (is.null(color) || !color[1])
            rep(0, ncolors) else 2:(ncolors+1)

    ##-- Plotting
    frame()
    opar <- par(usr = c(1, 1000, 1, 1000), mgp = c(1, 1, 0))
    on.exit(par(opar))

    if (!is.null(main) || !is.null(xlab) || !is.null(ylab))
        title(main, xlab=xlab, ylab=ylab)

    mosaic.cell(Ind,
                x1=50, y1=5, x2=950, y2=950,
                off/100, dir,
                color, 2, 2,
                maxdim= apply(as.matrix(Ind[,1:dimd]), 2, max),
                currlev= 1, label)

}

mosaicplot.formula <- function(formula, data = NULL, subset, na.action,
                               ...) {
    if (missing(na.action))
        na.action <- options()$na.action
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, sys.frame(sys.parent()))))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, sys.frame(sys.parent()))
    mosaicplot(table(mf), ...)
}
