## Copyright (C) 1998 John W. Emerson

mosaicplot <- function(X, main = NA, sort = NA, off = NA, dir = NA,
                       color = FALSE) {

    mosaic.cell <- function(X, x1, y1, x2, y2, off, dir, color, lablevx,
                            lablevy, maxdim, currlev, label) {

        if (dir[1] == "v") {            # split here on the X-axis.
            xdim <- maxdim[1]
            XP <- rep(0, xdim)
            for (i in 1:xdim) {
                XP[i] <- sum(X[X[,1]==i,ncol(X)]) / sum(X[,ncol(X)])
            }
            white <- off[1] * (x2 - x1) / (max(1, xdim-1))
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
                if (is.na(label[[1]][1])) {
                    this.lab <- paste(rep(as.character(currlev),
                                          length(currlev)),
                                      as.character(1:xdim), sep=".")
                } else { this.lab <- label[[1]] }
                text(x=(x.l + (x.r - x.l) / 2),
                     y=(965 + 22 * (lablevx - 1)), 
                     srt=0,adj=.5, cex=.5, this.lab)
            }
            if (ncol(X) > 2) {          # recursive call.
                for (i in 1:xdim) {
                    if (XP[i] > 0) {
                        mosaic.cell(as.matrix(X[X[,1]==i,2:ncol(X)]),
                                    x.l[i], y1, x.r[i], y2,
                                    off[2:length(off)],
                                    dir[2:length(dir)],
                                    color, lablevx-1, (i==1)*lablevy,
                                    maxdim[2:length(maxdim)], 
                                    currlev+1, label[2:ncol(X)])
                    } else {
                        segments(rep(x.l[i],3), y1+(y2-y1)*c(0,2,4)/5,
                                 rep(x.l[i],3), y1+(y2-y1)*c(1,3,5)/5)
                    }
                }
            } else {
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
        } else {                        # split here on the Y-axis.
            ydim <- maxdim[1]
            YP <- rep(0, ydim)
            for (j in 1:ydim) {
                YP[j] <- sum(X[X[,1]==j,ncol(X)]) / sum(X[,ncol(X)])
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
                if (is.na(label[[1]][1])) {
                    this.lab <- paste(rep(as.character(currlev),
                                          length(currlev)), 
                                      as.character(1:ydim), sep=".")
                } else { this.lab <- label[[1]] }
                text(x=(35 - 20 * (lablevy - 1)),
                     y=(y.b + (y.t - y.b) / 2),
                     srt=90, adj=.5, cex=.5, this.lab)
            }
            if (ncol(X) > 2) {          # recursive call.
                for (j in 1:ydim) {
                    if (YP[j] > 0) {
                        mosaic.cell(as.matrix(X[X[,1]==j,2:ncol(X)]),
                                    x1, y.b[j], x2, y.t[j],
                                    off[2:length(off)],
                                    dir[2:length(dir)], color,
                                    (j==1)*lablevx, lablevy-1,
                                    maxdim[2:length(maxdim)], 
                                    currlev+1, label[2:ncol(X)])
                    } else {
                        segments(x1+(x2-x1)*c(0,2,4)/5, rep(y.b[j],3),
                                 x1+(x2-x1)*c(1,3,5)/5, rep(y.b[j],3))
                    }
                }
            } else{                     # final split polygon and segments.
                for (j in 1:ydim) {
                    if (YP[j] > 0) {
                        polygon(c(x1,x2,x2,x1),
                                c(y.b[j],y.b[j],y.t[j],y.t[j]),
                                col=color[j])
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

    frame()
    opar <- par(usr = c(1,1000,1,1000))
    on.exit(par(opar))
    if (is.vector(X)) { X <- array(X) }
    dimd <- length(dim(X))
    if (!is.null(dimnames(X))) { label <- dimnames(X) } else { label <- NA }
    if (dimd>1) {
        Ind <- rep(1:(dim(X)[1]), prod(dim(X)[2:dimd]))
        for (i in 2:dimd) {
            Ind <- cbind(Ind,
                         c(matrix(1:(dim(X)[i]), byrow=TRUE,
                                  prod(dim(X)[1:(i-1)]),
                                  prod(dim(X)[i:dimd]))))
        }
    } else {
        Ind <- 1:(dim(X)[1])
    }
    Ind <- cbind(Ind, c(X))
    if (!is.na(main)) { title(main) }   # Make the title.
    if ((is.na(off[1]))||(length(off)!=dimd)) { # Initialize spacing.
        off <- rep(10,50)[1:dimd]
    }
    if (is.na(dir[1])||(length(dir)!=dimd)) { # Initialize directions.
        dir <- rep(c("v","h"),50)[1:dimd]
    }
    if ((!is.na(sort[1]))&&(length(sort)==dimd)) { # Sort columns.
        Ind <- Ind[,c(sort,dimd+1)]
        off <- off[sort]
        dir <- dir[sort]
        label <- label[sort]
    }
    ncolors <- length(tabulate(Ind[,dimd]))
    if (is.na(color[1])) {
        color <- rep(0, ncolors)
    } else {
        if (length(color) != ncolors) {
            if (!color[1]) { color <- rep(0, ncolors) }
            else { color <- 2:(ncolors+1) }
        }
    }

    mosaic.cell(Ind, 50, 5, 950, 950,
        off/100, dir, color, 2, 2, apply(as.matrix(Ind[,1:dimd]), 2, max),
        1, label)

}
