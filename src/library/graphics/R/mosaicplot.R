## Original code copyright (C) 1998 John W. Emerson

mosaicplot <- function(x, ...) UseMethod("mosaicplot")

### Changes by MM:
## - NULL instead of NA for default arguments, etc  [R / S convention]
## - plotting at end; cosmetic; warn about unused ... since we really don't..
## - mosaic.cell():  ...(?)
### Changes by KH:
##   Shading of boxes to visualize deviations from independence by
##   displaying sign and magnitude of the standardized residuals.
### Changes by W. Fischer and U. Ligges:
## - Deparsing x in for main title. New arguments: sub, las, cex.axis
## - made to work by BDR

mosaicplot.default <-
function(x, main = deparse(substitute(x)), sub = NULL, xlab = NULL,
         ylab = NULL, sort = NULL, off = NULL, dir = NULL,
         color = FALSE, shade = FALSE, margin = NULL,
         cex.axis = 0.66, las = par("las"),
         type = c("pearson", "deviance", "FT"), ...)
{
    mosaic.cell <- function(X, x1, y1, x2, y2, srt.x, srt.y,
            adj.x, adj.y, off, dir, color, lablevx, lablevy,
            maxdim, currlev, label)
    {
        ## Recursive function doing "the job"
        ##
        ## explicitly relying on (1,1000)^2 user coordinates.
        p <- ncol(X) - 2
        if (dir[1] == "v") {            # split here on the X-axis.
            xdim <- maxdim[1]
            XP <- rep.int(0, xdim)
            for (i in 1:xdim) {
                XP[i] <- sum(X[X[,1]==i,p]) / sum(X[,p])
            }
            if(any(is.na(XP))) stop("missing values in contingency table")
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
                        paste(rep.int(as.character(currlev),
                                      length(currlev)),
                              as.character(1:xdim), sep=".")
                    } else label[[1]]
                text(x= x.l + (x.r - x.l) / 2,
                     y= 965 + 22 * (lablevx - 1),
                     srt=srt.x, adj=adj.x, cex=cex.axis, this.lab)
            }
            if (p > 2) {                # recursive call.
                for (i in 1:xdim) {
                    if (XP[i] > 0) {
                        Recall(X[X[,1]==i, 2:(p+2) , drop=FALSE],
                               x.l[i], y1, x.r[i], y2,
                               srt.x, srt.y, adj.x, adj.y,
                               off[-1], dir[-1], color,
                               lablevx-1, (i==1)*lablevy,
                               maxdim[-1], currlev+1, label[2:p])
                    } else {
                        segments(rep.int(x.l[i],3), y1+(y2-y1)*c(0,2,4)/5,
                                 rep.int(x.l[i],3), y1+(y2-y1)*c(1,3,5)/5)
                    }
                }
            } else { # ncol(X) <= 1 : final split polygon and segments.
                for (i in 1:xdim) {
                    if (XP[i] > 0) {
                        polygon(c(x.l[i], x.r[i], x.r[i], x.l[i]),
                                c(y1, y1, y2, y2),
                                lty = if(extended) X[i, p+1] else 1,
                                col = color[if(extended) X[i, p+2] else i])
                    } else {
                        segments(rep.int(x.l[i],3), y1+(y2-y1)*c(0,2,4)/5,
                                 rep.int(x.l[i],3), y1+(y2-y1)*c(1,3,5)/5)
                    }
                }
            }
        } else {                        # split here on the Y-axis.
            ydim <- maxdim[1]
            YP <- rep.int(0, ydim)
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
                        paste(rep.int(as.character(currlev),
                                      length(currlev)),
                              as.character(1:ydim), sep=".")
                    } else label[[1]]
                text(x= 35 - 20 * (lablevy - 1),
                     y= y.b + (y.t - y.b) / 2,
                     srt=srt.y, adj=adj.y, cex=cex.axis, this.lab)
            }
            if (p > 2) {                # recursive call.
                for (j in 1:ydim) {
                    if (YP[j] > 0) {
                        Recall(X[X[,1]==j, 2:(p+2) , drop=FALSE],
                               x1, y.b[j], x2, y.t[j],
                               srt.x, srt.y, adj.x, adj.y,
                               off[-1], dir[-1], color,
                               (j==1)*lablevx, lablevy-1,
                               maxdim[-1], currlev+1, label[2:p])
                    } else {
                        segments(x1+(x2-x1)*c(0,2,4)/5, rep.int(y.b[j],3),
                                 x1+(x2-x1)*c(1,3,5)/5, rep.int(y.b[j],3))
                    }
                }
            } else { # ncol(X) <= 1: final split polygon and segments.
                for (j in 1:ydim) {
                    if (YP[j] > 0) {
                        polygon(c(x1,x2,x2,x1),
                                c(y.b[j],y.b[j],y.t[j],y.t[j]),
                                lty = if(extended) X[j, p+1] else 1,
                                col = color[if(extended) X[j, p+2] else j])
                    } else {
                        segments(x1+(x2-x1)*c(0,2,4)/5, rep.int(y.b[j],3),
                                 x1+(x2-x1)*c(1,3,5)/5, rep.int(y.b[j],3))
                    }
                }
            }
        }
    }

    ##-- Begin main function

    ## Calculate string rotation for different settings of las:
    srt.x <- if(las > 1) 90 else 0
    srt.y <- if(las == 0 || las == 3) 90 else 0

    if(is.null(dim(x)))
        x <- as.array(x)
    else if(is.data.frame(x))
        x <- data.matrix(x)
    dimd <- length(dx <- dim(x))
    if(dimd == 0 || any(dx == 0))
        stop("'x' must not have 0 dimensionality")
    if(length(list(...)))
        warning("extra argument(s) ",
                paste(sQuote(names(list(...))), collapse = ", "),
                " will be disregarded")
    ##-- Set up 'Ind' matrix : to contain indices and data
    Ind <- 1:dx[1]
    if(dimd > 1) {
        Ind <- rep.int(Ind, prod(dx[2:dimd]))
        for (i in 2:dimd) {
            Ind <- cbind(Ind,
                         c(matrix(1:dx[i], byrow=TRUE,
                                  nr = prod(dx[1:(i-1)]),
                                  nc = prod(dx[i:dimd]))))
        }
    }
    Ind <- cbind(Ind, c(x))
    ## Ok, now the columns of 'Ind' are the cell indices (which could
    ## also have been created by 'expand.grid()' and the corresponding
    ## cell counts.  We add two more columns for dealing with *EXTENDED*
    ## mosaic plots which are produced unless 'shade' is FALSE, which
    ## currently is the default.  These columns have NAs for the simple
    ## case.  Otherwise, they specify the line type (1 for positive and
    ## 2 for negative residuals) and color (by giving the index in the
    ## color vector which ranges from the "most negative" to the "most
    ## positive" residuals.
    if(is.logical(shade) && !shade) {
        extended <- FALSE
        Ind <- cbind(Ind, NA, NA)
    }
    else {
        if(is.logical(shade))
            shade <- c(2, 4)
        else if(any(shade <= 0) || length(shade) > 5)
            stop("invalid shade specification")
        extended <- TRUE
        shade <- sort(shade)
        breaks <- c(-Inf, - rev(shade), 0, shade, Inf)
        color <- c(hsv(0,               # red
                       s = seq(1, to = 0, length = length(shade) + 1)),
                   hsv(4/6,             # blue
                       s = seq(0, to = 1, length = length(shade) + 1)))
        if(is.null(margin))
            margin <- as.list(1:dimd)
        ## Fit the loglinear model.
        E <- stats::loglin(x, margin, fit = TRUE, print = FALSE)$fit
        ## Compute the residuals.
        type <- match.arg(type)
        residuals <-
            switch(type,
                   pearson = (x - E) / sqrt(E),
                   deviance = {
                       tmp <- 2 * (x * log(ifelse(x==0, 1, x/E)) - (x-E))
                       tmp <- sqrt(pmax(tmp, 0))
                       ifelse(x > E, tmp, -tmp)
                   },
                   FT = sqrt(x) + sqrt(x + 1) - sqrt(4 * E + 1))
        ## And add the information to the data matrix.
        Ind <- cbind(Ind,
                     c(1 + (residuals < 0)),
                     as.numeric(cut(residuals, breaks)))
    }

    ## The next four may all be NULL:
    label <- dimnames(x)
    nam.dn <- names(label)
    if(is.null(xlab)) xlab <- nam.dn[1]
    if(is.null(ylab)) ylab <- nam.dn[2]

    if (is.null(off) || length(off) != dimd) { # Initialize spacing.
        off <- rep.int(10, dimd)
    }
    if (is.null(dir) || length(dir) != dimd) {# Initialize directions
        dir <- rep(c("v","h"), length.out = dimd)
    }
    if (!is.null(sort)) {
        if(length(sort) != dimd)
            stop("length(sort) does not conform to dim(x)")
        ## Sort columns.
        Ind[,1:dimd] <- Ind[,sort]
        off <- off[sort]
        dir <- dir[sort]
        label <- label[sort]
    }

    ncolors <- length(tabulate(Ind[,dimd]))
    if(!extended && ((is.null(color) || length(color) != ncolors))) {
        color <-
            if (is.logical(color) && color[1])
                heat.colors(ncolors)
            else if (is.null(color) || (is.logical(color) && !color[1]))
                rep.int(0, ncolors)
            else ## recycle
                rep(color, length.out = ncolors)
    }

    ##-- Plotting
    plot.new()
    if(!extended) {
        opar <- par(usr = c(1, 1000, 1, 1000), mgp = c(1, 1, 0))
        on.exit(par(opar))
    }
    else {
        ## This code is extremely ugly, and certainly can be improved.
        ## In the case of extended displays, we also need to provide a
        ## legend for the shading and outline patterns.  The code works
        ## o.k. with integer breaks in 'shade'; rounding to two 2 digits
        ## will not be good enough if 'shade' has length 5.
        pin <- par("pin")
        rtxt <- "Standardized\nResiduals:"
        ## Compute cex so that the rotated legend text does not take up
        ## more than 1/12 of the of the plot region horizontally and not
        ## more than 1/4 vertically.
        rtxtCex <- min(1,
                       pin[1] / (strheight(rtxt, units = "inches") * 12),
                       pin[2] / (strwidth (rtxt, units = "inches") / 4))
        rtxtWidth <- 0.1                # unconditionally ..
        ## We put the legend to the right of the third axis.
        opar <- par(usr = c(1, 1000 * (1.1 + rtxtWidth), 1, 1000),
                    mgp = c(1, 1, 0))
        on.exit(par(opar))
        rtxtHeight <-
            strwidth(rtxt, units = "i", cex = rtxtCex) / pin[2]
        text(1000 * (1.05 + 0.5 * rtxtWidth), 0, labels = rtxt,
             adj = c(0, 0.25), srt = 90, cex = rtxtCex)
        ## 'len' is the number of positive or negative intervals of
        ## residuals (so overall, there are '2 * len')
        len <- length(shade) + 1
        ## 'bh' is the height of each box in the legend (including the
        ## separating whitespace
        bh <- 0.95 * (0.95 - rtxtHeight) / (2 * len)
        x.l <- 1000 * 1.05
        x.r <- 1000 * (1.05 + 0.7 * rtxtWidth)
        y.t <- 1000 * rev(seq(from = 0.95, by = - bh, length = 2 * len))
        y.b <- y.t - 1000 * 0.8 * bh
        ltype <- c(rep.int(2, len), rep.int(1, len))
        for(i in 1 : (2 * len)) {
            polygon(c(x.l, x.r, x.r, x.l),
                    c(y.b[i], y.b[i], y.t[i], y.t[i]),
                    col = color[i],
                    lty = ltype[i])
        }
        brks <- round(breaks, 2)
        y.m <- y.b + 1000 * 0.4 * bh
        text(1000 * (1.05 + rtxtWidth), y.m,
             c(paste("<", brks[2], sep = ""),
               paste(brks[2 : (2 * len - 1)],
                     brks[3 : (2 * len)],
                     sep = ":"),
               paste(">", brks[2 * len], sep = "")),
             srt = 90, cex = cex.axis)
    }

    if (!is.null(main) || !is.null(xlab) || !is.null(ylab) || !is.null(sub))
        title(main, sub = sub, xlab = xlab, ylab = ylab)
    adj.x <- adj.y <- 0.5
    x1 <- 50; y1 <- 5; x2 <- 950; y2 <- 950
    maxlen.xlabel <- maxlen.ylabel <- 35
    ## Calculations required for 'las' related string rotation
    ## and adjustment
    if(srt.x == 90){
        maxlen.xlabel <-
            max(strwidth(label[[dimd + 1 - match('v', rev(dir))]],
                cex = cex.axis))
        adj.x <- 1
        y2 <- y2 - maxlen.xlabel
    }
    if(srt.y == 0){
        maxlen.ylabel <-
            max(strwidth(label[[match('h', dir)]],
                cex = cex.axis))
        adj.y <- 0
        x1 <- x1 + maxlen.ylabel
    }

    mosaic.cell(Ind, x1 = x1, y1 = y1, x2 = x2, y2 = y2,
                srt.x = srt.x, srt.y = srt.y, adj.x = adj.x,
                adj.y = adj.y, off = off / 100, dir = dir,
                color = color, lablevx = 2, lablevy = 2,
                maxdim = apply(as.matrix(Ind[,1:dimd]), 2, max),
                currlev = 1, label = label)
}

mosaicplot.formula <-
function(formula, data = NULL, ...,
         main = deparse(substitute(data)), subset, na.action = stats::na.omit)
{
    main # force evaluation here
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, parent.frame())
    if(inherits(edata, "ftable")
       || inherits(edata, "table")
       || length(dim(edata)) > 2) {
        data <- as.table(data)
        varnames <- attr(stats:::terms.formula(formula), "term.labels")
        if(all(varnames != "."))
            data <- margin.table(data,
                                 match(varnames, names(dimnames(data))))
        mosaicplot(data, main = main, ...)
    }
    else {
        if(is.matrix(edata))
            m$data <- as.data.frame(data)
        m$main <- m$... <- NULL
        m$na.action <- na.action
        m[[1]] <- as.name("model.frame")
        mf <- eval(m, parent.frame())
        mosaicplot(table(mf), main = main, ...)
    }
}
