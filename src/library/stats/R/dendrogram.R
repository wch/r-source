#  File src/library/stats/R/dendrogram.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

as.dendrogram.dendrogram <- function(object, ...) object

as.dendrogram.hclust <- function (object, hang = -1, check = TRUE, ...)
## hang = 0.1  is default for plot.hclust
{
    nolabels <- is.null(object$labels)
    merge <- object$merge
    if(check && !isTRUE(msg <- .validity.hclust(object, merge, order=nolabels)))
	stop(msg)
    if(nolabels)
	object$labels <- seq_along(object$order)
    z <- list()
    nMerge <- length(oHgt <- object$height)
    hMax <- oHgt[nMerge]
    for (k in 1L:nMerge) {
	x <- merge[k, ]# no sort() anymore!
	if (any(neg <- x < 0))
	    h0 <- if (hang < 0) 0 else max(0, oHgt[k] - hang * hMax)
	if (all(neg)) {			# two leaves
	    zk <- as.list(-x)
	    attr(zk, "members") <- 2L
	    attr(zk, "midpoint") <- 0.5 # mean( c(0,1) )
	    objlabels <- object$labels[-x]
	    attr(zk[[1L]], "label") <- objlabels[1L]
	    attr(zk[[2L]], "label") <- objlabels[2L]
	    attr(zk[[1L]], "members") <- attr(zk[[2L]], "members") <- 1L
	    attr(zk[[1L]], "height")  <- attr(zk[[2L]], "height") <- h0
	    attr(zk[[1L]], "leaf")    <- attr(zk[[2L]], "leaf") <- TRUE
	}
	else if (any(neg)) {		# one leaf, one node
	    X <- as.character(x)
	    ## Originally had "x <- sort(..) above => leaf always left, x[1L];
	    ## don't want to assume this
	    isL <- x[1L] < 0 ## is leaf left?
	    zk <-
		if(isL) list(-x[1L], z[[X[2L]]])
		else	list(z[[X[1L]]], -x[2L])
	    attr(zk, "members") <- attr(z[[X[1 + isL]]], "members") + 1L
	    attr(zk, "midpoint") <-
                (.memberDend(zk[[1L]]) + attr(z[[X[1 + isL]]], "midpoint"))/2
	    attr(zk[[2 - isL]], "members") <- 1L
	    attr(zk[[2 - isL]], "height") <- h0
	    attr(zk[[2 - isL]], "label") <- object$labels[-x[2 - isL]]
	    attr(zk[[2 - isL]], "leaf") <- TRUE
	    z[[X[1 + isL]]] <- NULL
	}
	else {				# two non-leaf nodes
	    x <- as.character(x)
            ## "merge" the two ('earlier') branches:
	    zk <- list(z[[x[1L]]], z[[x[2L]]])
	    attr(zk, "members") <- attr(z[[x[1L]]], "members") +
		attr(z[[x[2L]]], "members")
	    attr(zk, "midpoint") <- (attr(z[[x[1L]]], "members") +
				     attr(z[[x[1L]]], "midpoint") +
				     attr(z[[x[2L]]], "midpoint"))/2
	    z[[x[1L]]] <- z[[x[2L]]] <- NULL
	}
	attr(zk, "height") <- oHgt[k]
	z[[as.character(k)]] <- zk
    }
    structure(z[[as.character(k)]], class = "dendrogram")
}

## Reversing the above (as much as possible)
## is only possible for dendrograms with *binary* splits
as.hclust.dendrogram <- function(x, ...)
{
    stopifnot(is.list(x), length(x) == 2)
    n <- length(ord <- as.integer(unlist(x)))
    iOrd <- sort.list(ord)
    if(!identical(ord[iOrd], seq_len(n)))
	stop(gettextf(
	    "dendrogram entries must be 1,2,..,%d (in any order), to be coercible to \"hclust\"",
	    n), domain=NA)
    stopifnot(n == attr(x, "members"))
    n.h <- n - 1L
    ## labels: not sure, if we'll use this; there should be a faster way!
    labsu <- unlist(labels(x))
    labs <- labsu[iOrd]
    x <- .add.dendrInd(x)

    SIMP <- function(d) {
	if(is.leaf(d)) {
	    - as.vector(d)# dropping attributes
	} else {
	    j <<- j + 1L
	    height[j] <<- attr(d, "height")
	    inds[[j]] <<- attr(d, ".indx.")
	    attributes(d) <- NULL # drop all, incl. class
	    ## recursively apply to components:
	    d[] <- lapply(d, SIMP)
	    d
	}
    }

    height <- numeric(n.h);  inds <- vector("list",n.h);  j <- 0L
    xS <- SIMP(x)
    ii <- sort.list(height) ## FIXME? - May only work if there are no 'inversion's !

    merge <- matrix(NA_integer_, 2L, n.h)
    for(k in seq_len(n.h)) {
	if(k < n.h) { in.k <- inds[[ ii[k] ]] ; s <- xS[[in.k]] } else s <- xS
	if(getOption("as.hclust.dendr", FALSE)) {
	    cat(sprintf("ii[k=%2d]=%2d -> s=xS[[in.k]]=", k, ii[k]))
	    str(s)
	}
	stopifnot(length(s) == 2L, all( vapply(s, is.integer, NA) ))# checking..
	merge[,k] <- unlist(s)
	if(k < n.h)
	    xS[[in.k]] <- + k
    }

    structure(list(merge = t(merge),
		   height = height[ii],
		   order = ord,
		   labels = labs,
		   call = match.call(),
		   method = NA_character_,
		   dist.method = NA_character_),
	      class = "hclust")
}

##' add the c(i1,i2,..) list indices to each non-leaf of a dendrogram
##' --> allowing "random access" into the dendrogram
.add.dendrInd <- function(x)
{
    add.I <- function(x, ind) {
        if(!is.leaf(x)) {
            for(i in seq_along(x))
                x[[i]] <- add.I(x[[i]], c(ind, i))
            attr(x, ".indx.") <- ind
        }
        x
    }
    ## apply recursively:
    add.I(x, integer())
}


### MM: 'FIXME'	 (2002-05-14):
###	 =====
## We currently (mis)use a node's "members" attribute for two things:
## 1) #{sub nodes}
## 2) information about horizontal layout of the given node
## Because of that, cut.dend..() cannot correctly set "members" as it should!

## ==> start using "x.member" and the following function :

.memberDend <- function(x) {
    r <- attr(x,"x.member")
    if(is.null(r)) {
	r <- attr(x,"members")
	if(is.null(r)) r <- 1L
    }
    r
}

.midDend <- function(x)
    if(is.null(mp <- attr(x, "midpoint"))) 0 else mp

midcache.dendrogram <- function (x, type = "hclust", quiet=FALSE)
{
    ## Recompute "midpoint" attributes of a dendrogram, e.g. after reorder().

    type <- match.arg(type) ## currently only "hclust"
    stopifnot( inherits(x, "dendrogram") )
    setmid <- function(d, type) {
	if(is.leaf(d))# no "midpoint"
	    return(d)
	k <- length(d)
	if(k < 1)
	    stop("dendrogram node with non-positive #{branches}")
	r <- d # incl. attributes!
	midS <- 0
	for(j in 1L:k) {
	    r[[j]] <- unclass(setmid(d[[j]], type))
	    midS <- midS + .midDend(r[[j]])
	}
	if(!quiet && type == "hclust" && k != 2)
	    warning("midcache() of non-binary dendrograms only partly implemented")
	## compatible to as.dendrogram.hclust() {MM: doubtful if k > 2}
	attr(r, "midpoint") <- (.memberDend(d[[1L]]) + midS) / 2
	r
    }
    setmid(x, type=type)
}


### Define a very concise print() method for dendrograms:
##  Martin Maechler, 15 May 2002
print.dendrogram <- function(x, digits = getOption("digits"), ...)
{
    cat("'dendrogram' ")
    if(is.leaf(x))
	cat("leaf '", format(attr(x, "label"), digits = digits),"'", sep = "")
    else
	cat("with", length(x), "branches and",
	    attr(x,"members"), "members total")

    cat(", at height", format(attr(x,"height"), digits = digits), "\n")
    invisible(x)
}

str.dendrogram <-
function (object, max.level = NA, digits.d = 3L, give.attr = FALSE,
          wid = getOption("width"), nest.lev = 0, indent.str = "",
          last.str = getOption("str.dendrogram.last"), stem = "--", ...)
{
## TO DO: when object is part of a larger structure which is str()ed
##    with default max.level= NA, it should not be str()ed to all levels,
##   but only to e.g. level 2
## Implement via smarter default for 'max.level' (?)

    pasteLis <- function(lis, dropNam, sep = " = ") {
	## drop uninteresting "attributes" here
	lis <- lis[!(names(lis) %in% dropNam)]
	fl <- sapply(lis, format, digits = digits.d)
	paste(paste(names(fl), fl, sep = sep), collapse = ", ")
    }

    ## when  indent.str  ends in a blank, i.e. "last" (see below)
    istr <- sub(" $", last.str, indent.str)
    cat(istr, stem, sep = "")

    at <- attributes(object)
    memb <- at[["members"]]
    hgt	 <- at[["height"]]
    if(!is.leaf(object)) {
	le <- length(object)
	if(give.attr) {
	    if(nzchar(at <- pasteLis(at, c("class", "height", "members"))))
		at <- paste(",", at)
	}
	cat("[dendrogram w/ ", le, " branches and ", memb, " members at h = ",
            format(hgt, digits = digits.d), if(give.attr) at,
            "]", if(!is.na(max.level) && nest.lev == max.level)" ..", "\n",
            sep = "")
	if (is.na(max.level) || nest.lev < max.level) {
	    for(i in 1L:le) {
		##cat(indent.str, nam.ob[i], ":", sep = "")
		str(object[[i]], nest.lev = nest.lev + 1,
		    indent.str = paste(indent.str, if(i < le) " |" else "  "),
                    last.str = last.str, stem = stem,
		    max.level = max.level, digits.d = digits.d,
		    give.attr = give.attr, wid = wid)
	    }
	}
    } else { ## leaf
	cat("leaf",
	    if(is.character(at$label)) paste("", at$label,"", sep = '"') else
	    format(object, digits = digits.d),"")
	any.at <- hgt != 0
	if(any.at) cat("(h=",format(hgt, digits = digits.d))
	if(memb != 1) #MM: when can this happen?
	    cat(if(any.at)", " else {any.at <- TRUE; "("}, "memb= ", memb, sep = "")
	at <- pasteLis(at, c("class", "height", "members", "leaf", "label"))
	if(any.at || nzchar(at)) cat(if(!any.at)"(", at, ")")
	cat("\n")
    }
    invisible()
}


## The ``generic'' method for "[["  (analogous to e.g., "[[.POSIXct"):
## --> subbranches (including leafs!) are dendrograms as well!
`[[.dendrogram` <- function(x, ..., drop = TRUE) {
    if(!is.null(r <- NextMethod("[[")))
        structure(r, class = "dendrogram")
}

nobs.dendrogram <- function(object, ...) attr(object, "members")

## FIXME: need larger par("mar")[1L] or [4L] for longish labels !
## {probably don't change, just print a warning ..}
plot.dendrogram <-
    function (x, type = c("rectangle", "triangle"), center = FALSE,
	      edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),
	      nodePar = NULL, edgePar = list(),
	      leaflab = c("perpendicular", "textlike", "none"), dLeaf = NULL,
	      xlab = "", ylab = "", xaxt="n", yaxt="s",
	      horiz = FALSE, frame.plot = FALSE, xlim, ylim, ...)
{
    type <- match.arg(type)
    leaflab <- match.arg(leaflab)
    hgt <- attr(x, "height")
    if (edge.root && is.logical(edge.root))
	edge.root <- 0.0625 * if(is.leaf(x)) 1 else hgt
    mem.x <- .memberDend(x)
    yTop <- hgt + edge.root
    if(center) { x1 <- 0.5 ; x2 <- mem.x + 0.5 }
    else       { x1 <- 1   ; x2 <- mem.x }
    xl. <- c(x1 - 1/2, x2 + 1/2)
    yl. <- c(0, yTop)
    if (horiz) {## swap and reverse direction on `x':
	tmp <- xl.; xl. <- rev(yl.); yl. <- tmp
	tmp <- xaxt; xaxt <- yaxt; yaxt <- tmp
    }
    if(missing(xlim) || is.null(xlim)) xlim <- xl.
    if(missing(ylim) || is.null(ylim)) ylim <- yl.
    dev.hold(); on.exit(dev.flush())
    plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab, ylab = ylab,
	 xaxt = xaxt, yaxt = yaxt, frame.plot = frame.plot, ...)
    if(is.null(dLeaf))
        dLeaf <- .75*(if(horiz) strwidth("w") else strheight("x"))

    if (edge.root) {
### FIXME: the first edge + edgetext is drawn here, all others in plotNode()
### -----  maybe use trick with adding a single parent node to the top ?
	x0 <- plotNodeLimit(x1, x2, x, center)$x
	if (horiz)
	    segments(hgt, x0, yTop, x0)
	else segments(x0, hgt, x0, yTop)
	if (!is.null(et <- attr(x, "edgetext"))) {
	    my <- mean(hgt, yTop)
	    if (horiz)
		text(my, x0, et)
	    else text(x0, my, et)
	}
    }
    plotNode(x1, x2, x, type = type, center = center, leaflab = leaflab,
             dLeaf = dLeaf, nodePar = nodePar, edgePar = edgePar, horiz = horiz)
}

### the work horse: plot node (if pch) and lines to all children
plotNode <-
    function(x1, x2, subtree, type, center, leaflab, dLeaf,
	     nodePar, edgePar, horiz = FALSE)
{
  wholetree <- subtree
  depth <- 0L
  llimit <- list()
  KK <- integer()
  kk <- integer()

  repeat {
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    depth <- depth + 1L
    llimit[[depth]] <- bx$limit

    ## handle node specific parameters in "nodePar":
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if(!hasP) nPar <- nodePar

    if(getOption("verbose")) {
	cat(if(inner)"inner node" else "leaf", ":")
	if(!is.null(nPar)) { cat(" with node pars\n"); str(nPar) }
	cat(if(inner )paste(" height", formatC(yTop),"; "),
	    "(x1,x2)= (", formatC(x1, width = 4), ",", formatC(x2, width = 4), ")",
	    "--> xTop=", formatC(xTop, width = 8), "\n", sep = "")
    }

    Xtract <- function(nam, L, default, indx)
	rep(if(nam %in% names(L)) L[[nam]] else default,
	    length.out = indx)[indx]
    asTxt <- function(x) # to allow 'plotmath' labels:
	if(is.character(x) || is.expression(x) || is.null(x)) x else as.character(x)

    i <- if(inner || hasP) 1 else 2 # only 1 node specific par

    if(!is.null(nPar)) { ## draw this node
	pch <- Xtract("pch", nPar, default = 1L:2,	 i)
	cex <- Xtract("cex", nPar, default = c(1,1),	 i)
	col <- Xtract("col", nPar, default = par("col"), i)
	bg <- Xtract("bg", nPar, default = par("bg"), i)
	points(if (horiz) cbind(yTop, xTop) else cbind(xTop, yTop),
	       pch = pch, bg = bg, col = col, cex = cex)
    }

    if(leaflab == "textlike")
        p.col <- Xtract("p.col", nPar, default = "white", i)
    lab.col <- Xtract("lab.col", nPar, default = par("col"), i)
    lab.cex <- Xtract("lab.cex", nPar, default = c(1,1), i)
    lab.font <- Xtract("lab.font", nPar, default = par("font"), i)
    lab.xpd <- Xtract("xpd", nPar, default = c(TRUE, TRUE), i)
    if (is.leaf(subtree)) {
	## label leaf
	if (leaflab == "perpendicular") { # somewhat like plot.hclust
	    if(horiz) {
                X <- yTop + dLeaf * lab.cex
                Y <- xTop; srt <- 0; adj <- c(0, 0.5)
	    }
	    else {
                Y <- yTop - dLeaf * lab.cex
                X <- xTop; srt <- 90; adj <- 1
	    }
            nodeText <- asTxt(attr(subtree,"label"))
	    text(X, Y, nodeText, xpd = lab.xpd, srt = srt, adj = adj,
                 cex = lab.cex, col = lab.col, font = lab.font)
	}
    }
    else if (inner) {
	segmentsHV <- function(x0, y0, x1, y1) {
	    if (horiz)
		segments(y0, x0, y1, x1, col = col, lty = lty, lwd = lwd)
	    else segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
	}
	for (k in seq_along(subtree)) {
	    child <- subtree[[k]]
	    ## draw lines to the children and draw them recursively
	    yBot <- attr(child, "height")
	    if (getOption("verbose")) cat("ch.", k, "@ h=", yBot, "; ")
	    if (is.null(yBot))
		yBot <- 0
	    xBot <-
		if (center) mean(bx$limit[k:(k + 1)])
		else bx$limit[k] + .midDend(child)

	    hasE <- !is.null(ePar <- attr(child, "edgePar"))
	    if (!hasE)
		ePar <- edgePar
	    i <- if (!is.leaf(child) || hasE) 1 else 2
	    ## define line attributes for segmentsHV():
	    col <- Xtract("col", ePar, default = par("col"), i)
	    lty <- Xtract("lty", ePar, default = par("lty"), i)
	    lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
	    if (type == "triangle") {
		segmentsHV(xTop, yTop, xBot, yBot)
	    }
	    else { # rectangle
		segmentsHV(xTop,yTop, xBot,yTop)# h
		segmentsHV(xBot,yTop, xBot,yBot)# v
	    }
	    vln <- NULL
	    if (is.leaf(child) && leaflab == "textlike") {
		nodeText <- asTxt(attr(child,"label"))
		if(getOption("verbose"))
		    cat('-- with "label"',format(nodeText))
		hln <- 0.6 * strwidth(nodeText, cex = lab.cex)/2
		vln <- 1.5 * strheight(nodeText, cex = lab.cex)/2
		rect(xBot - hln, yBot,
		     xBot + hln, yBot + 2 * vln, col = p.col)
		text(xBot, yBot + vln, nodeText, xpd = lab.xpd,
                     cex = lab.cex, col = lab.col, font = lab.font)
	    }
	    if (!is.null(attr(child, "edgetext"))) {
		edgeText <- asTxt(attr(child, "edgetext"))
		if(getOption("verbose"))
		    cat('-- with "edgetext"',format(edgeText))
		if (!is.null(vln)) {
		    mx <-
			if(type == "triangle")
			    (xTop+ xBot+ ((xTop - xBot)/(yTop - yBot)) * vln)/2
			else xBot
		    my <- (yTop + yBot + 2 * vln)/2
		}
		else {
		    mx <- if(type == "triangle") (xTop + xBot)/2 else xBot
		    my <- (yTop + yBot)/2
		}
		## Both for "triangle" and "rectangle" : Diamond + Text

                p.col <- Xtract("p.col", ePar, default = "white", i)
                p.border <- Xtract("p.border", ePar, default = par("fg"), i)
                ## edge label pars: defaults from the segments pars
                p.lwd <- Xtract("p.lwd", ePar, default = lwd, i)
                p.lty <- Xtract("p.lty", ePar, default = lty, i)
                t.col <- Xtract("t.col", ePar, default = col, i)
                t.cex <- Xtract("t.cex", ePar, default =  1,  i)
                t.font <- Xtract("t.font", ePar, default = par("font"), i)

		vlm <- strheight(c(edgeText,"h"), cex = t.cex)/2
		hlm <- strwidth (c(edgeText,"m"), cex = t.cex)/2
		hl3 <- c(hlm[1L], hlm[1L] + hlm[2L], hlm[1L])
                if(horiz) {
                    polygon(my+ c(-hl3, hl3), mx + sum(vlm)*c(-1L:1L, 1L:-1L),
                            col = p.col, border = p.border,
                            lty = p.lty, lwd = p.lwd)
                    text(my, mx, edgeText, cex = t.cex, col = t.col,
                         font = t.font)
                } else {
                    polygon(mx+ c(-hl3, hl3), my + sum(vlm)*c(-1L:1L, 1L:-1L),
                            col = p.col, border = p.border,
                            lty = p.lty, lwd = p.lwd)
                    text(mx, my, edgeText, cex = t.cex, col = t.col,
                         font = t.font)
                }
	    }
	}
    }

    if (inner && length(subtree)) {
	KK[depth] <- length(subtree)
	if (storage.mode(kk) != storage.mode(KK))
	    storage.mode(kk) <- storage.mode(KK)

	## go to first child
	kk[depth] <- 1L
	x1 <- bx$limit[1L]
	x2 <- bx$limit[2L]
	subtree <- subtree[[1L]]
    }
    else {
	repeat {
	    depth <- depth - 1L
	    if (!depth || kk[depth] < KK[depth]) break
	}
	if (!depth) break
	length(kk) <- depth
	kk[depth] <- k <- kk[depth] + 1L
	x1 <- llimit[[depth]][k]
	x2 <- llimit[[depth]][k + 1L]
	subtree <- wholetree[[kk]]
    }
  } ## repeat
  invisible()
}

plotNodeLimit <- function(x1, x2, subtree, center)
{
    ## get the left borders limit[k] of all children k=1..K, and
    ## the handle point `x' for the edge connecting to the parent.
    inner <- !is.leaf(subtree) && x1 != x2
    if(inner) {
	K <- length(subtree)
	mTop <- .memberDend(subtree)
	limit <- integer(K)
	xx1 <- x1
	for(k in 1L:K) {
	    m <- .memberDend(subtree[[k]])
	    ##if(is.null(m)) m <- 1
	    xx1 <- xx1 + (if(center) (x2-x1) * m/mTop else m)
	    limit[k] <- xx1
	}
	limit <- c(x1, limit)
    } else { ## leaf
	limit <- c(x1, x2)
    }
    mid <- attr(subtree, "midpoint")
    center <- center || (inner && !is.numeric(mid))
    x <- if(center) mean(c(x1,x2)) else x1 + (if(inner) mid else 0)
    list(x = x, limit = limit)
}

cut.dendrogram <- function(x, h, ...)
{
    LOWER <- list()
    X <- 1

    assignNodes <- function(subtree, h) {
	if(!is.leaf(subtree)) {
	    if(!(K <- length(subtree)))
		stop("non-leaf subtree of length 0")
	    new.mem <- 0L
	    for(k in 1L:K) {
                sub <- subtree[[k]]
		if(attr(sub, "height") <= h) {
		    ## cut it, i.e. save to LOWER[] and make a leaf
		    at <- attributes(sub)
		    at$leaf <- TRUE
                    at$class <- NULL# drop it from leaf
		    at$x.member <- at$members
		    new.mem <- new.mem + (at$members <- 1L)
		    at$label <- paste("Branch", X)
		    subtree[[k]] <- X #paste("Branch", X)
		    attributes(subtree[[k]]) <- at
		    class(sub) <- "dendrogram"
		    LOWER[[X]] <<- sub
		    X <<- X+1
		}
		else { ## don't cut up here, possibly its children:
		    subtree[[k]] <- assignNodes(sub, h)
		    new.mem <- new.mem + attr(subtree[[k]], "members")
		}
	    }
	    ## re-count members:
	    attr(subtree,"x.member") <- attr(subtree,"members")
	    attr(subtree,"members") <- new.mem
	}
	subtree
    }# assignNodes()

    list(upper = assignNodes(x, h), lower = LOWER)
}## cut.dendrogram()

is.leaf <- function(object) (is.logical(L <- attr(object, "leaf"))) && L

## *Not* a method (yet):
order.dendrogram <- function(x) {
    if( !inherits(x, "dendrogram") )
	stop("'order.dendrogram' requires a dendrogram")
    if(is.list(x))
	unlist(x)
    else ## leaf
	as.vector(x)
}

##RG's first version -- for posterity
# order.dendrogram <- function(x) {
#    if( !inherits(x, "dendrogram") )
#	stop("order.dendrogram requires a dendrogram")
#    ord <- function(x) {
#      if( is.leaf(x) ) return(x[1L])
#      return(c(ord(x[[1L]]), ord(x[[2L]])))
#    }
#   return(ord(x))
# }

reorder <- function(x, ...) UseMethod("reorder")

reorder.dendrogram <- function(x, wts, agglo.FUN = sum, ...)
{
    if( !inherits(x, "dendrogram") )
	stop("'reorder.dendrogram' requires a dendrogram")
    agglo.FUN <- match.fun(agglo.FUN)
    oV <- function(x, wts) {
	if(is.leaf(x)) {
	    attr(x, "value") <- wts[x[1L]]
	    return(x)
	}
        k <- length(x)
        if(k == 0L) stop("invalid (length 0) node in dendrogram")
        vals <- numeric(k)
        for(j in 1L:k) {
            ## insert/compute 'wts' recursively down the branches:
            b <- oV(x[[j]], wts)
            x[[j]] <- b
            vals[j] <- attr(b, "value")
        }
        iOrd <- sort.list(vals)
	attr(x, "value") <- agglo.FUN(vals[iOrd])
        x[] <- x[iOrd]
        x
    }
    midcache.dendrogram( oV(x, wts) )
}

rev.dendrogram <- function(x) {
    if(is.leaf(x))
	return(x)

    k <- length(x)
    if(k < 1)
	stop("dendrogram non-leaf node with non-positive #{branches}")
    r <- x # incl. attributes!
    for(j in 1L:k) ## recurse
	r[[j]] <- rev(x[[k+1-j]])
    midcache.dendrogram( r )
}

## This is cheap
labels.dendrogram <- function(object, ...)
    unlist(dendrapply(object, function(n) attr(n,"label")))

merge.dendrogram <- function(x, y, ..., height,
                             adjust = c("auto", "add.max", "none"))
{
    stopifnot(inherits(x,"dendrogram"), inherits(y,"dendrogram"))
    if((adjust <- match.arg(adjust)) == "auto")
        adjust <-
            ## dendrograms as from hclust(), have entries {1,2,..,n}; "cheap" check:
            if(min(unlist(x)) == 1 && min(unlist(y)) == 1)
                "add.max"
            else # for now, can imagine more:
                "none"
    if(adjust == "add.max") {
        add.ifleaf <- function(i, add) if(is.leaf(i)) i + add else i
        add <- max(unlist(x))
        y <- dendrapply(y, add.ifleaf, add=add)
    }
    r <- list(x,y)
    if(length(xtr <- list(...))) {
	if(!all(is.d <- vapply(xtr, inherits, NA, what="dendrogram"))) {
	    xpr <- substitute(c(...))
	    nms <- sapply(xpr[-1][!is.d], deparse, nlines = 1L)
            ## do not simplify: xgettext needs this form
            msg <- ngettext(length(nms),
                            "extra argument %s is not of class \"%s\"",
                            "extra arguments %s are not of class \"%s\"s")
	    stop(sprintf(msg, paste(nms, collapse=", "), "dendrogram"),
                 domain = NA)
	}
	if(adjust == "add.max") {
	    add <- max(add, unlist(y))
	    for(i in seq_along(xtr)) {
		if(i > 1L) add <- max(add, unlist(xtr[i-1L]))
		xtr[[i]] <- dendrapply(xtr[[i]], add.ifleaf, add=add)
	    }
	}
	r <- c(r, xtr)
    }
    attr(r, "members") <- sum(vapply(r, attr, 0L, which="members"))
    h.max <- max(vapply(r, attr, 0., which="height"))
    if(missing(height) || is.null(height))
	height <- 1.1 * h.max
    else if(height < h.max) {
        msg <- gettextf("'height' must be at least %g, the maximal height of its components", h.max)
        stop(msg, domain = NA)
    }
    attr(r, "height") <- height
    class(r) <- "dendrogram"
    midcache.dendrogram(r, quiet=TRUE)
}

dendrapply <- function(X, FUN, ...)
{
    ## Purpose: "dendrogram" recursive apply {to each node}
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 26 Jun 2004, 22:43
    FUN <- match.fun(FUN)
    if( !inherits(X, "dendrogram") ) stop("'X' is not a dendrogram")

    ## Node apply recursively:
    Napply <- function(d) {
	r <- FUN(d, ...)
	if(!is.leaf(d)) {
	    if(!is.list(r)) r <- as.list(r) # fixing unsafe FUN()s
	    if(length(r) < (n <- length(d))) r[seq_len(n)] <- vector("list", n)
	    ## and overwrite recursively, possibly keeping "attr"
	    r[] <- lapply(d, Napply)
        }
	r
    }
    Napply(X)
}


## original Andy Liaw; modified RG, MM :
heatmap <-
function (x, Rowv=NULL, Colv=if(symm)"Rowv" else NULL,
	  distfun = dist, hclustfun = hclust,
          reorderfun = function(d,w) reorder(d,w),
          add.expr, symm = FALSE, revC = identical(Colv, "Rowv"),
	  scale = c("row", "column", "none"), na.rm=TRUE,
	  margins = c(5, 5), ColSideColors, RowSideColors,
	  cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc),
	  labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL,
	  keep.dendro = FALSE,
	  verbose = getOption("verbose"), ...)
{
    scale <- if(symm && missing(scale)) "none" else match.arg(scale)
    if(length(di <- dim(x)) != 2 || !is.numeric(x))
	stop("'x' must be a numeric matrix")
    nr <- di[1L]
    nc <- di[2L]
    if(nr <= 1 || nc <= 1)
	stop("'x' must have at least 2 rows and 2 columns")
    if(!is.numeric(margins) || length(margins) != 2L)
	stop("'margins' must be a numeric vector of length 2")

    doRdend <- !identical(Rowv,NA)
    doCdend <- !identical(Colv,NA)
    if(!doRdend && identical(Colv, "Rowv")) doCdend <- FALSE
    ## by default order by row/col means
    if(is.null(Rowv)) Rowv <- rowMeans(x, na.rm = na.rm)
    if(is.null(Colv)) Colv <- colMeans(x, na.rm = na.rm)

    ## get the dendrograms and reordering indices

    if(doRdend) {
	if(inherits(Rowv, "dendrogram"))
	    ddr <- Rowv
	else {
	    hcr <- hclustfun(distfun(x))
	    ddr <- as.dendrogram(hcr)
	    if(!is.logical(Rowv) || Rowv)
		ddr <- reorderfun(ddr, Rowv)
	}
	if(nr != length(rowInd <- order.dendrogram(ddr)))
	    stop("row dendrogram ordering gave index of wrong length")
    }
    else rowInd <- 1L:nr

    if(doCdend) {
	if(inherits(Colv, "dendrogram"))
	    ddc <- Colv
	else if(identical(Colv, "Rowv")) {
	    if(nr != nc)
		stop('Colv = "Rowv" but nrow(x) != ncol(x)')
	    ddc <- ddr
	}
	else {
	    hcc <- hclustfun(distfun(if(symm)x else t(x)))
	    ddc <- as.dendrogram(hcc)
	    if(!is.logical(Colv) || Colv)
		ddc <- reorderfun(ddc, Colv)
	}
	if(nc != length(colInd <- order.dendrogram(ddc)))
	    stop("column dendrogram ordering gave index of wrong length")
    }
    else colInd <- 1L:nc

    ## reorder x
    x <- x[rowInd, colInd]

    labRow <-
	if(is.null(labRow))
	    if(is.null(rownames(x))) (1L:nr)[rowInd] else rownames(x)
	else labRow[rowInd]
    labCol <-
	if(is.null(labCol))
	    if(is.null(colnames(x))) (1L:nc)[colInd] else colnames(x)
	else labCol[colInd]

    if(scale == "row") {
	x <- sweep(x, 1L, rowMeans(x, na.rm = na.rm), check.margin=FALSE)
	sx <- apply(x, 1L, sd, na.rm = na.rm)
	x <- sweep(x, 1L, sx, "/", check.margin=FALSE)
    }
    else if(scale == "column") {
	x <- sweep(x, 2L, colMeans(x, na.rm = na.rm), check.margin=FALSE)
	sx <- apply(x, 2L, sd, na.rm = na.rm)
	x <- sweep(x, 2L, sx, "/", check.margin=FALSE)
    }

    ## Calculate the plot layout
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if(doRdend) 1 else 0.05, 4)
    lhei <- c((if(doCdend) 1 else 0.05) + if(!is.null(main)) 0.2 else 0, 4)
    if(!missing(ColSideColors)) { ## add middle row to layout
	if(!is.character(ColSideColors) || length(ColSideColors) != nc)
	    stop("'ColSideColors' must be a character vector of length ncol(x)")
	lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
	lhei <- c(lhei[1L], 0.2, lhei[2L])
    }
    if(!missing(RowSideColors)) { ## add middle column to layout
	if(!is.character(RowSideColors) || length(RowSideColors) != nr)
	    stop("'RowSideColors' must be a character vector of length nrow(x)")
	lmat <- cbind(lmat[,1]+1, c(rep(NA, nrow(lmat)-1), 1), lmat[,2]+1)
	lwid <- c(lwid[1L], 0.2, lwid[2L])
    }
    lmat[is.na(lmat)] <- 0
    if(verbose) {
	cat("layout: widths = ", lwid, ", heights = ", lhei,"; lmat=\n")
	print(lmat)
    }

    ## Graphics `output' -----------------------

    dev.hold(); on.exit(dev.flush())
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    ## draw the side bars
    if(!missing(RowSideColors)) {
	par(mar = c(margins[1L],0, 0,0.5))
	image(rbind(if(revC) nr:1L else 1L:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if(!missing(ColSideColors)) {
	par(mar = c(0.5,0, 0,margins[2L]))
	image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    ## draw the main carpet
    par(mar = c(margins[1L], 0, 0, margins[2L]))
    if(!symm || scale != "none")
	x <- t(x)
    if(revC) { # x columns reversed
	iy <- nr:1
        if(doRdend) ddr <- rev(ddr)
	x <- x[,iy]
    } else iy <- 1L:nr

    image(1L:nc, 1L:nr, x, xlim = 0.5+ c(0, nc), ylim = 0.5+ c(0, nr),
	  axes = FALSE, xlab = "", ylab = "", ...)
    axis(1, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0,
         cex.axis = cexCol)
    if(!is.null(xlab)) mtext(xlab, side = 1, line = margins[1L] - 1.25)
    axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0,
         cex.axis = cexRow)
    if(!is.null(ylab)) mtext(ylab, side = 4, line = margins[2L] - 1.25)

    if (!missing(add.expr))
	eval(substitute(add.expr))

    ## the two dendrograms :
    par(mar = c(margins[1L], 0, 0, 0))
    if(doRdend)
	plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()

    par(mar = c(0, 0, if(!is.null(main)) 1 else 0, margins[2L]))
    if(doCdend)
	plot(ddc,		axes = FALSE, xaxs = "i", leaflab = "none")
    else if(!is.null(main)) frame()

    ## title
    if(!is.null(main)) {
        par(xpd = NA)# {we have room on the left}
        title(main, cex.main = 1.5*op[["cex.main"]])
    }

    invisible(list(rowInd = rowInd, colInd = colInd,
		   Rowv = if(keep.dendro && doRdend) ddr,
		   Colv = if(keep.dendro && doCdend) ddc ))
}
