as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

as.dendrogram.hclust <- function (object, hang = -1, ...)
## hang = 0.1  is default for plot.hclust
{
    stopifnot(length(object$order) > 0)
    if (is.null(object$labels))
	object$labels <- 1:length(object$order)
    z <- list()
    nMerge <- length(oHgt <- object$height)
    if (nMerge != nrow(object$merge))
	stop("`merge' and `height' do not fit!")
    hMax <- oHgt[nMerge]
    one <- 1:1;	   two <- 2:2 # integer!
    for (k in 1:nMerge) {
	x <- object$merge[k, ]# no sort() anymore!
	if (any(neg <- x < 0))
	    h0 <- if (hang < 0) 0 else max(0, oHgt[k] - hang * hMax)
	if (all(neg)) {			# two leaves
	    zk <- as.list(-x)
	    attr(zk, "members") <- two
	    attr(zk, "midpoint") <- 0.5 # mean( c(0,1) )
	    objlabels <- object$labels[-x]
	    attr(zk[[1]], "label") <- objlabels[1]
	    attr(zk[[2]], "label") <- objlabels[2]
	    attr(zk[[1]], "members") <- attr(zk[[2]], "members") <- one
	    attr(zk[[1]], "height") <- attr(zk[[2]], "height") <- h0
	    attr(zk[[1]], "leaf") <- attr(zk[[2]], "leaf") <- TRUE
	}
	else if (any(neg)) {		# one leaf, one node
	    X <- as.character(x)
	    ## Originally had "x <- sort(..) above => leaf always left, x[1];
	    ## don't want to assume this
	    isL <- x[1] < 0 ## is leaf left?
	    zk <-
		if(isL) list(-x[1], z[[X[2]]])
		else	list(z[[X[1]]], -x[2])
	    attr(zk, "members") <- attr(z[[X[1 + isL]]], "members") + one
	    attr(zk, "midpoint") <-
                (.memberDend(zk[[1]]) + attr(z[[X[1 + isL]]], "midpoint"))/2
	    attr(zk[[2 - isL]], "members") <- one
	    attr(zk[[2 - isL]], "height") <- h0
	    attr(zk[[2 - isL]], "label") <- object$labels[-x[2 - isL]]
	    attr(zk[[2 - isL]], "leaf") <- TRUE
	}
	else {				# two nodes
	    x <- as.character(x)
	    zk <- list(z[[x[1]]], z[[x[2]]])
	    attr(zk, "members") <- attr(z[[x[1]]], "members") +
		attr(z[[x[2]]], "members")
	    attr(zk, "midpoint") <- (attr(z[[x[1]]], "members") +
				     attr(z[[x[1]]], "midpoint") +
				     attr(z[[x[2]]], "midpoint"))/2
	}
	attr(zk, "height") <- oHgt[k]
	z[[k <- as.character(k)]] <- zk
    }
    z <- z[[k]]
    class(z) <- "dendrogram"
    z
}

### MM: `FIXME'	 (2002-05-14):
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
	if(is.null(r)) r <- 1:1
    }
    r
}

.midDend <- function(x)
    if(is.null(mp <- attr(x, "midpoint"))) 0 else mp

midcache.dendrogram <- function (x, type = "hclust")
{
    ## Recompute "midpoint" attributes of a dendrogram, e.g. after reorder().

    type <- match.arg(type) ## currently only "hclust"
    if( !inherits(x, "dendrogram") )
	stop("we require a dendrogram")
    setmid <- function(d, type) {
	if(is.leaf(d))# no "midpoint"
	    return(d)
	k <- length(d)
	if(k < 1)
	    stop("dendrogram node with non-positive #{branches}")
	r <- d # incl. attributes!
	midS <- 0
	for(j in 1:k) {
	    r[[j]] <- unclass(setmid(d[[j]], type))
	    midS <- midS + .midDend(r[[j]])
	}
	if(type == "hclust" && k != 2)
	    warning("midcache() of non-binary dendrograms only partly implemented")
	## compatible to as.dendrogram.hclust() {MM: doubtful if k > 2}
	attr(r, "midpoint") <- (.memberDend(d[[1]]) + midS) / 2
	r
    }
    setmid(x, type=type)
}


### Define a very concise print() method for dendrograms:
##  Martin Maechler, 15 May 2002
print.dendrogram <- function(x, digits = getOption("digits"), ...)
{
    cat("`dendrogram' ")
    if(is.leaf(x))
	cat("leaf '", format(attr(x, "label"), digits = digits),"'", sep='')
    else
	cat("with", length(x), "branches and",
	    attr(x,"members"), "members total")

    cat(", at height", format(attr(x,"height"), digits = digits), "\n")
    invisible(x)
}

str.dendrogram <-
function (object, max.level = 0, digits.d = 3, give.attr = FALSE,
          wid = getOption("width"), nest.lev = 0, indent.str = "",
          stem = "--", ...)
{
## MM: Maybe improve this :
## -- e.g. when 'object' is part of a larger structure which *is* str()ed
## with default max.level= 0,  the dendrogram shouldn't be str()ed to all levels

    ## FIXME: `wid' argument is currently disregarded
    pasteLis <- function(lis, dropNam, sep = " = ") {
	## drop uninteresting "attributes" here
	lis <- lis[!(names(lis) %in% dropNam)]
	fl <- sapply(lis, format, digits=digits.d, wid=wid)
	paste(paste(names(fl), fl, sep=sep), collapse = ", ")
    }

    nind <- nchar(istr <- indent.str)
    if(substr(istr, nind,nind) == " ")
       substr(istr, nind,nind) <- "`"
    cat(istr, stem, sep="")

    at <- attributes(object)
    memb <- at[["members"]]
    hgt	 <- at[["height"]]
    if(!is.leaf(object)) {
	le <- length(object)
	if(give.attr) {
	    if(nchar(at <- pasteLis(at, c("class", "height", "members"))))
		at <- paste(",", at)
	}
	cat("[dendrogram w/ ", le, " branches and ", memb, " members at h = ",
            format(hgt, digits=digits.d), if(give.attr) at,
            "]", if(max.level > 0 && nest.lev == max.level)" ..", "\n", sep="")
	if (max.level==0 || nest.lev < max.level) {
	    for(i in 1:le) {
		##cat(indent.str, nam.ob[i], ":", sep="")
		str(object[[i]], nest.lev = nest.lev + 1,
		    indent.str= paste(indent.str, if(i < le) " |" else "  "),
		    max.level=max.level, digits.d=digits.d,
		    give.attr= give.attr, wid=wid)
	    }
	}
    } else { ## leaf
	cat("leaf",
	    if(is.character(at$label)) paste("",at$label,"",sep='"') else
	    format(object, digits=digits.d),"")
	any.at <- hgt != 0
	if(any.at) cat("(h=",format(hgt, digits=digits.d))
	if(memb != 1) #MM: when can this happen?
	    cat(if(any.at)", " else {any.at <- TRUE; "("}, "memb= ",memb,sep="")
	at <- pasteLis(at, c("class", "height", "members", "leaf", "label"))
	if(any.at || nchar(at)) cat(if(!any.at)"(", at, ")")
	cat("\n")
    }
}


## The ``generic'' method for "[["  (identical to e.g., "[[.POSIXct"):
## --> subbranches are dendrograms as well!
"[[.dendrogram" <- function(x, ..., drop = TRUE)
{
    cl <- class(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}


## FIXME: need larger par("mar")[1] or [4] for longish labels !
## {probably don't change, just print a warning ..}
plot.dendrogram <-
    function (x, type = c("rectangle", "triangle"), center = FALSE,
	      edge.root = is.leaf(x) || !is.null(attr(x, "edgetext")),
	      nodePar = NULL, edgePar = list(),
	      leaflab = c("perpendicular", "textlike", "none"), dLeaf = NULL,
	      xlab = "", ylab = "", xaxt="n", yaxt="s",
	      horiz = FALSE, frame.plot = FALSE, ...)
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
    xlim <- c(x1 - 1/2, x2 + 1/2)
    ylim <- c(0, yTop)
    if (horiz) {## swap and reverse direction on `x':
	xl <- xlim; xlim <- rev(ylim); ylim <- xl
	tmp <- xaxt; xaxt <- yaxt; yaxt <- tmp
    }
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
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x

    ## handle node specific parameters in "nodePar":
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if(!hasP) nPar <- nodePar

    if(getOption("verbose")) {
	cat(if(inner)"inner node" else "leaf", ":")
	if(!is.null(nPar)) { cat(" with node pars\n"); str(nPar) }
	cat(if(inner)paste(" height", formatC(yTop),"; "),
	    "(x1,x2)= (",formatC(x1,wid=4),",",formatC(x2,wid=4),")",
	    "--> xTop=", formatC(xTop, wid=8),"\n", sep="")
    }

    Xtract <- function(nam, L, default, indx)
	rep(if(nam %in% names(L)) L[[nam]] else default,
	    length.out = indx)[indx]
    asTxt <- function(x) # to allow 'plotmath' labels:
	if(is.character(x) || is.expression(x) || is.null(x)) x else as.character(x)

    i <- if(inner || hasP) 1 else 2 # only 1 node specific par

    if(!is.null(nPar)) { ## draw this node
	pch <- Xtract("pch", nPar, default = 1:2,	 i)
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
	    text(X, Y, nodeText, xpd = TRUE, srt = srt, adj = adj,
                 cex = lab.cex, col = lab.col, font = lab.font)
	}
    }
    else if (inner) {
	segmentsHV <- function(x0, y0, x1, y1) {
	    if (horiz)
		segments(y0, x0, y1, x1, col = col, lty = lty, lwd = lwd)
	    else segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
	}
	for (k in 1:length(subtree)) {
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
		text(xBot, yBot + vln, nodeText, xpd = TRUE,
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
                t.font<- Xtract("t.font",ePar, default= par("font"), i)

		vlm <- strheight(c(edgeText,"h"), cex = t.cex)/2
		hlm <- strwidth (c(edgeText,"m"), cex = t.cex)/2
		hl3 <- c(hlm[1], hlm[1] + hlm[2], hlm[1])
                polygon(mx+ c(-hl3, hl3), my + sum(vlm)*c(-1:1,1:-1),
                        col = p.col, border= p.border, lty = p.lty, lwd = p.lwd)
		text(mx, my, edgeText, cex = t.cex, col = t.col, font = t.font)
	    }
	    plotNode(bx$limit[k], bx$limit[k + 1], subtree = child,
		     type, center, leaflab, dLeaf, nodePar, edgePar, horiz)
	}
    }
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
	for(k in 1:K) {
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
		stop("non leaf ", sQuote("subtree"), " of length 0 !!")
	    new.mem <- 0:0
	    for(k in 1:K) {
                sub <- subtree[[k]]
		if(attr(sub, "height") <= h) {
		    ## cut it, i.e. save to LOWER[] and make a leaf
		    at <- attributes(sub)
		    at$leaf <- TRUE
                    at$class <- NULL# drop it from leaf
		    at$x.member <- at$members
		    new.mem <- new.mem + (at$members <- 1:1)
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
	stop("order.dendrogram requires a dendrogram")
    unlist(x)
}

##RG's first version -- for posterity
# order.dendrogram <- function(x) {
#    if( !inherits(x, "dendrogram") )
#	stop("order.dendrogram requires a dendrogram")
#    ord <- function(x) {
#      if( is.leaf(x) ) return(x[1])
#      return(c(ord(x[[1]]), ord(x[[2]])))
#    }
#   return(ord(x))
# }

reorder <- function(x, ...) UseMethod("reorder")

reorder.dendrogram <- function(x, wts, agglo.FUN = sum, ...)
{
    if( !inherits(x, "dendrogram") )
	stop("we require a dendrogram")
    agglo.FUN <- match.fun(agglo.FUN)
    oV <- function(x, wts) {
	if(is.leaf(x)) {
	    attr(x, "value") <- wts[x[1]]
	    return(x)
	}
        k <- length(x)
        if(k == 0) stop("invalid (length 0) node in dendrogram")
        vals <- numeric(k)
        for(j in 1:k) {
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
    for(j in 1:k) ## recurse
	r[[j]] <- rev(x[[k+1-j]])
    midcache.dendrogram( r )
}

## This is a cheap
labels.dendrogram <- function(object, ...)
    unlist(dendrapply(object, function(n) attr(n,"label")))


dendrapply <- function(X, FUN, ...)
{
    ## Purpose: "dendrogram" apply {to each node}
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 26 Jun 2004, 22:43
    FUN <- match.fun(FUN)
    if( !inherits(X, "dendrogram") ) stop("'X' is not a dendrogram")

    ## Node apply recursively:
    Napply <- function(d) {
	r <- FUN(d, ...)
	if(!is.leaf(d)) {
            if(!is.list(r)) r <- as.list(r) # fixing unsafe FUN()s
            for(j in seq(length = length(d)))
                r[[j]] <- Napply(d[[j]])
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
	stop("`x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if(nr <= 1 || nc <= 1)
	stop("`x' must have at least 2 rows and 2 columns")
    if(!is.numeric(margins) || length(margins) != 2)
	stop("`margins' must be a numeric vector of length 2")

    doRdend <- !identical(Rowv,NA)
    doCdend <- !identical(Colv,NA)
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
    else rowInd <- 1:nr

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
    else colInd <- 1:nc

    ## reorder x
    x <- x[rowInd, colInd]

    labRow <-
	if(is.null(labRow))
	    if(is.null(rownames(x))) (1:nr)[rowInd] else rownames(x)
	else labRow[rowInd]
    labCol <-
	if(is.null(labCol))
	    if(is.null(colnames(x))) (1:nc)[colInd] else colnames(x)
	else labCol[colInd]

    if(scale == "row") {
	x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
	sx <- apply(x, 1, sd, na.rm = na.rm)
	x <- sweep(x, 1, sx, "/")
    }
    else if(scale == "column") {
	x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
	sx <- apply(x, 2, sd, na.rm = na.rm)
	x <- sweep(x, 2, sx, "/")
    }

    ## Calculate the plot layout
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if(doRdend) 1 else 0.05, 4)
    lhei <- c((if(doCdend) 1 else 0.05) + if(!is.null(main)) 0.2 else 0, 4)
    if(!missing(ColSideColors)) { ## add middle row to layout
	if(!is.character(ColSideColors) || length(ColSideColors) != nc)
	    stop("'ColSideColors' must be a character vector of length ncol(x)")
	lmat <- rbind(lmat[1,]+1, c(NA,1), lmat[2,]+1)
	lhei <- c(lhei[1], 0.2, lhei[2])
    }
    if(!missing(RowSideColors)) { ## add middle column to layout
	if(!is.character(RowSideColors) || length(RowSideColors) != nr)
	    stop("'RowSideColors' must be a character vector of length nrow(x)")
	lmat <- cbind(lmat[,1]+1, c(rep(NA, nrow(lmat)-1), 1), lmat[,2]+1)
	lwid <- c(lwid[1], 0.2, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
    if(verbose) {
	cat("layout: widths = ", lwid, ", heights = ", lhei,"; lmat=\n")
	print(lmat)
    }

    ## Graphics `output' -----------------------

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    ## draw the side bars
    if(!missing(RowSideColors)) {
	par(mar = c(margins[1],0, 0,0.5))
	image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if(!missing(ColSideColors)) {
	par(mar = c(0.5,0, 0,margins[2]))
	image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    ## draw the main carpet
    par(mar = c(margins[1], 0, 0, margins[2]))
    if(!symm || scale != "none")
	x <- t(x)
    if(revC) { # x columns reversed
	iy <- nr:1
	ddr <- rev(ddr)
	x <- x[,iy]
    } else iy <- 1:nr

    image(1:nc, 1:nr, x, xlim = 0.5+ c(0, nc), ylim = 0.5+ c(0, nr),
	  axes = FALSE, xlab = "", ylab = "", ...)
    axis(1, 1:nc, labels= labCol, las= 2, line= -0.5, tick= 0, cex.axis= cexCol)
    if(!is.null(xlab)) mtext(xlab, side = 1, line = margins[1] - 1.25)
    axis(4, iy, labels= labRow, las= 2, line= -0.5, tick= 0, cex.axis= cexRow)
    if(!is.null(ylab)) mtext(ylab, side = 4, line = margins[2] - 1.25)

    if (!missing(add.expr))
	eval(substitute(add.expr))

    ## the two dendrograms :
    par(mar = c(margins[1], 0, 0, 0))
    if(doRdend)
	plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()

    par(mar = c(0, 0, if(!is.null(main)) 1 else 0, margins[2]))
    if(doCdend)
	plot(ddc,		axes = FALSE, xaxs = "i", leaflab = "none")
    else if(!is.null(main)) frame()

    ## title
    if(!is.null(main)) title(main, cex.main = 1.5*op[["cex.main"]])

    invisible(list(rowInd = rowInd, colInd = colInd,
		   Rowv = if(keep.dendro && doRdend) ddr,
		   Colv = if(keep.dendro && doCdend) ddc ))
}
