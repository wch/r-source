as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

as.dendrogram.hclust <- function (object, hang = -1, ...)
## hang = 0.1  is default for plot.hclust
{
    if (is.null(object$labels))
	object$labels <- (1:length(object$order))
    z <- list()
    nMerge <- length(oHgt <- object$height)
    if (nMerge != nrow(object$merge))
	stop("`merge' and `height' do not fit!")
    hMax <- oHgt[nMerge]
    one <- 1:1;	   two <- 2:2 # integer!
    for (k in 1:nMerge) {
	x <- sort(object$merge[k, ])
	if (any(neg <- x < 0))
	    h0 <- if (hang < 0)
		0
	    else max(0, oHgt[k] - hang * hMax)
	if (all(neg)) {			# two leaves
	    zk <- as.list(rev(-x))
	    attr(zk, "members") <- two
	    attr(zk, "midpoint") <- 0.5
	    objlabels <- rev(object$labels[-x])
	    attr(zk[[1]], "label") <- objlabels[1]
	    attr(zk[[2]], "label") <- objlabels[2]
	    attr(zk[[1]], "members") <- attr(zk[[2]], "members") <- one
	    attr(zk[[1]], "height") <- attr(zk[[2]], "height") <- h0
	    attr(zk[[1]], "leaf") <- attr(zk[[2]], "leaf") <- TRUE
	}
	else if (any(neg)) {		# one leaf, one node
	    X <- as.character(x)
	    ## For original "hclust" node ordering, the leaf is always left,
	    ## i.e. x[1]; don't want to assume this
	    isL <- x[1] < 0 ##is leaf left?
	    zk <-
		if(isL) list(-x[1], z[[X[2]]])
		else	list(z[[X[1]]], object$labels[-x[2]])
	    attr(zk, "members") <- attr(z[[X[1 + isL]]], "members") + one
	    attr(zk, "midpoint") <- (1 + attr(z[[X[1 + isL]]], "midpoint"))/2
	    attr(zk[[2 - isL]], "members") <- one
	    attr(zk[[2 - isL]], "height") <- h0
	    attr(zk[[2 - isL]], "label") <- object$labels[-x[1]]
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

### Define a very concise print() method for dendrograms:
##  Martin Maechler, 15 May 2002
print.dendrogram <- function(x, digits = getOption("digits"), ...)
{
    cat("`dendrogram' ")
    if(inherits(x, "dendrogramLeaf"))
	cat("leaf", format(attr(x, "label"), digits = digits))
    else
	cat("with", length(x), "branches and",
	    attr(x,"members"), "members total")

    cat(", at height", format(attr(x,"height"), digits = digits), "\n")
    invisible(x)
}

str.dendrogram <-
function (object, max.level = 0, vec.len = 4, digits.d = 3, nchar.max = 128,
	  give.attr = TRUE, give.length = TRUE, wid = getOption("width"),
	  nest.lev = 0, indent.str = "", ...)
{
    nind <- nchar(istr <- indent.str)
    if(substr(istr, nind,nind) == " ")
       substr(istr, nind,nind) <- "`"
    cat(istr, "--", sep="")
    if(!isLeaf(object)) {
	le <- length(object)
	cat("[dendrogram w/", le, "branches and", attr(object,"members"),
	    "members at h =", format(attr(object,"height"), digits=digits.d),
	    "]\n")
	if (max.level==0 || nest.lev < max.level) {
	    for(i in 1:le) {
		##cat(indent.str, nam.ob[i], ":", sep="")
		str(object[[i]], nest.lev = nest.lev + 1,
		    indent.str= paste(indent.str, if(i < le) " |" else "  "),
		    nchar.max=nchar.max,
		    max.level=max.level, vec.len=vec.len, digits.d=digits.d,
		    give.attr= give.attr, give.length= give.length, wid=wid)
	    }
	}
    } else { ## leaf
	at <- attributes(object)
	cat("leaf",
	    if(is.character(at$label)) paste("",at$label,"",sep='"') else
	    format(object, digits=digits.d),"")
	any.at <- (h <- at$height) != 0
	if(any.at) cat("(h=",format(h, digits=digits.d))
	if((m <- at$members) != 1) #MM: when can this happen?
	    cat(if(any.at)", " else {any.at <- TRUE; "("}, "memb= ", m, sep="")
	## drop `leaf = TRUE' which we know here
	at <- at[!(names(at) %in% c("class", "height", "leaf", "members"))]
	at$label <- NULL
	for(i in seq(along=at))
	    cat(if(any.at) "," else {any.at <- TRUE; "("},
		names(at)[i],"=", format(at[[i]], digits=digits.d,wid=wid))
	if(any.at) cat(")")
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


## MM: new "flag"  `leaflab' = c("perpendicular", "textlike","none")

### CARE : THIS starts labeling cluster dendrograms !!!
### ==== : where it didn't before

## Also: need larger par("mar")[1] or [4] for longish labels !
## [probably don't change, just print a warnig or so !!
plot.dendrogram <-
    function (x, type = c("rectangle", "triangle"), center = FALSE,
	      edge.root = !is.null(attr(x, "edgetext")), nodePar = NULL,
	      xaxt="n", yaxt="s",
	      edgePar = list(), leaflab= c("perpendicular", "textlike", "none"),
	      xlab = "", ylab = "", horiz = FALSE, ...)
{
    type <- match.arg(type)
    leaflab <- match.arg(leaflab)
    hgt <- attr(x, "height")
    if (edge.root && is.logical(edge.root))
	edge.root <- 0.0625 * hgt
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
	 xaxt = xaxt, yaxt = yaxt, ...)
    if (edge.root) {
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
	     nodePar = nodePar, edgePar = edgePar, horiz = horiz)
}

### the work horse: plot node (if pch) and lines to all children
plotNode <-
    function(x1, x2, subtree, type, center, leaflab,
             nodePar, edgePar, horiz = FALSE)
{
    inner <- !isLeaf(subtree) && x1 != x2
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
	rep(if(any(nam == names(L))) L[[nam]] else default, length = indx)[indx]

    if(!is.null(nPar)) { ## draw this node
	i <- if(inner || hasP) 1 else 2 # only 1 node specific par
	pch <- Xtract("pch", nPar, default = 1:2,	 i)
	cex <- Xtract("cex", nPar, default = c(1,1),	 i)
	col <- Xtract("col", nPar, default = par("col"), i)
	bg <- Xtract("bg", nPar, default = par("bg"), i)
	points(if (horiz) cbind(yTop, xTop) else cbind(xTop, yTop),
	       pch = pch, bg = bg, col = col, cex = cex)
    }
    if (isLeaf(subtree)) {
	## label leaf
	if (leaflab == "perpendicular") { # somewhat like plot.hclust
	    if(horiz) {
	       X <- yTop; Y <- xTop; srt <- 0; adj <- c(0, 0.5)
	    }
	    else{
	       X <- xTop; Y <- yTop; srt <- 90; adj <- 1
	    }
	    text(X, Y, attr(subtree,"label"), xpd = TRUE, srt = srt, adj = adj)
	}
    }
    else if (inner) {
	for (k in 1:length(subtree)) {
	    child <- subtree[[k]]
	    ## draw lines to the children and draw themselves recursively
	    yBot <- attr(child, "height")
	    if (getOption("verbose"))
		cat("ch.", k, "@ h=", yBot, "; ")
	    if (is.null(yBot))
		yBot <- 0
	    xBot <- if (center)
		mean(bx$limit[k:(k + 1)])
	    else {
		mid <- attr(child, "midpoint")
		bx$limit[k] + if (is.null(mid)) 0 else mid
	    }
	    hasE <- !is.null(ePar <- attr(child, "edgePar"))
	    if (!hasE)
		ePar <- edgePar
	    i <- if (!isLeaf(child) || hasE) 1 else 2
	    col <- Xtract("col", ePar, default = par("col"), i)
	    lty <- Xtract("lty", ePar, default = par("lty"), i)
	    lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
	    segmentsHV <- function(x0, y0, x1, y1) {
		if (horiz)
		     segments(y0, x0, y1, x1, col = col, lty = lty, lwd = lwd)
		else segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
	    }
	    if (type == "triangle") {
		segmentsHV(xTop, yTop, xBot, yBot)
	    }
	    else { # rectangle
		segmentsHV(xTop,yTop, xBot,yTop)# h
		segmentsHV(xBot,yTop, xBot,yBot)# v
	    }
	    vln <- NULL
	    if ((isLeaf(child) && leaflab == "textlike") ||
		!is.null(attr(child, "text"))) {
		nodeText <-
		    if(isLeaf(child)) attr(child, "label")
		    else as.character(attr(child,"text"))
		if(getOption("verbose")) cat('-- with "text"',nodeText)
		hln <- 0.6 * strwidth(nodeText)/2
		vln <- 1.5 * strheight(nodeText)/2
		rect(xBot - hln, yBot,
		     xBot + hln, yBot + 2 * vln, col = "white")
		text(xBot, yBot + vln, nodeText)
	    }
	    if (!is.null(attr(child, "edgetext"))) {
		edgeText <- as.character(attr(child, "edgetext"))
		if(getOption("verbose")) cat('-- with "edgetext"',edgeText)
		hlm <- 1.2 * strwidth(edgeText)/2
		vlm <- 1.5 * strheight(edgeText)/2
		if (!is.null(vln)) {
		  mx <- (xTop + xBot + ((xTop - xBot)/(yTop - yBot)) * vln)/2
		  my <- (yTop + yBot + 2 * vln)/2
		}
		else {
		  mx <- (xTop + xBot)/2
		  my <- (yTop + yBot)/2
	      }
		if (type == "triangle") {
		    polygon(c(mx - hlm, mx - 1.3*hlm, mx - hlm,
			      mx + hlm, mx + 1.3*hlm, mx + hlm),
			    c(my - vlm, my, my + vlm, my + vlm, my, my - vlm),
			    col = "white")
		  text(mx, my, edgeText)
		}
		else {
		  polygon(c(xBot - hlm, xBot - 1.3*hlm, xBot - hlm,
			    xBot + hlm, xBot + 1.3*hlm, xBot + hlm),
			  c(my - vlm, my, my + vlm, my + vlm, my, my - vlm),
			  col = "white")
		  text(xBot, my, edgeText)
		}
	    }
	    plotNode(bx$limit[k], bx$limit[k + 1], subtree = child,
		type, center, leaflab, nodePar, edgePar, horiz)
	}
    }
}

plotNodeLimit <- function(x1, x2, subtree, center)
{
    ## get the left borders limit[k] of all children k=1..K, and
    ## the handle point `x' for the edge connecting to the parent.
    inner <- !isLeaf(subtree) && x1 != x2
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
    x <-
	if(center) mean(c(x1,x2))
	else x1 + (if(inner) attr(subtree, "midpoint") else 0)
    list(x = x, limit = limit)
}

cut.dendrogram <- function(x, h, ...)
{
    LOWER <- list()
    X <- 1

    assignNodes <- function(subtree, h) {
	if(!isLeaf(subtree)) {
	    if(!(K <- length(subtree)))
		warning("`subtree' of length 0 !!")
	    new.mem <- 0
	    for(k in 1:K) {
		if(attr(subtree[[k]], "height") <= h) {
		    ## cut it, i.e. save to LOWER[] and make a leaf
		    sub <- subtree[[k]]
		    at <- attributes(sub)
		    at$leaf <- TRUE
		    at$x.member <- at$members
		    new.mem <- new.mem + (at$members <- 1)
		    at$label <- paste("Branch", X)
		    subtree[[k]] <- paste("Branch", X)
		    attributes(subtree[[k]]) <- at
		    class(sub) <- "dendrogram"
		    LOWER[[X]] <<- sub
		    X <<- X+1
		}
		else { ## don't cut up here, possibly its children:
		    subtree[[k]] <- assignNodes(subtree[[k]], h)
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

isLeaf <- function(object) (is.logical(L <- attr(object, "leaf"))) && L


## *Not* a method (yet):
order.dendrogram <- function(x) {
    if( !inherits(x, "dendrogram") )
        stop("order.dendrogram requires a dendrogram")
    unlist(x)
}

##RG's first version -- for posterity
# order.dendrogram <- function(x) {
#    if( !inherits(x, "dendrogram") )
#       stop("order.dendrogram requires a dendrogram")
#    ord <- function(x) {
#      if( isLeaf(x) ) return(x[1])
#      return(c(ord(x[[1]]), ord(x[[2]])))
#    }
#   return(ord(x))
# }

reorder <- function(x, ...)
    UseMethod("reorder")

reorder.dendrogram <- function(x, wts, ...)
{
    if( !inherits(x, "dendrogram") )
        stop("we require a dendrogram")
    oV <- function(x, wts) {
        if( isLeaf(x) ) {
            attr(x, "value") <- wts[x[1]]
            return(x)
        }
        left  <- oV(x[[1]], wts)
        right <- oV(x[[2]], wts)
        lV <- attr(left, "value")
        rV <- attr(right, "value")
        attr(x, "value") <- lV+rV
        if( lV > rV ) {
            x[[1]] <- right
            x[[2]] <- left
        } else {
            x[[1]] <- left
            x[[2]] <- right
        }
        x
    }
    oV(x, wts)
}


## original Andy Liaw; modified RG, MM

heatmap <-
function (x, Rowv, Colv, distfun = dist, hclustfun = hclust, add.expr,
         scale = c("row", "column", "none"), na.rm=TRUE, ...)
{
    scale <- match.arg(scale)
    if(length(di <- dim(x)) != 2 || !is.numeric(x))
	stop("`x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if(nr <= 1 || nc <= 1)
	stop("`x' must have at least 2 rows and 2 columns")
    r.cex <- 0.2 + 1/log10(nr)
    c.cex <- 0.2 + 1/log10(nc)
    ## by default order by row/col means
    if( missing(Rowv) )
	Rowv <- rowMeans(x, na.rm=na.rm)
    if( missing(Colv) )
	Colv <- colMeans(x, na.rm=na.rm)
    ## get the dendrograms
    if( !inherits(Rowv, "dendrogram") ) {
	hcr <- hclustfun(distfun(x))
	ddr <- as.dendrogram(hcr)
	ddr <- reorder(ddr, Rowv)
    }
    else
	ddr <- Rowv
    if( !inherits(Colv, "dendrogram") ) {
	hcc <- hclustfun(distfun(t(x)))
	ddc <- as.dendrogram(hcc)
	ddc <- reorder(ddc, Colv)
    }
    else
	ddc <- Colv
    rowInd <- order.dendrogram(ddr)
    colInd <- order.dendrogram(ddc)
    ## reorder x
    x <- x[rowInd, colInd]
    if(scale == "row") {
	x <- sweep(x, 1, rowMeans(x, na.rm=na.rm))
	sd <- apply(x, 1, sd, na.rm=na.rm)
	x <- sweep(x, 1, sd, "/")
    }
    else if(scale == "column") {
	x <- sweep(x, 2, colMeans(x, na.rm=na.rm))
	sd <- apply(x, 2, sd, na.rm=na.rm)
	x <- sweep(x, 2, sd, "/")
    }
    ## Graphics :
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(matrix(c(0, 3, 2, 1), 2, 2, byrow = TRUE),
	   widths = c(1,4), heights = c(1, 4), respect = TRUE)
    par(mar = c(5, 0, 0, 5))
    image(1:ncol(x), 1:nrow(x), t(x), axes = FALSE,
	  xlim = c(0.5, ncol(x) + 0.5), ylim = c(0.5, nrow(x) + 0.5),
	  xlab = "",ylab = "", ...)
    axis(1, 1:ncol(x), las = 2, line = -0.5, tick = 0,
	 labels = if(is.null(colnames(x))) (1:ncol(x))[colInd] else colnames(x),
	 cex.axis = c.cex)
    axis(4, 1:nrow(x), las = 2, line = -0.5, tick = 0,
	 labels = if(is.null(rownames(x))) (1:nrow(x))[rowInd] else rownames(x),
	 cex.axis = r.cex)
    if (!missing(add.expr))
	eval(substitute(add.expr))
    ## the two dendrograms :
    par(mar = c(5, 3, 0, 0))
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs= "i", leaflab= "none")
    par(mar = c(0, 0, 3, 5))
    plot(ddc,               axes = FALSE, xaxs= "i", leaflab= "none")
    invisible(list(rowInd = rowInd, colInd = colInd))
}
