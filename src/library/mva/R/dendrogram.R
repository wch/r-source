as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

### FIXME: Consider  `hang = -1' or `= 0.1' argument
### -----  as in plot.hclust() --- would be used for "height" construction!
as.dendrogram.hclust <- function(object, ...)
{
    if(is.null(object$labels))
        object$labels <- object$order

    z <- list()
    for(k in 1:nrow(object$merge)) {
        x <- sort(object$merge[k,])
        if(all(x < 0)) {  # two leaves
            zk <- as.list(object$labels[-x])
            attr(zk,"members") <- 2
            attr(zk,"midpoint") <- 0.5
            attr(zk[[1]], "members") <- attr(zk[[2]], "members") <- 1
            attr(zk[[1]], "height") <- attr(zk[[2]], "height") <- 0
            attr(zk[[1]], "leaf") <- attr(zk[[2]], "leaf") <- TRUE 
        }
        else if(x[1] < 0) { # one leaf, one node
            X <- as.character(x)
            zk <- list(object$labels[-x[1]], z[[X[2]]])
            attr(zk,"members") <- attr(z[[X[2]]],"members") + 1
            attr(zk, "midpoint") <- (1 + attr(z[[X[2]]],"midpoint"))/2
            attr(zk[[1]], "members") <- 1
            attr(zk[[1]], "height") <- 0
            attr(zk[[1]], "leaf") <- TRUE 
        }
        else { # two nodes
            x <- as.character(x)
            zk <- list(z[[x[1]]], z[[x[2]]])
            attr(zk,"members") <-
                attr(z[[x[1]]], "members") + attr(z[[x[2]]], "members")
            attr(zk,"midpoint") <-
                (attr(z[[x[1]]], "members")+
                 attr(z[[x[1]]], "midpoint")+
                 attr(z[[x[2]]], "midpoint"))/2
        }
        attr(zk,"height") <- object$height[k]
        z[[k <- as.character(k)]] <- zk
    }
    z <- z[[k]]
    class(z) <- "dendrogram"
    z
}

### MM: `FIXME'  (2002-05-14):
###      =====
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
        cat("leaf", format(x, digits = digits))
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
        cat("leaf", format(object, digits=digits.d),"")
        any.at <- (h <- at$height) != 0
        if(any.at) cat("(h=",format(h, digits=digits.d))
        if((m <- at$members) != 1)
            cat(if(any.at)", " else {any.at <- TRUE; "("}, "memb= ", m, sep="")
        at <- at[!(names(at) %in% c("class", "height", "members"))]
        for(i in seq(along=at))
            cat(if(any.at) "," else {any.at <- TRUE; "("},
                names(at)[i],"=", format(at[[i]], digits=digits.d,wid=wid))
        if(any.at)cat(")")
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

plot.dendrogram <-
    function(x, type = c("rectangle", "triangle"),
             center = FALSE, edge.root= !is.null(attr(x,"edgetext")),
             nodePar = NULL, edgePar = list(),
             xlab = "", ylab = "", horiz = FALSE,
             ...)
{
    type <- match.arg(type)
    hgt <- attr(x, "height")
    if(edge.root && is.logical(edge.root))
        edge.root <- 0.0625 * hgt
    mem.x <- .memberDend(x)
    yTop <- hgt + edge.root
    if(center) { x1 <- 0.5 ; x2 <- mem.x + 0.5 }
    else       { x1 <- 1   ; x2 <- mem.x }
    xlim <- c(x1 - 1/2, x2 + 1/2)
    ylim <- c(0, yTop)
    if(horiz) { ## swap and reverse direction on `x':
        xl <- xlim; xlim <- rev(ylim); ylim <- xl
    }
    plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab, ylab = ylab, ...)
    if(edge.root) {
        x0 <- plotNodeLimit(x1, x2, x, center)$ x
        if(horiz) segments(hgt, x0, yTop, x0) else segments(x0, hgt, x0, yTop)
        if(!is.null(et <- attr(x,"edgetext"))) {
            my <- mean(hgt, yTop)
            if(horiz) text(my,x0, et) else text(x0, my, et)
        }
    }
    plotNode(x1, x2, x, type = type, center = center,
             nodePar = nodePar, edgePar = edgePar, horiz = horiz)
}


### the work horse: plot node (if pch) and lines to all children
plotNode <-
    function(x1, x2, subtree, type, center, nodePar, edgePar, horiz = FALSE)
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
        pch <- Xtract("pch", nPar, default = 1:2,        i)
        cex <- Xtract("cex", nPar, default = c(1,1),     i)
        col <- Xtract("col", nPar, default = par("col"), i)
        bg  <- Xtract("bg",  nPar, default = par("bg"),  i)
        points(if(horiz)cbind(yTop, xTop) else cbind(xTop, yTop),
               pch = pch, bg = bg, col = col, cex = cex)
    }

    if(inner) {
        for(k in 1:length(subtree)) {
            ## draw lines to the children and draw themselves recursively
            yBot <- attr(child <- subtree[[k]], "height")
            if(is.null(yBot)) yBot <- 0
            xBot <-
                if(center) mean(bx$limit[k:(k+1)])
                else {
                    mid <- attr(child,"midpoint")
                    bx$limit[k] + if(is.null(mid)) 0 else mid
                }
            hasE <- !is.null(ePar <- attr(child, "edgePar"))
            if(!hasE) ePar <- edgePar
            i <- if(!isLeaf(child) || hasE) 1 else 2
            col <- Xtract("col", ePar, default = par("col"), i)
            lty <- Xtract("lty", ePar, default = par("lty"), i)
            lwd <- Xtract("lwd", ePar, default = par("lwd"), i)
            segmentsHV <- function(x0, y0, x1, y1) {
                if(horiz) segments(y0, x0, y1, x1, col=col, lty=lty, lwd=lwd)
                else	  segments(x0, y0, x1, y1, col=col, lty=lty, lwd=lwd)
            }
            if(type == "triangle") {
                    segmentsHV(xTop,yTop, xBot,yBot)
            }
            else { # rectangle
                segmentsHV(xTop,yTop, xBot,yTop)# h
                segmentsHV(xBot,yTop, xBot,yBot)# v
            }

            vln <- NULL
            if(!is.null(attr(subtree[[k]],"text"))){
                nodeText <- as.character(attr(subtree[[k]],"text"))
                hln <- (1.2*strwidth(nodeText))/2          
                vln <-1.5*strheight(nodeText)/2
                rect(xBot-hln,yBot,xBot+hln,yBot+2*vln,col="white")
                text(xBot,yBot+vln,nodeText)
            }

            if(!is.null(attr(subtree[[k]],"edgetext"))){
                edgeText <- as.character(attr(subtree[[k]],"edgetext"))
                hlm <- 1.2*strwidth(edgeText)/2
                vlm <- 1.5*strheight(edgeText)/2
                if(!is.null(vln)){
                    mx <- (xTop + xBot + ((xTop-xBot)/(yTop-yBot))*vln)/2
                    my <- (yTop + yBot + 2*vln)/2
                }
                else{
                    my <- (yTop + yBot)/2
                    mx <- (xTop + xBot)/2
                }
                if(type=="triangle"){
                    polygon(c(mx-hlm,mx-1.3*hlm,mx-hlm,
                              mx+hlm,mx+1.3*hlm,mx+hlm),
                            c(my-vlm,my,my+vlm,my+vlm,my,my-vlm),
                            col="white")
                    text(mx,my,edgeText)
                }
                else{
                    polygon(c(xBot-hlm,xBot-1.3*hlm,xBot-hlm,
                              xBot+hlm,xBot+1.3*hlm,xBot+hlm),   
                            c(my-vlm,my,my+vlm,my+vlm,my,my-vlm),
                            col="white") 
                    text(xBot,my,edgeText)
                }
            }
        
            plotNode(bx$limit[k], bx$limit[k+1],
                     subtree = child, type, center, nodePar, edgePar, horiz)
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

isLeaf <- function(object){
    (!is.null(attr(object, "leaf"))) && attr(object, "leaf")
}

    
