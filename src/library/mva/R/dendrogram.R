as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

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
            attr(zk[[1]],  "height") <- attr(zk[[2]], "height") <- 0

        }
        else if(x[1] < 0) { # one leave, one node
            X <- as.character(x)
            zk <- list(object$labels[-x[1]], z[[X[2]]])
            attr(zk,"members") <- attr(z[[X[2]]],"members") + 1
            attr(zk, "midpoint") <- (1 + attr(z[[X[2]]],"midpoint"))/2
            attr(zk[[1]], "members") <- 1
            attr(zk[[1]], "height") <- 0
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
    if(is.recursive(x))
        cat("with", length(x), "branches and",
            attr(x,"members"), "members total")
    else cat("leaf", format(x, digits = digits))
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
    if(is.recursive(object)) {
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
             center = FALSE, xlab = "", ylab = "", ...)
{
    type <- match.arg(type)
    plot(0, xlim = c(0, .memberDend(x)+1),
         ylim = c(0, attr(x, "height")), type = "n",
         xlab = xlab, ylab = ylab, ...)

    if(center)
        plotNode(0.5, .memberDend(x)+ 0.5, x, type, center)
    else
        plotNode(1,   .memberDend(x),      x, type, center)
}

### the work horse: plots lines from a node to all children
plotNode <- function(x1, x2, subtree, type, center) {

    if(is.recursive(subtree) && x1 != x2) {
        K <- length(subtree)
        topy <- attr(subtree, "height")

        bx <- plotNodeLimit(x1, x2, subtree, center)
        topx <- bx$x

        for(k in 1:K) {
            boty <- attr(subtree[[k]], "height")
            if(is.null(boty)) boty <- 0

            botx <-
                if(center) mean(bx$limit[k:(k+1)])
                else {
                    mid <- attr(subtree[[k]],"midpoint")
                    bx$limit[k] + if(is.null(mid)) 0 else mid
                }

            if(type == "triangle")
                lines(c(topx, botx), c(topy, boty))
            else {
                lines(c(topx, botx), c(topy, topy))
                lines(c(botx, botx), c(topy, boty))
            }

            plotNode(bx$limit[k], bx$limit[k+1],
                     subtree[[k]], type, center)
        }
    }
}


plotNodeLimit <- function(x1, x2, subtree, center)
{
    ## get the left borders limit[k] of all children k=1..K, and
    ## the handle point `x' for the edge connecting to the parent.

    limit <- c(x1, x2)
    x <- mean(c(x1,x2))
    if(is.recursive(subtree) && x1 != x2) {
        K <- length(subtree)
        topm <- .memberDend(subtree)
        limit <- integer(K)
        xx1 <- x1
        for(k in 1:K) {
            m <- .memberDend(subtree[[k]])
            ##if(is.null(m)) m <- 1
            limit[k] <- xx1 +
                (if(center) (x2-x1) * m/topm else m)
            xx1 <- limit[k]
        }
        limit <- c(x1, limit)
        if(!center) {
            x <- x1 + attr(subtree, "midpoint")
        }
    }
    list(x = x, limit = limit)
}

cut.dendrogram <- function(x, h, ...)
{
    LOWER <- list()
    X <- 1

    assignNodes <- function(subtree, h) {
        if(is.recursive(subtree)) {
            new.mem <- 0
            for(k in seq(along = subtree)) {
                if(attr(subtree[[k]], "height") <= h) {
                    ## cut it, i.e. save to LOWER[] and make a leaf
                    sub <- subtree[[k]]
                    at <- attributes(sub)
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
