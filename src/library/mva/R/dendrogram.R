as.dendrogram <- function(object, ...) UseMethod("as.dendrogram")

as.dendrogram.hclust <- function(object, ...)
{
    if(is.null(object$labels))
        object$labels <- object$order

    z <- list()
    for(k in 1:nrow(object$merge)){
        x <- sort(object$merge[k,])
        k <- as.character(k)
        if(all(x<0)){  # two leaves
            z[[k]] <- as.list(object$labels[-x])
            attr(z[[k]],"members") <- 2
            attr(z[[k]],"midpoint") <- 0.5
            attr(z[[k]][[1]], "members") <-
              attr(z[[k]][[2]], "members") <- 1
            attr(z[[k]][[1]], "height") <-
              attr(z[[k]][[2]], "height") <- 0

        }
        else if(x[1]<0){ # one leave, one node
            z[[k]] <- list(object$labels[-x[1]],
                           z[[as.character(x[2])]])
            x <- as.character(x)
            attr(z[[k]],"members") <-
                attr(z[[x[2]]],"members")+1
            attr(z[[k]], "midpoint") <-
                (1+attr(z[[x[2]]],"midpoint"))/2
            attr(z[[k]][[1]], "members") <- 1
            attr(z[[k]][[1]], "height") <- 0
        }
        else{ # two nodes
            x <- as.character(x)
            z[[k]] <- list(z[[x[1]]],
                           z[[x[2]]])
            attr(z[[k]],"members") <-
                attr(z[[x[1]]], "members")+
                    attr(z[[x[2]]], "members")
            attr(z[[k]],"midpoint") <-
                (attr(z[[x[1]]], "members")+
                 attr(z[[x[1]]], "midpoint")+
                 attr(z[[x[2]]], "midpoint"))/2
        }
        attr(z[[k]],"height") <- object$height[as.integer(k)]
    }
    z <- z[[k]]
    class(z) <- "dendrogram"
    z
}

plot.dendrogram <-
    function(x, type=c("rectangle", "triangle"),
             center=FALSE, xlab="", ylab="", ...) {

    type <- match.arg(type)
    plot(0, xlim=c(0, attr(x, "members")+1),
         ylim=c(0, attr(x, "height")), type="n",
         xlab=xlab, ylab=ylab, ...)

    if(center)
        plotNode(0.5, attr(x, "members")+.5,
                 x, type, center)
    else
        plotNode(1, attr(x, "members"),
                 x, type, center)
}

### the work horse: plots lines from a node to all
### children
plotNode <- function(x1, x2, subtree, type, center){

    if(is.recursive(subtree) & (x1!=x2)){
        K <- length(subtree)
        topy <- attr(subtree, "height")

        bx <- plotNodeLimit(x1, x2, subtree, center)
        topx <- bx$x

        for(k in 1:K){
            boty <- attr(subtree[[k]], "height")
            if(is.null(boty)) boty <- 0

            if(center){
                botx <- mean(c(bx$limit[k], bx$limit[k+1]))
            }
            else{
                mid <- attr(subtree[[k]],"midpoint")
                if(is.null(mid)) mid <- 0
                botx <- bx$limit[k] + mid

            }

            if(type=="triangle")
                lines(c(topx, botx), c(topy, boty))
            else{
                lines(c(topx, botx), c(topy, topy))
                lines(c(botx, botx), c(topy, boty))
            }

            plotNode(bx$limit[k], bx$limit[k+1],
                     subtree[[k]], type, center)
        }
    }
}


### get the left borders of all children and the
### handle point for the edge connecting to
### the parent.
plotNodeLimit <- function(x1, x2, subtree, center){

    limit <- c(x1, x2)
    x <- mean(c(x1,x2))
    if(is.recursive(subtree) & (x1!=x2)){
        K <- length(subtree)
        topm <- attr(subtree, "members")
        limit <- integer(K)
        xx1 <- x1
        for(k in 1:K){
            m <- attr(subtree[[k]], "members")
            if(is.null(m)) m <- 1
            if(center)
                limit[k] <- xx1 + (x2-x1) * m/topm
            else
                limit[k] <- xx1 + m
            xx1 <- limit[k]
        }
        limit <- c(x1, limit)
        if(!center){
            x <- x1 + attr(subtree, "midpoint")
        }
    }
    list(x=x, limit=limit)
}

cut.dendrogram <- function(x, h, ...)
{
    LOWER <- list()
    X <- 1

    assignNodes <- function(subtree, h){

        if(is.recursive(subtree)){
            for(k in 1:length(subtree)){
                if(attr(subtree[[k]], "height")<=h){
                    sub <- subtree[[k]]
                    class(sub) <- "dendrogram"
                    LOWER[[X]] <<- sub
                    at <- attributes(subtree[[k]])
                    subtree[[k]] <- paste("Branch", X)
                    attributes(subtree[[k]]) <- at
                    X <<- X+1
                }
                else
                    subtree[[k]] <- assignNodes(subtree[[k]], h)
            }
        }
        subtree
    }

    list(upper=assignNodes(x, h), lower=LOWER)
}
