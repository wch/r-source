rect.hclust <- function(hclust.obj, k=NULL, which=NULL,
                        x=NULL, h=NULL, border=2, cluster=NULL)
{
    if(length(h)>1 | length(k)>1)
        stop("k and h must be a scalar")
    
    if(!is.null(h)){
        if(!is.null(k))
            stop("specify exactly one of k and h")
        k <- min(which(rev(hclust.obj$height)<h))
        k <- max(k, 2)
    }
    else
        if(is.null(k)) 
            stop("specify exactly one of k and h")
    
    if(k < 2 | k > length(hclust.obj$height))
        stop(paste("k must be between 2 and", length(hclust.obj$height)))
    
    if(is.null(cluster))
        cluster <- cutree(hclust.obj, k=k)
    ## cutree returns classes sorted by data, we need classes
    ## as occurring in the tree (from left to right)
    clustab <- table(cluster)[unique(cluster[hclust.obj$order])]
    m <- c(0, cumsum(clustab))
    
    if(!is.null(x)){
        if(!is.null(which))
            stop("specify exactly one of which and x")        
        which <- x
        for(n in 1:length(x))
            which[n] <- max(which(m<x[n]))
    }
    else
        if(is.null(which))
            which <- 1:k

    if(any(which>k))
        stop(paste("all elements of which must be between 1 and", k))
    
    border <- rep(border, length=length(which))
    
    retval <- list()
    for(n in 1:length(which)){
        rect(m[which[n]]+0.66, par("usr")[3],
             m[which[n]+1]+0.33, mean(rev(hclust.obj$height)[(k-1):k]),
             border = border[n])
        retval[[n]] <- which(cluster==as.integer(names(clustab)[which[n]]))
    }
    invisible(retval)
}

identify.hclust <- function(HCOBJ, FUN=NULL, N=20, MAXCLUSTER=20,
                            DEV.FUN=NULL, ...)
{
  cluster <- cutree(HCOBJ, k=2:MAXCLUSTER)

  retval <- list()
  oldk <- NULL
  oldx <- NULL
  DEV.HCOBJ <- dev.cur()

  for(n in 1:N){

    dev.set(DEV.HCOBJ)
    x <- locator(1)
    if(is.null(x))
      break

    k <- min(which(rev(HCOBJ$height)<x$y), MAXCLUSTER)
    k <- max(k, 2)
    if(!is.null(oldx)){
      rect.hclust(HCOBJ, k=oldk, x=oldx, cluster=cluster[,oldk-1],
                  border="grey")
    }
    retval[[n]] <- unlist(rect.hclust(HCOBJ, k=k, x=x$x,
                                      cluster=cluster[,k-1],
                                      border="red"))
    if(!is.null(FUN)){
      if(!is.null(DEV.FUN)){
        dev.set(DEV.FUN)
      }
      retval[[n]] <- FUN(retval[[n]], ...)
    }
      
    oldx <- x$x
    oldk <- k
  }
  dev.set(DEV.HCOBJ)
  invisible(retval)
}
    


  
