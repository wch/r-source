cutree <- function(tree, k=NULL, h=NULL)
{
    if(is.null(k) & is.null(h))
        stop("Either k or h must be specified")
    else if(is.null(k)){
        k <- integer(length(h))
        myh <- h
        myh[h<min(tree$height)] <- min(tree$height)
        myh[h>max(tree$height)] <- max(tree$height)
        for(n in 1:length(h))
            k[n] <- min(which(rev(tree$height) <= myh[n]))
    }
    else{
        k <- as.integer(k)
        if(min(k) < 2 | max(k) > nrow(tree$merge))
            stop(paste("Elements of k must be between 2 and",
                       nrow(tree$merge)))
    }
    
    ans <- .Call("R_cutree", tree$merge, k)
    
    if(length(k)==1){
        ans <- as.vector(ans)
        names(ans) <- tree$labels
    }
    else{
        if(! is.null(h))
            colnames(ans) <- h
        else
            colnames(ans) <- k
        rownames(ans) <- tree$labels
    }
    
    return(ans)
}
        


        
    

             
