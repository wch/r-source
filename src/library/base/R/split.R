split <- function( x, f ) UseMethod( "split" )

split.default <- function( x, f )
{
    f <- factor(f) # drop extraneous levels
    if(is.null(class(x)))
        return(.Internal(split(x, f)))
    ## else
    lf <- levels(f)
    retval <- list()
    for(k in lf){
        retval[[k]] <- x[f==k]
    }
    retval
}


split.data.frame <- function( x, f )
{
    lapply( split( 1:nrow(x), f ), function(ind) x[ ind, , drop = FALSE ] )
}
