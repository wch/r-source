rowsum<-function(x,group,reorder=TRUE,...)
    UseMethod("rowsum")

rowsum.default <-function(x,group,reorder=TRUE,...){
    if (!is.numeric(x))
        stop("x must be numeric")
    if (length(group) != NROW(x))
        stop("Incorrect length for 'group'")
    if (any(is.na(group)))
        warning("missing values for 'group'")
    ugroup<-unique(group)
    if (reorder) ugroup<-sort(ugroup,na.last=TRUE)

    rval<-.Call("Rrowsum_matrix",x,NCOL(x),group,ugroup,PACKAGE="base")

    dimnames(rval)<-list(as.character(ugroup),dimnames(x)[[2]])
    rval
}

rowsum.data.frame<-function(x,group,reorder=TRUE,...){
    if (!is.data.frame(x)) stop("not a data frame") ## make MM happy
    if (length(group) != NROW(x))
        stop("Incorrect length for 'group'")
    if (any(is.na(group)))
        warning("missing values for 'group'")
    ugroup<-unique(group)
    if (reorder) ugroup<-sort(ugroup,na.last=TRUE)

    rval<-.Call("Rrowsum_df", x, NCOL(x), group, ugroup, PACKAGE="base")

    as.data.frame(rval, row.names=as.character(ugroup))
}
