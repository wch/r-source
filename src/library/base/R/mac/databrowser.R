browse.wspace <- function(html=FALSE, expanded=TRUE)
{
    objlist <- ls(sys.frame())
    n <- length(objlist)
    if(n==0)
	stop("Empty workspace, nothing to do!")

    N <- 0
    M <- n
    IDS <- rep(NA,n)
    NAMES <- rep(NA,n)
    TYPES <- rep(NA,n)
    DIMS <- rep(NA,n)

    IsRoot <- rep(TRUE,n)
    Container <- rep(FALSE,n)
    ItemsPerContainer <- rep(0,n)
    ParentID <- rep(-1,n)

    for( object in objlist ){
	N <- N+1
	obj    <- get(object)
	md     <- mode(obj)
	objdim <- dim(obj)
	if(length(objdim) == 0)
	    dim.field <- paste("length:",length(obj))
	else{
	    dim.field <- "dim:"
	    for(i in 1:length(objdim))
		dim.field <- paste(dim.field,objdim[i])
	    if(is.matrix(obj))
		md <- "matrix"
	}
	obj.class <- attr(obj,"class")
	if(length(obj.class)!=0){
	    md <- obj.class[1]
	    if(obj.class[1] == "factor")
		dim.field <- paste("levels:",length(levels(obj)))
	}
	##cat("objname:",object,", type=",md,",",dim.field,"\n")

	IDS[N] <- N
	NAMES[N] <- object
	TYPES[N] <- md
	DIMS[N] <- dim.field

	if(md == "list"){
	    lg <- length(obj)
	    Container[N] <- TRUE
	    ItemsPerContainer[N] <- lg
	    nm <- names(obj)
	    for(i in 1:lg){
		M <- M+1
		ParentID[M] <- N
		if(nm[i] == "") nm[i] = paste("[[",i,"]]",sep="")
		md.l  <- mode(obj[[i]])
		objdim.l <- dim(obj[[i]])
		if(length(objdim.l) == 0)
		    dim.field.l <- paste("length:",length(obj[[i]]))
		else{
		    dim.field.l <- "dim:"
		    for(j in 1:length(objdim.l))
			dim.field.l <- paste(dim.field.l,objdim.l[i])
		}

		obj.at <- attr(obj[[i]],"class")
		if(length(obj.at)!=0)
		    if(obj.at == "factor"){
			md.l <- obj.at
			dim.field.l <- paste("levels:",length(levels(obj[[i]])))
		    }

		##cat("	   objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		IDS <- c(IDS,M)
		NAMES <- c(NAMES, nm[i])
		TYPES <- c(TYPES, md.l)
		DIMS <- c(DIMS,dim.field.l)
	    }
	}## "list"

	obj.class <- attr(obj,"class")
	if(length(obj.class)!=0){
	    if( obj.class == "table"){
		obj.nms <- attr(obj,"dimnames")
		lg <- length(obj.nms)
		if(length(names(obj.nms)) >0)
		    nm <- names(obj.nms)
		else
		    nm <- rep("",lg)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in 1:lg){
		    M <- M+1
		    ParentID[M] <- N
		    if(nm[i] == "") nm[i] = paste("[[",i,"]]",sep="")
		    md.l  <- mode(obj.nms[[i]])
		    objdim.l <- dim(obj.nms[[i]])
		    if(length(objdim.l) == 0)
			dim.field.l <- paste("length:",length(obj.nms[[i]]))
		    else{
			dim.field.l <- "dim:"
			for(j in 1:length(objdim.l))
			    dim.field.l <- paste(dim.field.l,objdim.l[i])
		    }
		    ##cat("    objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		    IDS <- c(IDS,M)
		    NAMES <- c(NAMES, nm[i])
		    TYPES <- c(TYPES, md.l)
		    DIMS <- c(DIMS,dim.field.l)
		}
	    }## "table"

	    if( obj.class == "data.frame"){
		nm <- dimnames(obj)[[2]]
		lg <- length(nm)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in 1:lg){
		    M <- M +1
		    ParentID[M] <- N
		    md.l  <- mode(obj[[i]])
		    dim.field.l <- paste("length:",length(obj[[i]]))

		    obj.at <- attr(obj[[i]],"class")
		    if(length(obj.at)!=0)
			if(obj.at == "factor"){
			    md.l <- obj.at
			    dim.field.l <- paste("levels:",length(levels(obj[[i]])))
			}
		    ##cat("    varname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		    IDS <- c(IDS,M)
		    NAMES <- c(NAMES, nm[i])
		    TYPES <- c(TYPES, md.l)
		    DIMS <- c(DIMS,dim.field.l)
		}
	    }## "data.frame"

	    if( obj.class == "mts"){
		nm <- dimnames(obj)[[2]]
		lg <- length(nm)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in 1:lg){
		    M <- M+1
		    ParentID[M] <- N
		    md.l  <- mode(obj[[i]])
		    dim.field.l <- paste("length:",dim(obj)[1])
		    md.l <- "ts"
		    ##cat("    tseries:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		    IDS <- c(IDS,M)
		    NAMES <- c(NAMES, nm[i])
		    TYPES <- c(TYPES, md.l)
		    DIMS <- c(DIMS,dim.field.l)
		}
	    }## "mts"

	    if( obj.class[1] == "lm" | obj.class[1] == "glm"){
		nm <- names(obj)
		lg <- length(nm)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in 1:lg){
		    M <- M+1
		    ParentID[M] <- N
		    md.l  <- mode(obj[[i]])
		    dim.field.l <- paste("length:",length(obj[[i]]))
		    ##	   md.l <- "ts"

		    ##cat("    name:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		    IDS <- c(IDS,M)
		    NAMES <- c(NAMES, nm[i])
		    TYPES <- c(TYPES, md.l)
		    DIMS <- c(DIMS,dim.field.l)
		}
	    } ## "lm"

	} ## "length"

    } ## "for"

    Container	      <- c(Container,	  rep(FALSE, M-N))
    IsRoot	      <- c(IsRoot,	  rep(FALSE, M-N))
    ItemsPerContainer <- c(ItemsPerContainer, rep(0, M-N))
    if(html)
	html.wsbrowser(IDS,IsRoot,Container,ItemsPerContainer, ParentID,
		       NAMES,TYPES,DIMS,expanded)
    else
	.Internal(wsbrowser(as.integer(IDS),IsRoot,Container,
			    as.integer(ItemsPerContainer),as.integer(ParentID),
			    NAMES,TYPES,DIMS))
}


html.wsbrowser <- function(IDS, IsRoot, IsContainer, ItemsPerContainer,
			   ParentID, NAMES, TYPES, DIMS, expanded=TRUE)
{
    n <- length(IDS)
    RootItems <- which(IsRoot)
    NumOfRoots <- length(RootItems)

    tmp.dir <- tempdir()
    tmp.html <- file.path(tmp.dir,"wsbrowser.html")
    zz <- file(tmp.html,"w")
    cat("<html>\n<title>R Workspace browser</title>\n<body>",
        "<H1>R Workspace as of",date(),"</h1>\n",
        "<table border=1>\n",
        "<tr><td><b>Object</b></td>"
        if(expanded) "<td></td>",
        "<td><b>Type</b></td>",
        "<td><b>Property</b></td></tr>\n",
        file = zz)

    for(i in 1:NumOfRoots) {

        cat("<tr><td>", NAMES[RootItems[i]], if(expanded) "</td><td>",
            "</td><td><i>",TYPES[RootItems[i]],"</i></td><td>",
            DIMS[RootItems[i]],"</td></tr>\n",file=zz)
	if(IsContainer[i] && expanded) {
	    items <- which(ParentID == i)
	    for(j in 1:ItemsPerContainer[i]) {
		cat("<tr><td></td><td>$",NAMES[IDS[items[j]]],"</td>",
                    "<td><i>",TYPES[IDS[items[j]]],"</i></td>",
                    "<td>",DIMS[IDS[items[j]]],"</td></tr>\n",file=zz)
	    }
	}
    }
    cat("</table>\n</body></html>",file=zz)
    close(zz)

    browser <- getOption("browser")
    if( is.null(browser) )
	fname <- gsub(":", "/", .Internal(truepath(tmp.html)))
    else {
	if (browser == "netscape")
	    fname <- gsub(":", "/", tmp.html)
	else
	    fname <- gsub(":", "/", .Internal(truepath(tmp.html)))
    }
    ch <- strsplit(fname, "")[[1]][1]
    if (ch != "/")
	fname <- paste("/", fname, sep = "")
    fname <- paste("file://", fname, sep = "")
    browseURL(url = fname, browser = browser)
    cat("R Workspace is shown in browser\n")
    return(invisible())
}

