browseEnv <- function(envir = .GlobalEnv, pattern,
                      excludepatt = "^last\\.warning",
		      html = .Platform$OS.type != "mac",
		      expanded = TRUE, properties = NULL,
		      main = NULL, debugMe = FALSE)
{
    objlist <- ls(envir = envir, pattern = pattern)#, all.names = FALSE
    if(length(iX <- grep(excludepatt, objlist)))
        objlist <- objlist[ - iX]
    if(debugMe) { cat("envir= "); print(envir)
		  cat("objlist =\n"); print(objlist) }
    n <- length(objlist)
    if(n == 0) {
	cat("Empty environment, nothing to do!\n")
	return(invisible())
    }

    str1 <- function(obj) {
	md <- mode(obj)
	lg <- length(obj)
	objdim <- dim(obj)
	if(length(objdim) == 0)
	    dim.field <- paste("length:", lg)
	else{
	    dim.field <- "dim:"
	    for(i in 1:length(objdim))
		dim.field <- paste(dim.field,objdim[i])
	    if(is.matrix(obj))
		md <- "matrix"
	}
	obj.class <- oldClass(obj)
	if(!is.null(obj.class)) {
	    md <- obj.class[1]
	    if(inherits(obj, "factor"))
		dim.field <- paste("levels:",length(levels(obj)))
	}
	list(type = md, dim.field = dim.field)
    }

    N <- 0
    M <- n
    IDS <- rep.int(NA,n)
    NAMES <- rep.int(NA,n)
    TYPES <- rep.int(NA,n)
    DIMS <- rep.int(NA,n)

    IsRoot <- rep.int(TRUE,n)
    Container <- rep.int(FALSE,n)
    ItemsPerContainer <- rep.int(0,n)
    ParentID <- rep.int(-1,n)

    for( objNam in objlist ){
	N <- N+1
	if(debugMe) cat("  ", N,":", objNam)
	obj    <- get(objNam, envir = envir)

	sOb <- str1(obj)

	if(debugMe) cat(", type=", sOb$type,",", sOb$dim.field,"\n")

	## Fixme : put these 4 in a matrix or data.frame row:
	IDS[N] <- N
	NAMES[N] <- objNam
	TYPES[N] <- sOb$type
	DIMS[N] <- sOb$dim.field

	if(is.recursive(obj) && !is.function(obj) && !is.environment(obj)
	    ## includes "list", "expression", also "data.frame", ..
	   && (lg <- length(obj)) > 0) {
	    Container[N] <- TRUE
	    ItemsPerContainer[N] <- lg
	    nm <- names(obj)
	    if(is.null(nm)) nm <- paste("[[",format(1:lg),"]]", sep="")
	    for(i in 1:lg) {
		M <- M+1
		ParentID[M] <- N
		if(nm[i] == "") nm[i] <- paste("[[",i,"]]", sep="")

		s.l <- str1(obj[[i]])
		##cat("	   objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
		IDS   <- c(IDS,M)
		NAMES <- c(NAMES, nm[i])
		TYPES <- c(TYPES, s.l$type)
		DIMS  <- c(DIMS,  s.l$dim.field)
	    }
	}## recursive

	else if(!is.null(class(obj))) {
	    ## treat some special __non-recursive__ classes:
	    if(inherits(obj, "table")) {
		obj.nms <- attr(obj,"dimnames")
		lg <- length(obj.nms)
		if(length(names(obj.nms)) >0)
		    nm <- names(obj.nms)
		else
		    nm <- rep.int("",lg)
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

	    else if(inherits(obj, "mts")) {

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

	} ## recursive or classed

    } ## "for each object"

    if(debugMe) cat(" __end {for}\n ")##; browser()

    Container	      <- c(Container,	  rep.int(FALSE, M-N))
    IsRoot	      <- c(IsRoot,	  rep.int(FALSE, M-N))
    ItemsPerContainer <- c(ItemsPerContainer, rep.int(0, M-N))

    if(is.null(main))
	main <- paste("R objects in", deparse(substitute(envir)))
    if(is.null(properties)) {
	properties <- as.list(c(date = format(Sys.time(), "%Y-%b-%d %H:%M"),
				local({
				    si <- Sys.info()
				    si[c("user","nodename","sysname")]})))
    }
    if(html)
	wsbrowser(IDS,IsRoot,Container,ItemsPerContainer, ParentID,
		  NAMES,TYPES,DIMS,
		  kind = "HTML", main = main, properties = properties,
		  expanded)
    else ## currently only for Mac:
	.Internal(wsbrowser(as.integer(IDS),IsRoot,Container,
			    as.integer(ItemsPerContainer),as.integer(ParentID),
			    NAMES,TYPES,DIMS))
}

wsbrowser <- function(IDS, IsRoot, IsContainer, ItemsPerContainer,
		      ParentID, NAMES, TYPES, DIMS, expanded=TRUE,
		      kind = "HTML",
		      main = "R Workspace", properties = list(),
		      browser = getOption("browser"))
{
    if(kind != "HTML")
        stop(gettextf("kind '%s' not yet implemented", kind), domain = NA)

    Pst <- function(...) paste(..., sep="")

    bold <- function(ch) Pst("<b>",ch,"</b>")
    ital <- function(ch) Pst("<i>",ch,"</i>")
    entry<- function(ch) Pst("<td>",ch,"</td>")
    Par	 <- function(ch) Pst("<P>",ch,"</P>")
    Trow <- function(N, ...) {
	if(length(list(...)) != N) stop("wrong number of table row entries")
	paste("<tr>", ..., "</tr>\n")
    }
    catRow <- function(...) cat(Trow(nCol, ...), file = Hfile)

#    n <- length(IDS)
    RootItems <- which(IsRoot)
    NumOfRoots <- length(RootItems)

    props <- properties
    if(length(props)) { ## translate named list into 2-column (vertical) table
	nms <- names(props)
	nms <- unlist(lapply(unlist(lapply(Pst(nms,":"),
					   bold)),
			     entry))
	props <- unlist(lapply(props, entry))
	props <-
	    paste("<table border=2>",
		  paste(Trow(1, paste(nms, props)), collapse=""),
		  "</table>", sep = "\n")
    }
    fname <- file.path(tempdir(), "wsbrowser.html")
    Hfile <- file(fname,"w")

    cat("<html>\n<title>", main, "browser</title>\n<body>",
	"<H1>",main,"</H1>\n",
	if(is.character(props)) Par(props),
	"<table border=1>\n", file = Hfile)
    nCol <- if(expanded) 4 else 3
    catRow(entry(bold("Object")),
	   if(expanded) entry(bold(ital("(components)"))),
	   entry(bold("Type")),
	   entry(bold("Property")))

    for(i in 1:NumOfRoots) {
	iid <- RootItems[i]
	catRow(entry(NAMES[iid]),
	       if(expanded) entry(""),
	       entry(ital(TYPES[iid])),
	       entry(DIMS[iid]))
	if(IsContainer[i] && expanded) {
	    items <- which(ParentID == i)
	    for(j in 1:ItemsPerContainer[i]) {
		id <- IDS[items[j]]
		catRow(entry(""),
		       entry(NAMES[id]),#was Pst("$",NAMES[id]) : ugly for [[i]]
		       entry(ital(TYPES[id])),
		       entry(DIMS[id]))
	    }
	}
    }
    cat("</table>\n</body></html>",file=Hfile)
    close(Hfile)

    switch(.Platform$OS.type,
	   windows = , ## do we need anything here?
	   unix = { url <- fname },
	   )
    if(substr(url, 1,1) != "/")
	url <- paste("/", url, sep = "")
    url <- paste("file://", URLencode(url), sep = "")

    browseURL(url = url, browser = browser)
    cat(main, "environment is shown in browser",
        if(!is.null(browser))paste("`",browser, "'", sep=""),"\n")

    invisible(fname)
}
