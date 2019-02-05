#  File src/library/utils/R/databrowser.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

browseEnv <- function(envir = .GlobalEnv, pattern,
                      excludepatt = "^last\\.warning",
		      html = .Platform$GUI != "AQUA",
		      expanded = TRUE, properties = NULL,
		      main = NULL, debugMe = FALSE)
{
    objlist <- ls(envir = envir, pattern = pattern)#, all.names = FALSE
    if(length(iX <- grep(excludepatt, objlist)))
        objlist <- objlist[ - iX]
    if(debugMe) { cat("envir= "); print(envir)
		  cat("objlist =\n"); print(objlist) }
    n <- length(objlist)
    if(n == 0L) {
	cat("Empty environment, nothing to do!\n")
	return(invisible())
    }

    str1 <- function(obj) {
	md <- mode(obj)
	lg <- length(obj)
	objdim <- dim(obj)
	if(length(objdim) == 0L)
	    dim.field <- paste("length:", lg)
	else{
	    dim.field <- "dim:"
	    for(i in seq_along(objdim))
		dim.field <- paste(dim.field,objdim[i])
	    if(is.matrix(obj))
		md <- "matrix"
	}
	obj.class <- oldClass(obj)
	if(!is.null(obj.class)) {
	    md <- obj.class[1L]
	    if(inherits(obj, "factor"))
		dim.field <- paste("levels:",length(levels(obj)))
	}
	list(type = md, dim.field = dim.field)
    }

    N <- 0L
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
	N <- N+1L
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
	   && (lg <- length(obj))) {
	    Container[N] <- TRUE
	    ItemsPerContainer[N] <- lg
	    nm <- names(obj)
	    if(is.null(nm)) nm <- paste0("[[", format(1L:lg), "]]")
	    for(i in 1L:lg) {
		M <- M+1
		ParentID[M] <- N
		if(nm[i] == "") nm[i] <- paste0("[[", i, "]]")

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
		    nm <- rep.int("", lg)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in seq_len(lg)){
		    M <- M+1L
		    ParentID[M] <- N
		    if(nm[i] == "") nm[i] <- paste0("[[",i,"]]")
		    md.l  <- mode(obj.nms[[i]])
		    objdim.l <- dim(obj.nms[[i]])
		    if(length(objdim.l) == 0L)
			dim.field.l <- paste("length:", length(obj.nms[[i]]))
		    else{
			dim.field.l <- "dim:"
			for(j in seq_along(objdim.l))
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

		nm <- dimnames(obj)[[2L]]
		lg <- length(nm)
		Container[N] <- TRUE
		ItemsPerContainer[N] <- lg
		for(i in seq_len(lg)){
		    M <- M+1L
		    ParentID[M] <- N
		    md.l  <- mode(obj[[i]])
		    dim.field.l <- paste("length:",dim(obj)[1L])
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
	wsbrowser(IDS, IsRoot, Container, ItemsPerContainer, ParentID,
		  NAMES, TYPES, DIMS, kind = "HTML", main = main,
                  properties = properties, expanded)
    else if(.Platform$GUI == "AQUA") {
        awsbrowser <- get("wsbrowser", envir = as.environment("tools:RGUI"))
 	awsbrowser(as.integer(IDS), IsRoot, Container,
                   as.integer(ItemsPerContainer), as.integer(ParentID),
                   NAMES, TYPES, DIMS)
   } else stop("only 'html = TRUE' is supported on this platform")
}

wsbrowser <- function(IDS, IsRoot, IsContainer, ItemsPerContainer,
		      ParentID, NAMES, TYPES, DIMS, expanded=TRUE,
		      kind = "HTML",
		      main = "R Workspace", properties = list(),
		      browser = getOption("browser"))
{
    if(kind != "HTML")
        stop(gettextf("kind '%s' not yet implemented", kind), domain = NA)

    bold <- function(ch) paste0("<b>",ch,"</b>")
    ital <- function(ch) paste0("<i>",ch,"</i>")
    entry <- function(ch) paste0("<td>",ch,"</td>")
    Par	<- function(ch) paste0("<P>",ch,"</P>")
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
	nms <- unlist(lapply(unlist(lapply(paste0(nms,":"),
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
    nCol <- if(expanded) 4L else 3L
    catRow(entry(bold("Object")),
	   if(expanded) entry(bold(ital("(components)"))),
	   entry(bold("Type")),
	   entry(bold("Property")))

    for(i in seq_len(NumOfRoots)) {
	iid <- RootItems[i]
	catRow(entry(NAMES[iid]),
	       if(expanded) entry(""),
	       entry(ital(TYPES[iid])),
	       entry(DIMS[iid]))
	if(IsContainer[i] && expanded) {
	    items <- which(ParentID == i)
	    for(j in seq_len(ItemsPerContainer[i])) {
		id <- IDS[items[j]]
		catRow(entry(""),
		       entry(NAMES[id]),#was paste0("$",NAMES[id]) : ugly for [[i]]
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
    if(!startsWith(url, "/"))
	url <- paste0("/", url)
    url <- paste0("file://", URLencode(url))

    browseURL(url = url, browser = browser)
    cat(main, "environment is shown in browser",
	if(is.character(browser)) sQuote(browser),"\n")

    invisible(fname)
}
