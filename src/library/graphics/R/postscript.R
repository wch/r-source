## An environment not exported from namespace:graphics used to
## pass .PostScript.Options to the windows() device for use in its menus.
## and also to hide the variable.
.PSenv <- new.env()

assign(".PostScript.Options",
    list(paper	= "default",
	 horizontal = TRUE,
	 width	= 0,
	 height = 0,
	 family = "Helvetica",
	 encoding = "default",
	 pointsize  = 12,
	 bg	= "transparent",
	 fg	= "black",
	 onefile    = TRUE,
	 print.it   = FALSE,
	 append	    = FALSE,
	 pagecentre = TRUE,
	 command    = "default"), envir = .PSenv)

check.options <-
    function(new, name.opt, reset = FALSE, assign.opt = FALSE,
	     envir = .GlobalEnv, check.attributes = c("mode", "length"),
	     override.check = FALSE)
{
    lnew <- length(new)
    if(lnew != length(newnames <- names(new)))
	stop(paste("invalid arguments in \"",
		   deparse(sys.call(sys.parent())),
		   "\" (need NAMED args)", sep=""))
    if(!is.character(name.opt))
	stop("'name.opt' must be character, name of an existing list")
    if(reset) {
	if(exists(name.opt, envir=envir, inherits=FALSE)) {
	    if(length(utils::find(name.opt)) > 1)
		rm(list=name.opt, envir=envir)
##-	    else
##-		stop(paste("Cannot reset '", name.opt,
##-			"'  since it exists only once in search()!\n", sep=""))

	} else stop(paste("Cannot reset non-existing '", name.opt, "'", sep=""))
    }
    old <- get(name.opt, envir=envir)
    if(!is.list(old))
	stop(paste("invalid options in `",name.opt,"'",sep=""))
    oldnames <- names(old)
    if(lnew > 0) {
	matches <- pmatch(newnames, oldnames)
	if(any(is.na(matches)))
	    stop(paste("invalid argument name(s) `",
		       paste(newnames[is.na(matches)], collapse=", "),
		       "' in \"", deparse(sys.call(sys.parent())),"\"",sep=""))
##-- This does not happen: ambiguities are plain "NA" here:
##-	else if(any(matches==0))
##-	    stop(paste("ambiguous argument name(s) `",
##-			   paste(newnames[matches == 0], collapse=", "),
##-			   "' in \"", deparse(sys.call(sys.parent())),"\"",sep=""))
	else { #- match(es) found:  substitute if appropriate
	    i.match <- oldnames[matches]
	    prev <- old[i.match]
	    doubt <- rep.int(FALSE, length(prev))
	    for(fn in check.attributes)
		if(any(ii <- sapply(prev, fn) != sapply(new, fn))) {
		    doubt <- doubt | ii
		    do.keep <- ii & !override.check
		    warning(paste(paste(paste("`",fn,"(",names(prev[ii]),")'",
					      sep=""),
					collapse=" and "),
				  " differ", if(sum(ii)==1) "s",
				  " between new and previous!",
				  if(any(do.keep))
				  paste("\n\t ==> NOT changing ",
					paste(paste("`",names(prev[do.keep]),
						    "'", sep=""),
					      collapse=" & "),
					collapse = ""),
				  sep=""))
		}
	    names(new) <- NULL
	    if(any(doubt)) {
		ii <- !doubt | override.check
		old[i.match[ii]] <- new[ii]
	    } else old[i.match] <- new

	}
	if(assign.opt) assign(name.opt, old, envir=envir)
    }
    old
}

ps.options <- function(..., reset=FALSE, override.check= FALSE)
{
    l... <- length(new <- list(...))
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = as.logical(reset), assign.opt = l... > 0,
			 override.check= override.check)
    if(reset || l... > 0) invisible(old)
    else old
}

##--> source in ../../../main/devices.c	 and ../../../main/devPS.c :

postscript <- function (file = ifelse(onefile,"Rplots.ps", "Rplot%03d.ps"),
                        onefile = TRUE, family,
                        title = "R Graphics Output", ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)

    if(is.null(old$command) || old$command == "default")
        old$command <- if(!is.null(cmd <- getOption("printcmd"))) cmd else ""
    ## handle family separately as length can be 1, 4, or 5
    if(!missing(family)) {
        if(length(family) == 4) family <- c(family, "sy______.afm")
        old$family <- family
    }
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- switch(.Platform$OS.type,
                               "windows" = "WinAnsi.enc",
                               "ISOLatin1.enc")
    .Internal(PS(file, old$paper, old$family, old$encoding, old$bg, old$fg,
		 old$width, old$height, old$horizontal, old$pointsize,
                 old$onefile, old$pagecentre, old$print.it, old$command,
                 title))
    # if .ps.prolog is searched for and fails, NULL got returned.
    invisible()
}

xfig <- function (file = ifelse(onefile,"Rplots.fig", "Rplot%03d.fig"),
                  onefile = FALSE, ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)

    .Internal(XFig(file, old$paper, old$family, old$bg, old$fg,
		 old$width, old$height, old$horizontal, old$pointsize,
                 old$onefile, old$pagecentre))
}

pdf <- function (file = ifelse(onefile, "Rplots.pdf", "Rplot%03d.pdf"),
                 width = 6, height = 6, onefile = TRUE,
                 title = "R Graphics Output", ...)
{
    new <- list(onefile=onefile, ...)# eval
    old <- check.options(new = new, envir = .PSenv,
                         name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)
    if(is.null(old$encoding) || old$encoding  == "default")
        old$encoding <- switch(.Platform$OS.type,
                               "windows" = "WinAnsi.enc",
                               "ISOLatin1.enc")
    .Internal(PDF(file, old$family, old$encoding, old$bg, old$fg,
                  width, height, old$pointsize, old$onefile, title))
}

.ps.prolog <- c(
"/gs  { gsave } def",
"/gr  { grestore } def",
"/ep  { showpage gr gr } def",
"/m   { moveto } def",
"/l  { rlineto } def",
"/np  { newpath } def",
"/cp  { closepath } def",
"/f   { fill } def",
"/o   { stroke } def",
"/c   { newpath 0 360 arc } def",
"/r   { 4 2 roll moveto 1 copy 3 -1 roll exch 0 exch rlineto 0 rlineto -1 mul 0 exch rlineto closepath } def",
"/p1  { stroke } def",
"/p2  { gsave bg setrgbcolor fill grestore newpath } def",
"/p3  { gsave bg setrgbcolor fill grestore stroke } def",
"/t   { 6 -2 roll moveto gsave rotate",
"       ps mul neg 0 2 1 roll rmoveto",
"       1 index stringwidth pop",
"       mul neg 0 rmoveto show grestore } def",
"/cl  { grestore gsave newpath 3 index 3 index moveto 1 index",
"       4 -1 roll lineto  exch 1 index lineto lineto",
"       closepath clip newpath } def",
"/rgb { setrgbcolor } def",
"/s   { scalefont setfont } def",
"/R   { /Font1 findfont } def",
"/B   { /Font2 findfont } def",
"/I   { /Font3 findfont } def",
"/BI  { /Font4 findfont } def",
"/S   { /Font5 findfont } def",
"1 setlinecap 1 setlinejoin")
