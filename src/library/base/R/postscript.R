.PostScript.Options <- list(paper="default",
			    horizontal = TRUE,
			    width = 0,
			    height = 0,
			    family = "Helvetica",
			    pointsize = 12,
			    bg = "white",
			    fg = "black",
			    onefile = TRUE,
			    print.it = FALSE,
			    append = FALSE)

check.options <-
    function(new, name.opt, reset = FALSE, assign.opt = FALSE,
	     envir=.GlobalEnv, check.attributes = c("mode", "length"),
	     override.check= FALSE)
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
	    if(length(find(name.opt)) > 1)
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
	    stop(paste("invalid argument names in \"",
		       deparse(sys.call(sys.parent())),"\"",sep=""))
	else if(any(matches==0))
	    stop(paste("ambiguous argument names in \"",
		       deparse(sys.call(sys.parent())),"\"",sep=""))
	else { #- match(es) found:  substitute if appropriate
	    i.match <- oldnames[matches]
	    prev <- old[i.match]
	    doubt <- rep(FALSE, length(prev))
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
    old <- check.options(new = new, name.opt = ".PostScript.Options",
			 reset = as.logical(reset), assign.opt = l... > 0,
			 override.check= override.check)
    if(reset || l... > 0) invisible(old)
    else old
}

postscript <- function (file = "Rplots.ps", ...)
{
    new <- list(...)# eval
    old <- check.options(new = new, name.opt = ".PostScript.Options",
			 reset = FALSE, assign.opt = FALSE)
    .Internal(PS(file, old$paper, old$family, old$bg, old$fg,
		 old$width, old$height, old$horizontal, old$pointsize))
}
##--> source in ../../../main/devices.c	 and ../../../unix/devPS.c
