####------ str : show STRucture of an R object
str <- function(object, ...) UseMethod("str")

str.data.frame <- function(object, ...)
{
    ## Method to 'str' for  'data.frame' objects
    ## $Id: str.R,v 1.14 2000/01/31 10:01:52 hornik Exp $
    if(! is.data.frame(object)) {
	warning("str.data.frame(.) called with non-data.frame. Coercing one.")
	object <- data.frame(object)
    }

    ## Show further classes // Assume that they do NOT have an own Method --
    ## not quite perfect ! (.Class = 'remaining classes', starting with current)
    cl <- class(object); cl <- cl[cl != "data.frame"]  #- not THIS class
    if(0 < length(cl)) cat("Classes", cl, " and ")

    cat("`data.frame':	", nrow(object), " obs. of  ",
	(p <- length(object)), " variable", if(p>1)"s",":\n",sep="")

    ## calling next method, usually  str.default:
    if(length(l <- list(...)) && any("give.length" == names(l)))
	invisible(NextMethod("str", ...))
    else invisible(NextMethod("str", give.length=FALSE,...))
}

str.default <- function(object, max.level = 0, vec.len = 4, digits.d = 3,
			give.attr = TRUE, give.length = TRUE,
			wid = getOption("width"),
			nest.lev = 0,
			indent.str = paste(rep(" ", max(0, nest.lev + 1)),
			collapse = "..")
			)
{
    ## Purpose: Display STRucture of any R - object (in a compact form).
    ## ------------------------------------------------------------------------
    ## Arguments: --- see HELP file --
    ##	max.level: Maximal level of nesting to be reported (0: as many as nec.)
    ##
    ## ------------------------------------------------------------------------
    ## Author: Martin Maechler <maechler@stat.math.ethz.ch>	1990--1997
    ## ------ Please send Bug-reports, -fixes and improvements !
    ## ------------------------------------------------------------------------
    ## $Id: str.R,v 1.14 2000/01/31 10:01:52 hornik Exp $

    oo <- options(digits = digits.d); on.exit(options(oo))
    le <- length(object)
    ## le.str: not used for arrays:
    le.str <-
	if(is.na(le)) " __no length(.)__ "
	else if(give.length) {
	    if(le > 0) paste("[1:", paste(le), "]", sep = "")
	    else "(0)"
	} else ""
    ## NON interesting attributes:
    std.attr <- "names"

    has.class <- !is.null(cl <- class(object))
    mod <- ""; char.like <- FALSE
    if(give.attr) a <- attributes(object)#-- save for later...

    if(is.function(object)) {
	cat(if(is.null(ao <- args(object)))
	    deparse(object)  else { dp <- deparse(ao); dp[-length(dp)] },"\n")
    } else if (is.null(object))
	cat(" NULL\n")
    else if(is.list(object)) {
	i.pl <- is.pairlist(object)
	if(le == 0) { cat(" ", if(i.pl)"pair", "list()\n",sep="")
		      return(invisible()) }
	is.d.f <- is.data.frame(object)
	if(is.d.f ||
	   (has.class && any(sapply(paste("str", cl, sep="."),
					#use sys.function(.) ..
				    function(ob)exists(ob, mode = "function",
						       inherits = TRUE))))) {
	    ##---- str.default	is a 'NextMethod' : omit the 'List of ..' ----
	    std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	} else {
	    cat(if(i.pl) "Dotted pair list" else "List",
		" of ", le, "\n", sep="")
	}
	if (max.level==0 || nest.lev < max.level) {
	    nam.ob <-
		if(is.null(nam.ob <- names(object))) rep("", le)
		else { max.ncnam <- max(nchar(nam.ob))
		       format.char(nam.ob, width = max.ncnam, flag = '-')
		   }
	    for(i in 1:le) {
		cat(indent.str,"$ ", nam.ob[i], ":", sep="")
		str(object[[i]], nest.lev = nest.lev + 1,
		    indent.str = paste(indent.str,".."),
		    max.level= max.level, vec.len= vec.len, digits.d= digits.d,
		    give.attr = give.attr, give.length= give.length, wid=wid)
	    }
	}
    } else { #- not function, not list
	if(is.vector(object)
	   || (is.array(object) && is.atomic(object))
	   || is.vector(object, mode='language')
	   || is.vector(object, mode='symbol')## R bug(<=0.50-a4) should be part
	   ) { ##-- Splus: FALSE for 'named vectors'
	    if(is.atomic(object)) {
		##-- atomic:   numeric	complex	 character  logical
		mod <- substr(mode(object), 1, 4)
		if     (mod == "nume")
		    mod <- if(is.integer(object)) "int"
		    else if(has.class) cl[1] else "num"
		else if(mod == "char") { mod <- "chr"; char.like <- TRUE }
		else if(mod == "comp") mod <- "cplx" #- else: keep 'logi'
		if(is.array(object)) {
		    di <- dim(object)
		    di <- paste(ifelse(di>1, "1:",""), di,
				ifelse(di>0, "" ," "), sep = "")
		    le.str <- paste(c("[", paste(di[-length(di)], ", ", sep=""),
				      di[length(di)], "]"), collapse = "")
		    std.attr <- "dim" #- "names"
		} else if(!is.null(names(object))) {
		    mod <- paste("Named", mod)
		    std.attr <- std.attr[std.attr != "names"]
		}
		str1 <- if(le == 1) paste(NULL, mod)
		else	   paste(" ", mod, if(le>0)" ", le.str, sep = "")
	    } else { ##-- not atomic, but vector: #
		mod <- typeof(object)#-- typeof(.) is more precise than mode!
		str1 <- switch(mod,
			       call = " call",
			       language = " language",
			       symbol = " symbol",
			       expression = " ",# "expression(..)" by deparse(.)
			       name = " name",
			       ##not in R:argument = "",# .Argument(.) by deparse(.)
			       ## in R (once):	comment.expression

			       ## default :
			       paste("		#>#>", mod, NULL)
			       )
	    }
	} else if (inherits(object,"rts") || inherits(object,"cts")
		   || inherits(object,"its")) {
	    tsp.a <- tspar(object)
	    t.cl <- cl[b.ts <- substring(cl,2,3) == "ts"] # "rts" "cts" or "its"
	    ts.kind <- switch(t.cl,
			      rts="Regular", cts="Calendar", its="Irregular")
	    ## from  print.summary.ts(.) :
	    pars <- unlist(sapply(summary(object)$ pars, format,
				  nsmall=0, digits=digits.d, justify = "none"))
	    if(length(pars)>=4) pars <- pars[-3]
	    pars <- paste(abbreviate(names(pars),min=2), pars,
			  sep= "=", collapse=", ")
	    str1 <- paste(ts.kind, " Time-Series ", le.str, " ", pars, ":",
			  sep = "")
	    vec.len <- switch(t.cl,rts=.8, cts=.6, its=.9) * vec.len
	    class(object) <- if(any(!b.ts)) cl[!b.ts]
	    std.attr <- c(std.attr, "tspar")
	} else if(is.ts(object)) {
	    tsp.a <- tsp(object)
	    str1 <- paste(" Time-Series ", le.str, " from ", format(tsp.a[1]),
			  " to ", format(tsp.a[2]), ":", sep = "")
	    std.attr <- c("tsp","class") #- "names"
	} else if (is.factor(object)) {
	    nl <- length(lev.att <- levels(object))
	    if(!is.character(lev.att)) {# should not happen..
		warning("`object' doesn't have legal levels()!")
		nl <- 0
	    }
	    object <- unclass(object)
	    if(nl) {
		lenl <- cumsum(3 + nchar(lev.att))# level space
		ml <- if(nl <= 1 || lenl[nl] <= 13)
		    nl else which(lenl > 13)[1]
		if((d <- lenl[ml] - if(ml>1)18 else 14) >= 3)# truncate last
		    lev.att[ml] <-
			paste(substring(lev.att[ml],1, nchar(lev.att[ml])-d),
			      "..", sep="")
	    }
	    else # nl == 0
		ml <- length(lev.att <- "")

	    str1 <- paste(" Factor w/ ", nl, " level",if(nl!=1) "s",
			  if(nl)' "', paste(lev.att[1:ml], collapse ='","'),
			  if(nl)'"', if(ml < nl)",..", ":", sep="")
	    std.attr <- c("levels","class")
	} else if(has.class) {
	    ## str1 <- paste("Class '",cl,"' of length ", le, " :", sep="")
	    ##===== NB. cl may be of length > 1 !!! ===========
	    cat("Class ", cl, " ", sep="'")
	    ## has.method <- exists( paste("str", cl, sep=".") )
	    ##== If there is a str.METHOD,
	    ##== it should have been called BEFORE this !
	    str(unclass(object),
		max.level = max.level, vec.len = vec.len, digits.d = digits.d,
		indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1,
		give.attr = give.attr, wid=wid)
	    return(invisible())
	} else if(is.atomic(object)) {
	    if((1 == length(a <- attributes(object))) && (names(a) == "names"))
		str1 <- paste(" Named vector", le.str)
	    else {
		##-- atomic / not-vector  "unclassified object" ---
		str1 <- paste(" atomic", le.str)
	    }
	} else {
	    ##-- NOT-atomic / not-vector  "unclassified object" ---
	    ##str1 <- paste(" ??? of length", le, ":")
	    str1 <- paste("length", le)
	}
###-###-- end  if elseif elseif .. --------------------------

	##-- This needs some improvement: Not list nor atomic --
	if ((is.language(object) || !is.atomic(object)) && !has.class) {
	    ##-- has.class superfluous --
	    mod <- mode(object)
	    give.mode <- FALSE
	    if (mod == "call" || mod == "language" || mod == "symbol"
		|| is.environment(object)) {
		##give.mode <- !is.vector(object)#--then it has not yet been done
		object <- deparse(object)
		le <- length(object) #== 1, always / depending on char.length ?
		format.fun <- function(x)x
		vec.len <- round(.5 * vec.len)
	    } else if (mod == "expression") {
		format.fun <- function(x) deparse(as.expression(x))
		vec.len <- round(.75 * vec.len)
	    } else if (mod == "name"){
		object <- paste(object)#-- show `as' char
	    } else if (mod == "argument"){
		format.fun <- deparse
	    } else {
		give.mode <- TRUE
	    }
	    if(give.mode) str1 <- paste(str1, ', mode "', mod,'":', sep = "")

	} else if(is.logical(object)) {
	    vec.len <- 3 * vec.len
	    format.fun <- format
	} else if(is.numeric(object)) {
	    ivec.len <- round(2.5 * vec.len)
	    if(!is.integer(object)){
		ob <- if(le > ivec.len) object[1:ivec.len] else object
		ao <- abs(ob <- ob[!is.na(ob)])
	    }
	    if(is.integer(object) || mod == "Surv" ||
	       (all(ao > 1e-10 | ao==0) && all(ao < 1e10| ao==0) &&
		all(ob == signif(ob, digits.d)))) {
		vec.len <- ivec.len
		format.fun <- function(x)x
	    } else {
		vec.len <- round(1.25 * vec.len)
		format.fun <- format
	    }
	} else if(is.complex(object)) {
	    vec.len <- round(.75 * vec.len)
	    format.fun <- format
	}

	if(char.like) {
	    bracket <- if (le>0) '"' else ""
	    format.fun <- function(x)x
	    vec.len <- sum(cumsum(3 + if(le>0) nchar(object) else 0) <
			   wid - (4 + 5*nest.lev + nchar(str1)))
				##    5*nest  is 'arbitrary'
	} else {
	    bracket <- ""
	    if(!exists("format.fun", inherits=TRUE)) #-- define one --
		format.fun <-
		    if(mod == 'num' || mod == 'cplx') format
		    else	   as.character
	}
	if(is.na(le)) { warning("'str.default': 'le' is NA !!"); le <- 0}

	vec.len <- max(1,round(vec.len))
	cat(str1, " ", bracket,
	    paste(format.fun(if(le>1) object[1:min(vec.len, le)] else object),
		  collapse = paste(bracket, " ", bracket, sep="")),
	    bracket, if(le > vec.len) " ...", "\n", sep="")

    } ## else (not function nor list)----------------------------------------

    if(give.attr) { #possible:	 || has.class && any(cl == 'terms')
	nam <- names(a)
	for (i in seq(len=length(a)))
	    if (all(nam[i] != std.attr)) { #-- only `non-standard' attributes:
		cat(indent.str,paste('- attr(*, "',nam[i],'")=',sep=''),sep="")
		str(a[[i]],
		    indent.str = paste(indent.str,".."), nest.lev= nest.lev+1,
		    max.level= max.level, vec.len= vec.len, digits.d= digits.d,
		    give.attr= give.attr, give.length = give.length, wid = wid)
	    }
    }
    invisible()	 ## invisible(object)#-- is SLOOOOW on large objects
} #-- end of function 'str.default' --

ls.str <- function(..., mode = "any", max.level = 1, give.attr = FALSE)
{
    ##--- An extended "ls()" using  str(.) --
    for(name in ls(..., envir = sys.frame(sys.parent())))
	if(exists(name, mode = mode)) {
	    cat(name, ": ")
	    str(get(name, mode = mode), max.level = max.level,
		give.attr = give.attr)
	}
    invisible()
}
lsf.str <- function(...)
{
    ##--- An extended "ls()" -- find ONLY functions -- using  str(.) --
    r <- character(0)
    for(name in ls(..., envir = sys.frame(sys.parent())))
	if(is.function(get(name))) {
	    cat(name, ": ")
	    r <- c(r,name)
	    str(get(name))
	}
    invisible(r)
}
