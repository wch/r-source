####------ str : show STRucture of an R object
str <- function(object, ...) UseMethod("str")

str.data.frame <- function(object, ...)
{
    ## Method to 'str' for  'data.frame' objects
    if(! is.data.frame(object)) {
	warning("str.data.frame() called with non-data.frame -- coercing to one.")
	object <- data.frame(object)
    }

    ## Show further classes // Assume that they do NOT have an own Method --
    ## not quite perfect ! (.Class = 'remaining classes', starting with current)
    cl <- oldClass(object); cl <- cl[cl != "data.frame"]  #- not THIS class
    if(0 < length(cl)) cat("Classes", cl, " and ")

    cat("`data.frame':	", nrow(object), " obs. of  ",
	(p <- length(object)), " variable", if(p != 1)"s", if(p > 0)":",
	"\n",sep="")

    ## calling next method, usually  str.default:
    if(length(l <- list(...)) && any("give.length" == names(l)))
	invisible(NextMethod("str", ...))
    else invisible(NextMethod("str", give.length=FALSE,...))
}

str.default <-
    function(object, max.level = NA, vec.len = 4, digits.d = 3,
	     nchar.max = 128, give.attr = TRUE, give.length = TRUE,
	     wid = getOption("width"), nest.lev = 0,
	     indent.str= paste(rep.int(" ", max(0,nest.lev+1)), collapse= ".."),
	     comp.str="$ ", no.list = FALSE, envir = baseenv(),
	     ...)
{
    ## Purpose: Display STRucture of any R - object (in a compact form).
    ## --- see HELP file --
    ## ------------------------------------------------------------------------
    ## Author: Martin Maechler <maechler@stat.math.ethz.ch>	1990--1997
    ## ------ Please send Bug-reports, -fixes and improvements !
    ## ------------------------------------------------------------------------

    oo <- options(digits = digits.d); on.exit(options(oo))
    le <- length(object)
    P0 <- function(...) paste(..., sep="")
    maybe_truncate <- function(x, e.x = x, Sep = "\"", ch = "| __truncated__")
    {
	trimmed <- strtrim(e.x, nchar.max)
	ii <- trimmed != e.x
	ii[is.na(ii)] <- FALSE
	if(any(ii)) x[ii] <- P0(trimmed[ii], Sep, ch)
	x
    }

    ## le.str: not used for arrays:
    le.str <-
	if(is.na(le)) " __no length(.)__ "
	else if(give.length) {
	    if(le > 0) P0("[1:", paste(le), "]") else "(0)"
	} else ""
    v.len <- vec.len # modify v.len, not vec.len!
    ## NON interesting attributes:
    std.attr <- "names"

    #NOT yet:if(has.class <- !is.null(cl <- class(object)))
    if(has.class <- !is.null(cl <- attr(object, "class"))) { # S3 or S4 class
	## FIXME: a kludge
	S4 <- !is.null(attr(cl, "package")) || cl == "classRepresentation"
	## better, but needs 'methods':	  length(methods::getSlots(cl)) > 0
    }
    mod <- ""; char.like <- FALSE
    if(give.attr) a <- attributes(object)#-- save for later...

    if(is.function(object)) {
	cat(if(is.null(ao <- args(object))) deparse(object)
	else { dp <- deparse(ao); paste(dp[-length(dp)], collapse="\n") },"\n")
    } else if (is.null(object))
	cat(" NULL\n")
    else if(has.class && S4) {
	a <- sapply(.slotNames(object), slot, object=object, simplify = FALSE)
	cat("Formal class", " '", paste(cl, collapse = "', '"),
	    "' [package \"", attr(cl,"package"), "\"] with ",
	    length(a)," slots\n", sep="")
	str(a, no.list = TRUE, comp.str = "@ ", # instead of "$ "
	    max.level = max.level, vec.len = vec.len, digits.d = digits.d,
	    indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1,
	    nchar.max = nchar.max, give.attr = give.attr, wid=wid)
	return(invisible())
    }
    else if(is.list(object)) {
	i.pl <- is.pairlist(object)
	is.d.f <- is.data.frame(object)
	##?if(is.d.f) std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	if(le == 0) {
	    if(is.d.f) std.attr <- c(std.attr, "class", "row.names")
	    else cat(" ", if(i.pl)"pair", "list()\n",sep="")
	} else {
	    if(no.list || (has.class &&
			   any(sapply(paste("str", cl, sep="."),
					#use sys.function(.) ..
				      function(ob)exists(ob, mode= "function",
							 inherits= TRUE))))) {
		## str.default is a 'NextMethod' : omit the 'List of ..'
		std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	    } else {
		cat(if(i.pl) "Dotted pair list" else "List",
		    " of ", le, "\n", sep="")
	    }
	    if (is.na(max.level) || nest.lev < max.level) {
		nam.ob <-
		    if(is.null(nam.ob <- names(object))) rep.int("", le)
		    else { max.ncnam <- max(nchar(nam.ob, type="w"))
			   format(nam.ob, width = max.ncnam, justify="left")
		       }
		for(i in 1:le) {
		    cat(indent.str, comp.str, nam.ob[i], ":", sep="")
		    envir <- # pass envir for 'promise' components:
			if(typeof(object[[i]]) == "promise") {
			    structure(object, nam= as.name(nam.ob[i]))
			} # else NULL
		    str(object[[i]], nest.lev = nest.lev + 1,
                        indent.str = paste(indent.str,".."),
                        nchar.max = nchar.max, max.level = max.level,
                        vec.len = vec.len, digits.d = digits.d,
                        give.attr = give.attr, give.length = give.length,
                        wid = wid, envir = envir)
		}
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
		    di. <- dim(object)
		    di <- P0(ifelse(di. > 1, "1:",""), di.,
			     ifelse(di. > 0, "" ," "))
		    le.str <- paste(c("[", P0(di[-length(di)], ", "),
				      di[length(di)], "]"), collapse = "")
		    std.attr <- "dim" #- "names"
		} else if(!is.null(names(object))) {
		    mod <- paste("Named", mod)
		    std.attr <- std.attr[std.attr != "names"]
		}
		str1 <-
		    if(le == 1 && !is.array(object)) paste(NULL, mod)
		    else P0(" ", mod, if(le>0)" ", le.str)
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
#  These are S-PLUS classes not found in R.
#	} else if (inherits(object,"rts") || inherits(object,"cts")
#		   || inherits(object,"its")) {
#	    tsp.a <- tspar(object)
#	    t.cl <- cl[b.ts <- substring(cl,2,3) == "ts"] # "rts" "cts" or "its"
#	    ts.kind <- switch(t.cl,
#			      rts="Regular", cts="Calendar", its="Irregular")
#	    ## from  print.summary.ts(.) :
#	    pars <- unlist(sapply(summary(object)$ pars, format,
#				  nsmall=0, digits=digits.d, justify = "none"))
#	    if(length(pars)>=4) pars <- pars[-3]
#	    pars <- paste(abbreviate(names(pars),min=2), pars,
#			  sep= "=", collapse=", ")
#	    str1 <- P0(ts.kind, " Time-Series ", le.str, " ", pars, ":")
#	    v.len <- switch(t.cl,rts=.8, cts=.6, its=.9) * v.len
#	    class(object) <- if(any(!b.ts)) cl[!b.ts]
#	    std.attr <- c(std.attr, "tspar")
	} else if(stats::is.ts(object)) {
	    tsp.a <- stats::tsp(object)
	    str1 <- P0(" Time-Series ", le.str, " from ", format(tsp.a[1]),
		       " to ", format(tsp.a[2]), ":")
	    std.attr <- c("tsp","class") #- "names"
	} else if (is.factor(object)) {
	    nl <- length(lev.att <- levels(object))
	    if(!is.character(lev.att)) {# should not happen..
		warning("'object' does not have valid levels()")
		nl <- 0
	    } else lev.att <- encodeString(lev.att, na = FALSE, quote = '"')
	    ord <- is.ordered(object)
	    object <- unclass(object)
	    if(nl) {
		## as from 2.1.0, quotes are included ==> '-2':
		lenl <- cumsum(3 + (nchar(lev.att, type="w") - 2))# level space
		ml <- if(nl <= 1 || lenl[nl] <= 13)
		    nl else which(lenl > 13)[1]
		lev.att <- maybe_truncate(lev.att[1:ml])
##		if((d <- lenl[ml] - if(ml>1)18 else 14) >= 3)# truncate last
##		    lev.att[ml] <-
##			P0(substring(lev.att[ml],1, nchar(lev.att[ml])-d), "..")
	    }
	    else # nl == 0
		ml <- length(lev.att <- "")

	    lsep <- if(ord) "<" else ","
	    str1 <- P0(if(ord)" Ord.f" else " F",
		       "actor w/ ", nl, " level", if(nl != 1) "s",
		       if(nl) " ",
		       if(nl) P0(lev.att, collapse = lsep),
		       if(ml < nl) P0(lsep, ".."), ":")

	    std.attr <- c("levels", "class")
	} else if(typeof(object) %in%
		  c("externalptr", "weakref", "environment")) {
	    ## Careful here, we don't want to change pointer objects
	    if(has.class)
		cat("Class", if(length(cl) > 1) "es",
		" '", paste(cl, collapse = "', '"), "' ", sep="")
	    le <- v.len <- 0
	    str1 <- paste("<", typeof(object), ">", sep="")
	    if(typeof(object) == "environment")
		str1 <- paste("length", length(object), str1)
	    has.class <- TRUE # fake for later
	    std.attr <- "class"
	    ## ideally we would figure out if as.character has a
	    ## suitable method and use that.
	} else if(has.class) {
	    cat("Class", if(length(cl) > 1) "es",
		" '", paste(cl, collapse = "', '"), "' ", sep="")
	    ## If there's a str.<method>, it should have been called before!
	    str(unclass(object),
		max.level = max.level, vec.len = vec.len, digits.d = digits.d,
		indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1,
		nchar.max = nchar.max, give.attr = give.attr, wid=wid)
	    return(invisible())
	} else if(is.atomic(object)) {
	    if((1 == length(a <- attributes(object))) && (names(a) == "names"))
		str1 <- paste(" Named vector", le.str)
	    else {
		##-- atomic / not-vector  "unclassified object" ---
		str1 <- paste(" atomic", le.str)
	    }
	} else if(typeof(object) == "promise") {
	    cat(" promise ")
	    if (!is.null(envir)) {
		objExp <- eval(bquote(substitute(.(attr(envir, "nam")), envir)))
		cat("to ")
		str(objExp,
		    max.level= max.level, vec.len= vec.len, digits.d= digits.d,
		    indent.str = indent.str, nest.lev = nest.lev,
		    nchar.max = nchar.max, give.attr = give.attr, wid=wid)
	    } else cat(" <...>\n")
	    return(invisible())
	} else {
	    ##-- NOT-atomic / not-vector  "unclassified object" ---
	    ##str1 <- paste(" ??? of length", le, ":")
	    str1 <- paste("length", le)
	}
	##-- end  if else..if else...  {still non-list case}

	##-- This needs some improvement: Not list nor atomic --
	if ((is.language(object) || !is.atomic(object)) && !has.class) {
	    ##-- has.class superfluous --
	    mod <- mode(object)
	    give.mode <- FALSE
	    if (any(mod == c("call", "language", "(", "symbol"))
		|| is.environment(object)) {
		##give.mode <- !is.vector(object)# then it has not yet been done
		if(mod == "(") give.mode <- TRUE
		typ <- typeof(object)
		object <- deparse(object)

		le <- length(object) # is > 1 e.g. for {A;B} language
		format.fun <- function(x)x
		v.len <- round(.5 * v.len)
		if(le > 1 && typ=="language" && object[1] == "{" && object[le]=="}") {
		    v.len <- v.len + 2
		    if(le >= 3) {
			object <- c(object[1],
				    paste(sub("^ +", " ", object[2:(le-1)]),
					  collapse = ";"),
				    object[le])
			le <- length(object)
		    }
		}
	    } else if (mod == "expression") {
		format.fun <- function(x) deparse(as.expression(x))
		v.len <- round(.75 * v.len)
	    } else if (mod == "name"){
		object <- paste(object)#-- show `as' char
	    } else if (mod == "argument"){
		format.fun <- deparse
	    } else {
		give.mode <- TRUE
	    }
	    if(give.mode) str1 <- P0(str1, ', mode "', mod,'":')

	} else if(is.logical(object)) {
	    v.len <- 3 * v.len
	    format.fun <- format
	} else if(is.numeric(object)) {
	    iv.len <- round(2.5 * v.len)
	    if(iSurv <- inherits(object, "Surv"))
		std.attr <- c(std.attr, "class")
	    int.surv <- iSurv || is.integer(object)
	    if(!int.surv) {
		ob <- if(le > iv.len) object[seq(len=iv.len)] else object
		ao <- abs(ob <- ob[!is.na(ob)])
	    }
	    else if(iSurv)
		le <- length(object <- as.character(object))
	    if(int.surv || (all(ao > 1e-10 | ao==0) && all(ao < 1e10| ao==0) &&
			    all(ob == signif(ob, digits.d)))) {
		if(!iSurv || di.[2] == 2)
		    ## use integer-like length
		    v.len <- iv.len
		format.fun <- function(x)x
	    } else {
		v.len <- round(1.25 * v.len)
		format.fun <- format
	    }
	} else if(is.complex(object)) {
	    v.len <- round(.75 * v.len)
	    format.fun <- format
	}

	## Not sure, this is ever triggered:
	if(is.na(le)) { warning("'str.default': 'le' is NA"); le <- 0}

	if(char.like) {
	    en_object <- encodeString(object)
	    v.len <-
		if(missing(vec.len))
		    max(1,sum(cumsum(3 + if(le>0) nchar(en_object, type="w") else 0) <
			      wid - (4 + 5*nest.lev + nchar(str1, type="w"))))
	    ## `5*ne..' above is fudge factor
		else round(v.len)
	    ile <- min(le, v.len)
	    if(ile >= 1)  # truncate if LONG char:
		object <- maybe_truncate(encodeString(object,
						      quote= '"', na= FALSE))
					#en_object[1:ile]
	    formObj <- function(x) paste(as.character(x), collapse=" ")
	}
	else {
	    if(!exists("format.fun", inherits=TRUE)) #-- define one --
		format.fun <-
		    if(mod == 'num' || mod == 'cplx') format else as.character
	    ## v.len <- max(1,round(v.len))
	    ile <- min(v.len, le)
	    formObj <- function(x) paste(format.fun(x), collapse = " ")
	}

	cat(str1, " ", formObj(if(ile >= 1) object[1:ile] else
			       if(v.len > 0) object),
	    if(le > v.len) " ...", "\n", sep="")

    } ## else (not function nor list)----------------------------------------

    if(give.attr) { ## possible: || has.class && any(cl == 'terms')
	nam <- names(a)
	for (i in seq(len=length(a)))
	    if (all(nam[i] != std.attr)) {# only `non-standard' attributes:
		cat(indent.str, P0('- attr(*, "',nam[i],'")='),sep="")
		str(a[[i]],
		    indent.str= paste(indent.str,".."), nest.lev= nest.lev+1,
		    max.level = max.level, digits.d = digits.d,
		    nchar.max = nchar.max,
		    vec.len = if(nam[i] == "source") 1 else vec.len,
		    give.attr= give.attr, give.length= give.length, wid= wid)
	    }
    }
    invisible()	 ## invisible(object)#-- is SLOOOOW on large objects
}# end of `str.default()'

## An extended `ls()' whose print method will use str() :
ls.str <-
    function(pos = 1, pattern, ..., envir = as.environment(pos), mode = "any")
{
    nms <- ls(..., envir = envir, pattern = pattern)
    r <- sapply(nms, function(n)
		if(exists(n, envir= envir, mode= mode)) n else as.character(NA))
    names(r) <- NULL
    structure(r[!is.na(r)], envir = envir, mode = mode, class = "ls_str")
}

lsf.str <- function(pos = 1, ..., envir = as.environment(pos))
    ls.str(pos = pos, envir = envir, mode = "function", ...)

print.ls_str <- function(x, max.level = 1, give.attr = FALSE, ...)
{
    E <- attr(x, "envir") # can be NULL for "package:base"
    if(!is.null(E)) stopifnot(is.environment(E))
    M <- attr(x, "mode")
    for(nam in x) {
	cat(nam, ": ")
	str(get(nam, envir = E, mode = M),
	    max.level = max.level, give.attr = give.attr, ...)
    }
    invisible(x)
}
