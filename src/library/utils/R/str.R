#  File src/library/utils/R/str.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

####------ str : show STRucture of an R object
str <- function(object, ...) UseMethod("str")

## FIXME: convert to use sQuote
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
    if(0 < length(cl)) cat("Classes", paste(sQuote(cl), collapse=", "), "and ")

    cat("'data.frame':	", nrow(object), " obs. of  ",
	(p <- length(object)), " variable", if(p != 1)"s", if(p > 0)":",
	"\n", sep = "")

    ## calling next method, usually  str.default:
    if(length(l <- list(...)) && any("give.length" == names(l)))
	invisible(NextMethod("str", ...))
    else invisible(NextMethod("str", give.length=FALSE,...))
}

str.Date <- str.POSIXt <- function(object, ...) {
    cl <- oldClass(object)
    ## be careful to be fast for large object:
    n <- length(object) # FIXME, could be NA
    if(n == 0L) return(str.default(object))
    if(n > 1000L) object <- object[seq_len(1000L)]

    give.length <- TRUE ## default
    ## use 'give.length' when specified, else default = give.head
    if(length(larg <- list(...))) {
	nl <- names(larg)
	which.head <- which(nl == "give.head")
	if (any(Bgl <- nl == "give.length"))
	    give.length <- larg[[which(Bgl)]]
	else if(length(which.head))
	    give.length <- larg[[which.head]]
	if(length(which.head)) # eliminate it from arg.list
	    larg <- larg[ - which.head ]
	if(is.numeric(larg[["nest.lev"]]) &&
	   is.numeric(larg[["vec.len"]])) # typical call from data.frame
	    ## reduce length for typical call:
	    larg[["vec.len"]] <-
		min(larg[["vec.len"]],
		    (larg[["width"]]- nchar(larg[["indent.str"]]) -31)%/% 19)
    }

    le.str <- if(give.length) paste0("[1:",as.character(n),"]")
    cat(" ", cl[1L], le.str,", format: ", sep = "")
    ## do.call(str, c(list(format(object), give.head = FALSE), larg))
    ## ensuring 'object' is *not* copied:
    str.f.obj <- function(...) str(format(object), ...)
    do.call(str.f.obj, c(list(give.head = FALSE), larg))
}

strOptions <- function(strict.width = "no", digits.d = 3, vec.len = 4,
                       drop.deparse.attr = TRUE,
		       formatNum = function(x, ...)
		       format(x, trim=TRUE, drop0trailing=TRUE, ...))
    list(strict.width = strict.width, digits.d = digits.d, vec.len = vec.len,
	 drop.deparse.attr = drop.deparse.attr,
	 formatNum = match.fun(formatNum))

str.default <-
    function(object, max.level = NA, vec.len = strO$vec.len,
	     digits.d = strO$digits.d,
	     nchar.max = 128, give.attr = TRUE,
	     drop.deparse.attr = strO$drop.deparse.attr,
	     give.head = TRUE, give.length = give.head,
	     width = getOption("width"), nest.lev = 0,
	     indent.str= paste(rep.int(" ", max(0,nest.lev+1)), collapse= ".."),
	     comp.str="$ ", no.list = FALSE, envir = baseenv(),
	     strict.width = strO$strict.width,
	     formatNum = strO$formatNum, list.len = 99,
	     ...)
{
    ## Purpose: Display STRucture of any R - object (in a compact form).
    ## --- see HELP file --
    ## ------------------------------------------------------------------------
    ## Author: Martin Maechler <maechler@stat.math.ethz.ch>	1990--1997

    ## strOptions() defaults for
    oDefs <- c("vec.len", "digits.d", "strict.width", "formatNum",
	       "drop.deparse.attr")
    ## from
    strO <- getOption("str")
    if (!is.list(strO)) {
	warning('invalid options("str") -- using defaults instead')
	strO <- strOptions()
    }
    else {
        if (!all(names(strO) %in% oDefs))
            warning(gettextf("invalid components in options(\"str\"): %s",
                             paste(setdiff(names(strO), oDefs), collapse = ", ")),
                    domain = NA)
        strO <- modifyList(strOptions(), strO)
    }
    strict.width <- match.arg(strict.width, choices = c("no", "cut", "wrap"))
    if(strict.width != "no") {
	## using eval() would be cleaner, but fails inside capture.output():
	ss <- capture.output(str.default(object, max.level = max.level,
				 vec.len = vec.len, digits.d = digits.d,
				 nchar.max = nchar.max,
				 give.attr= give.attr, give.head= give.head,
				 give.length= give.length,
				 width = width, nest.lev = nest.lev,
				 indent.str = indent.str, comp.str= comp.str,
				 no.list= no.list || is.data.frame(object),
				 envir = envir, strict.width = "no",
				 formatNum = formatNum, list.len = list.len,
					 ...) )
	if(strict.width == "wrap") {
	    nind <- nchar(indent.str) + 2
	    ss <- strwrap(ss, width = width, exdent = nind)
					# wraps at white space (only)
	}
	if(length(iLong <- which(nchar(ss) > width))) { ## cut hard
	    sL <- ss[iLong]
	    k <- as.integer(width-2L)
	    if(any(i <- grepl("\"", substr(sL, k +1L, nchar(sL))))) {
		## care *not* to cut off the closing   "  at end of
		## string that's already truncated {-> maybe_truncate()} :
		ss[iLong[ i]] <- paste0(substr(sL[ i], 1, k-1L), "\"..")
		ss[iLong[!i]] <- paste0(substr(sL[!i], 1, k), "..")
	    } else {
		ss[iLong] <- paste0(substr(sL, 1, k),"..")
	    }
	}
	cat(ss, sep = "\n")
	return(invisible())
    }

    oo <- options(digits = digits.d); on.exit(options(oo))
    le <- length(object)[1L] # [1]: protect from nonsense
    if(is.na(le)) {
        warning("'str.default': 'le' is NA, so taken as 0", immediate. = TRUE)
        le <- 0
        vec.len <- 0
    }

    nchar.w <- function(x) nchar(x, type="w", allowNA=TRUE)
    ncharN  <- function(x) {
	r <- nchar(x, type="w", allowNA=TRUE)
	if(anyNA(r)) {
	    iN <- is.na(r)
	    r[iN] <- nchar(x[iN], type="bytes")
	}
	r
    }
    ## x: character ; using "global" 'nchar.max'
    maybe_truncate <- function(x, nx = nchar.w(x), S = "\"", ch = "| __truncated__")
    {
	ok <- if(anyNA(nx)) !is.na(nx) else TRUE
	if(any(lrg <- ok & nx > nchar.max)) {
	    nc <- nchar(ch <- paste0(S, ch))
	    if(nchar.max <= nc)
		stop(gettextf("'nchar.max = %d' is too small", nchar.max), domain=NA)
	    x.lrg <- x[lrg]
	    tr.x <- strtrim(x.lrg, nchar.max - nc)
	    if(any(ii <- tr.x != x.lrg & paste0(tr.x, S) != x.lrg)) {
		x[lrg][ii] <- paste0(tr.x[ii], ch)
	    }
	}
	x
    }
    pClass <- function(cls)
	paste0("Class", if(length(cls) > 1) "es",
	       " '", paste(cls, collapse = "', '"), "' ")

    nfS <- names(fStr <- formals())# names of all formal args to str.default()
    ##' Purpose: using short strSub() calls instead of long str() ones
    ##' @title Call str() on sub-parts, with mostly the *same* arguments
    ##' @param obj R object; always a "part" of the main 'object'
    ##' @param ... further arguments to str(), [often str.default()]
    strSub <- function(obj, ...) {
	## 'give.length', ...etc are *not* automatically passed down:
	nf <- setdiff(nfS, c("object", "give.length", "comp.str", "no.list",
			     ## drop fn.name & "obj" :
			     names(match.call())[-(1:2)], "..."))
	aList <- as.list(fStr)[nf]
	aList[] <- lapply(nf, function(n) eval(as.name(n)))
	## do.call(str, c(list(object=obj),aList,list(...)), quote=TRUE), *not* copying 'obj'
	do.call(function(...) str(obj, ...), c(aList, list(...)), quote = TRUE)
    }

    ## le.str: not used for arrays:
    le.str <-
	if(is.na(le)) " __no length(.)__ "
	else if(give.length) {
	    if(le > 0) paste0("[1:", paste(le), "]") else "(0)"
	} else ""
    v.len <- vec.len # modify v.len, not vec.len!
    ## NON interesting attributes:
    std.attr <- "names"

    cl <- if((S4 <- isS4(object))) class(object) else oldClass(object)
    has.class <- S4 || !is.null(cl) # S3 or S4
    mod <- ""; char.like <- FALSE
    if(give.attr) a <- attributes(object)#-- save for later...
    dCtrl <- eval(formals(deparse)$control)
    if(drop.deparse.attr) dCtrl <- dCtrl[dCtrl != "showAttributes"]
    deParse <- function(.) deparse(., width.cutoff = min(500L, max(20L, width-10L)),
				   control = dCtrl)
    n.of. <- function(n, singl, plural) paste(n, ngettext(n, singl, plural))
    n.of <- function(n, noun) n.of.(n, noun, paste0(noun,"s"))
    arrLenstr <- function(obj) {
	rnk <- length(di. <- dim(obj))
	di <- paste0(ifelse(di. > 1, "1:",""), di.,
		     ifelse(di. > 0, "" ," "))
	pDi <- function(...) paste(c("[", ..., "]"), collapse = "")
	if(rnk == 1)
	     pDi(di[1L], "(1d)")
	else pDi(paste0(di[-rnk], ", "), di[rnk])
    }
    if(is.ts <- stats::is.ts(object))
        str1.ts <- function(o, lestr) {
            tsp.a <- stats::tsp(o)
            paste0(" Time-Series ", lestr, " from ", format(tsp.a[1L]),
                   " to ", format(tsp.a[2L]), ":")
        }
    if (is.null(object))
	cat(" NULL\n")
    else if(S4) {
	trygetSlots <- function(x, nms) {
	    r <- tryCatch(sapply(nms, methods::slot, object=x, simplify = FALSE),
			  error = conditionMessage)
	    if(is.list(r))
		r
	    else {
		warning("Not a validObject(): ", r, call.=FALSE) # instead of error
		r <- attributes(x) ## "FIXME" low-level assumption about S4 slots
		r <- r[names(r) != "class"]
		dp <- list(methods::getDataPart(x, NULL.for.none=TRUE))
		if(!is.null(dp)) names(dp) <- methods:::.dataSlot(nms)
		c(r, dp)
	    }
	}
	if(methods::is(object,"envRefClass")) {
	    cld <- tryCatch(object$getClass(), error=function(e)e)
	    if(inherits(cld, "error")) {
		cat("Prototypical reference class", " '", paste(cl, collapse = "', '"),
		    "' [package \"", attr(cl,"package"), "\"]\n", sep="")
		## add a bit more info ??
		return(invisible())
	    }
	    nFlds <- names(cld@fieldClasses)
	    a <- sapply(nFlds, function(ch) object[[ch]], simplify = FALSE)
	    cat("Reference class", " '", paste(cl, collapse = "', '"),
		"' [package \"", attr(cl,"package"), "\"] with ",
                n.of(length(a), "field"), "\n", sep = "")
	    strSub(a, no.list=TRUE, give.length=give.length,
		   nest.lev = nest.lev + 1)
	    meths <- names(cld@refMethods)
	    oMeths <- meths[is.na(match(meths, methods:::envRefMethodNames))]
	    cat(indent.str, "and ", n.of(length(meths), "method"), sep = "")
	    sNms <- names(cld@slots)
	    if(lo <- length(oMeths)) {
		cat(", of which", lo, ngettext(lo, "is", "are", domain = NA), " possibly relevant")
		if (is.na(max.level) || nest.lev < max.level)
		    cat(":",
			strwrap(paste(sort(oMeths), collapse=", "),
				indent = 2, exdent = 2,
				prefix = indent.str, width=width),# exdent = nind),
			sep = "\n")
		else cat("\n")
	    }
	    if(length(sNms <- sNms[sNms != ".xData"])) {
		cat(" and ", n.of(length(sNms), "slot"), "\n", sep="")
		sls <- trygetSlots(object, sNms)
		strSub(sls, comp.str = "@ ", no.list=TRUE, give.length=give.length,
		       indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1)
	    }
	    else if(lo == 0) cat(".\n")
	}
	else { ## S4 non-envRefClass
	    sNms <- methods::.slotNames(object)
	    cat("Formal class", " '", paste(cl, collapse = "', '"),
		"' [package \"", attr(cl,"package"), "\"] with ",
		n.of(length(sNms), "slot"), "\n", sep = "")
	    a <- trygetSlots(object, sNms)
	    strSub(a, comp.str = "@ ", no.list=TRUE, give.length=give.length,
		   indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1)
	}
	return(invisible())
    }
    else if(is.function(object)) {
	cat(if(is.null(ao <- args(object))) deParse(object)
	else { dp <- deParse(ao); paste(dp[-length(dp)], collapse="\n") },"\n")
    } else if(is.list(object)) {
	i.pl <- is.pairlist(object)
	is.d.f <- is.data.frame(object)
	##?if(is.d.f) std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	if(le == 0) {
	    if(is.d.f) std.attr <- c(std.attr, "class", "row.names")
	    else cat(" ", if(!is.null(names(object))) "Named ",
		     if(i.pl)"pair", "list()\n", sep = "")
	} else { # list, length >= 1 :
	    if(irregCl <- has.class && identical(object[[1L]], object)) {
		le <- length(object <- unclass(object))
		std.attr <- c(std.attr, "class")
	    }
	    if(no.list || (has.class &&
			   any(sapply(paste0("str.", cl),
					#use sys.function(.) ..
				      function(ob)exists(ob, mode= "function",
							 inherits= TRUE))))) {
		## str.default is a 'NextMethod' : omit the 'List of ..'
		std.attr <- c(std.attr, "class", if(is.d.f) "row.names")
	    } else { # need as.character here for double lengths.
		cat(if(i.pl) "Dotted pair list" else
		    if(irregCl) paste(pClass(cl), "hidden list") else "List",
		    " of ", as.character(le), "\n", sep = "")
	    }
	    if (is.na(max.level) || nest.lev < max.level) {
		nam.ob <-
		    if(is.null(nam.ob <- names(object))) rep.int("", le)
		    else { ncn <- nchar.w(nam.ob)
			   if(anyNA(ncn)) ## slower, but correct:
			      ncn <- vapply(nam.ob, format.info, 0L)
			   format(nam.ob, width = max(ncn), justify="left")
		       }
		for (i in seq_len(min(list.len,le) ) ) {
		    cat(indent.str, comp.str, nam.ob[i], ":", sep = "")
		    envir <- # pass envir for 'promise' components:
			if(typeof(object[[i]]) == "promise") {
			    structure(object, nam= as.name(nam.ob[i]))
			} # else NULL
		    strSub(object[[i]], give.length=give.length,
                           nest.lev = nest.lev + 1,
                           indent.str = paste(indent.str,".."))
		}
		if(list.len < le)
		    cat(indent.str, "[list output truncated]\n")
	    }
	}
    } else { # not  NULL, S4, function, or list
	if (is.factor(object)) {
	    nl <- length(lev.att <- levels(object))
	    if(!is.character(lev.att)) {# should not happen..
		warning("'object' does not have valid levels()")
		nl <- 0
	    } else { ## protect against large nl:
                w <- min(max(width/2, 10), 1000)
                if(nl > w) lev.att <- lev.att[seq_len(w)]
                n.l <- length(lev.att) # possibly  n.l << nl
                lev.att <- encodeString(lev.att, na.encode = FALSE, quote = '"')
            }
	    ord <- is.ordered(object)
	    object <- unclass(object)
	    if(nl) {
		## as from 2.1.0, quotes are included ==> '-2':
		lenl <- cumsum(3 + (ncharN(lev.att) - 2))# level space
		ml <- if(n.l <= 1 || lenl[n.l] <= 13)
		    n.l else which.max(lenl > 13)
		lev.att <- maybe_truncate(lev.att[seq_len(ml)])
	    }
	    else # nl == 0
		ml <- length(lev.att <- "")

	    lsep <- if(ord) "<" else ","
	    str1 <-
		paste0(if(ord)" Ord.f" else " F", "actor",
                       if(is.array(object)) arrLenstr(object),
		       " w/ ", nl, " level", if(nl != 1) "s",
		       if(nl) " ",
		       if(nl) paste0(lev.att, collapse = lsep),
		       if(ml < nl) paste0(lsep, ".."), ":")

	    std.attr <- c("levels", "class", if(is.array(object)) "dim")
	} else if(is.ts) {
	    str1 <- str1.ts(object,
                            if(isA <- is.array(object)) arrLenstr(object) else le.str)
	    std.attr <- c("tsp", "class", if(isA) "dim")
        } else if(is.vector(object)
	       || is.atomic(object)
               ##f fails for formula:
               ##f typeof(object) in {"symbol", "language"} =: is.symbolic(.):
               ##f || (is.language(object) && !is.expression(object))
               || (is.language(object) && !is.expression(object) && !any(cl == "formula")) ) {
	    if(is.atomic(object)) {
		##-- atomic:   numeric{dbl|int} complex character logical raw
		mod <- substr(mode(object), 1, 4)
		if     (mod == "nume")
		    mod <- if(is.integer(object)) "int" else "num"
		else if(mod == "char") { mod <- "chr"; char.like <- TRUE }
		else if(mod == "comp") mod <- "cplx" #- else: keep 'logi'
		if(is.array(object)) {
		    le.str <- arrLenstr(object)
		    if(m <- match("AsIs", cl, 0L)) ## workaround bad format.AsIs()
			oldClass(object) <- cl[-m]
                    std.attr <- "dim"
		} else if(!is.null(names(object))) {
		    mod <- paste("Named", mod)
		    std.attr <- std.attr[std.attr != "names"]
		}
		if(has.class) {
		    cl <- cl[1L] # and "forget" potential other classes
		    if(cl != mod && substr(cl, 1L, nchar(mod)) != mod)
			mod <- paste0("'",cl,"' ", mod)
		    ## don't show the class *twice*
		    std.attr <- c(std.attr, "class")
		}
		str1 <-
		    if(le == 1 && !is.array(object)) paste(NULL, mod)
		    else paste0(" ", mod, if(le>0)" ", le.str)
	    } else { ##-- not atomic, but vector: #
		mod <- typeof(object)#-- typeof(.) is more precise than mode!
		str1 <- switch(mod,
			       call = " call",
			       language = " language",
			       symbol = " symbol",
			       expression = " ",# "expression(..)" by deParse(.)
			       name = " name",
			       ##not in R:argument = "",# .Argument(.) by deParse(.)
			       ## in R (once):	comment.expression

			       ## default :
			       paste("		#>#>", mod, NULL)
			       )
	    }
	} else if(typeof(object) %in%
		  c("externalptr", "weakref", "environment", "bytecode")) {
	    ## Careful here, we don't want to change pointer objects
	    if(has.class)
                cat(pClass(cl))
	    le <- v.len <- 0
	    str1 <-
		if(is.environment(object)) format(object)
		else paste0("<", typeof(object), ">")
	    has.class <- TRUE # fake for later
	    std.attr <- "class"
	    ## ideally we would figure out if as.character has a
	    ## suitable method and use that.
	} else if(has.class) {
	    cat("Class", if(length(cl) > 1) "es",
		" '", paste(cl, collapse = "', '"), "' ", sep = "")
	    ## If there's a str.<method>, it should have been called before!
	    uo <- unclass(object)
	    if(!is.null(attributes(uo)$class)) {
		## another irregular case
		xtr <- c(if(identical(uo, object)) { # trap infinite loop
		    class(uo) <- NULL
		    "unclass()-immune"
		} else if(!is.object(object)) "not-object")
		if(!is.null(xtr)) cat("{",xtr,"} ", sep = "")
	    }
	    strSub(uo, indent.str = paste(indent.str,".."), nest.lev = nest.lev + 1)
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
		strSub(objExp)
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
            trimEnds <- function(ch) sub(" +$", '', sub("^ +", ' ', ch))
	    if (any(mod == c("call", "language", "(", "symbol"))
		|| is.environment(object)) {
		##give.mode <- !is.vector(object)# then it has not yet been done
		if(mod == "(") give.mode <- TRUE
		typ <- typeof(object)
		object <- deParse(object)

		le <- length(object) # is > 1 e.g. for {A;B} language
		format.fun <- function(x) x
		v.len <- round(.5 * v.len)
		if(le > 1 && typ=="language" && object[1L] == "{" && object[le]=="}") {
		    v.len <- v.len + 2
		    if(le >= 3) {
			object <- c(object[1L],
				    paste(trimEnds(object[2:(le-1)]), collapse = ";"),
				    object[le])
			le <- length(object)
		    }
		}
	    } else if (mod == "expression") {
		format.fun <- function(x) trimEnds(deParse(as.expression(x)))
		v.len <- round(.75 * v.len)
	    } else if (mod == "name"){
		object <- paste(object)#-- show `as' char
	    } else if (mod == "argument"){
		format.fun <- deParse
	    } else {
		give.mode <- TRUE
	    }
	    if(give.mode) str1 <- paste0(str1, ', mode "', mod,'":')

	} else if(is.logical(object)) {
	    v.len <- 1.5 * v.len # was '3' originally (but S prints 'T' 'F' ..)
	    format.fun <- formatNum
	} else if(is.numeric(object)) {
	    iv.len <- round(2.5 * v.len)
	    if(iSurv <- inherits(object, "Surv"))
		std.attr <- c(std.attr, "class")
	    int.surv <- iSurv || is.integer(object)
	    if(!int.surv) {
		ob <- if(le > iv.len) object[seq_len(iv.len)] else object
		ao <- abs(ob <- unclass(ob[!is.na(ob)]))
	    }
	    else if(iSurv) {
		nc <- ncol(object)
		le <- length(object <- as.character(object))
	    }
	    if(int.surv || (all(ao > 1e-10 | ao==0) && all(ao < 1e10| ao==0) &&
			    all(abs(ob - signif(ob, digits.d)) <= 9e-16*ao))) {
		if(!iSurv || nc == 2L) # "Surv" : implemented as matrix
		    ## use integer-like length
		    v.len <- iv.len
		format.fun <- formatNum
	    } else {
		v.len <- round(1.25 * v.len)
		format.fun <- formatNum
	    }
	} else if(is.complex(object)) {
	    v.len <- round(.75 * v.len)
	    format.fun <- formatNum
	}

	if(char.like) {
	    ## if object is very long, drop the rest which won't be used anyway:
	    max.len <- max(100L, width %/% 3L + 1L, if(!missing(vec.len)) vec.len)
	    if(le > max.len) le <- length(object <- object[seq_len(max.len)])
	    ## For very long strings, truncated later anyway,
	    ## both nchar(*, type="w") and encodeString() are too expensive
	    trimWidth <- as.integer(nchar.max)
	    ## FIXME: need combined  encode.and.trim.string(object, m)  with O(m) !
	    encObj <- tryCatch(strtrim(object, trimWidth), error=function(e) NULL)
	    encObj <-
		if(is.null(encObj)) { # must first encodeString() before we can trim
		    e <- encodeString(object, quote= '"', na.encode= FALSE)
		    r <- tryCatch(strtrim(e, trimWidth), error=function(.) NULL)
		    ## What else can we try?
		    if(is.null(r)) e else r
		}
		else
		    encodeString(encObj, quote= '"', na.encode= FALSE)
	    if(le > 0) ## truncate if LONG char:
		encObj <- maybe_truncate(encObj)
	    v.len <-
		if(missing(vec.len)) {
		    max(1,sum(cumsum(1 + if(le>0) ncharN(encObj) else 0) <
			      width - (4 + 5*nest.lev + nchar(str1, type="w"))))
		}	 	      # '5*ne..' above is fudge factor
		else round(v.len)
	    ile <- min(le, v.len)
	    if(ile >= 1)
		object <- encObj[seq_len(ile)]
	    formObj <- function(x) paste(as.character(x), collapse = " ")
	}
	else { # not char.like
	    if(!exists("format.fun"))
		format.fun <-
		    if(mod == "num" || mod == "cplx") format else as.character
	    ## v.len <- max(1,round(v.len))
	    ile <- min(v.len, le)
	    formObj <- function(x) maybe_truncate(paste(format.fun(x), collapse = " "),
						  S = "") # *not* string-like
	}

	cat(if(give.head) paste0(str1, " "),
	    formObj(if(ile >= 1) object[seq_len(ile)] else if(v.len > 0) object),
	    if(le > v.len) " ...", "\n", sep = "")

    } ## else (not function nor list)----------------------------------------

    if(give.attr) { ## possible: || has.class && any(cl == "terms")
	nam <- names(a)
	for (i in seq_along(a))
	    if (all(nam[i] != std.attr)) {# only `non-standard' attributes:
		cat(indent.str, paste0('- attr(*, "', nam[i], '")='), sep = "")
		strSub(a[[i]], give.length = give.length,
		       indent.str = paste(indent.str, ".."), nest.lev = nest.lev+1)
	    }
    }
    invisible()	 ## invisible(object)#-- is SLOOOOW on large objects
}# end of `str.default()'

## An extended `ls()' whose print method will use str() :
ls.str <-
    function (pos = -1, name, envir, all.names = FALSE, pattern, mode = "any")
{
    if(missing(envir)) ## [for "lazy" reasons, this fails as default]
        envir <- as.environment(pos)
    nms <- ls(name, envir = envir, all.names=all.names, pattern=pattern)
    r <- vapply(nms, exists, NA, envir=envir, mode=mode, inherits=FALSE)
    structure(nms[r], envir = envir, mode = mode, class = "ls_str")
}

lsf.str <- function(pos = -1, envir, ...) {
    if(missing(envir)) ## [for "lazy" reasons, this fails as default]
        envir <- as.environment(pos)
    ls.str(pos=pos, envir=envir, mode = "function", ...)
}

print.ls_str <- function(x, max.level = 1, give.attr = FALSE,
                         ..., digits = max(1, getOption("str")$digits.d))
{
    E <- attr(x, "envir")
    stopifnot(is.environment(E))
    M <- attr(x, "mode")
    args <- list(...)
    if(length(args) && "digits.d" %in% names(args)) {
        if(missing(digits))
            digits <- args$digits.d
        else
            warning("'digits' and 'digits.d' are both specified and the latter is not used")
        args$digits.d <- NULL
    }
    strargs <- c(list(max.level = max.level, give.attr = give.attr,
                      digits.d = digits), args)
    n. <- substr(tempfile("ls_str_", tmpdir=""), 2L, 20L)
    for(nam in x) {
	cat(nam, ": ")
	## check missingness, e.g. inside debug(.) :

##__ Why does this give	 too many <missing> in some case?
##__	if(eval(substitute(missing(.), list(. = as.name(nam))),
##__		envir = E))
##__	    cat("<missing>\n")
##__	else
##__	    str(get(nam, envir = E, mode = M),
##__		max.level = max.level, give.attr = give.attr, ...)

	eA <- sprintf("%s:%s", nam, n.)
	o <- tryCatch(get(nam, envir = E, mode = M),
		      error = function(e){ attr(e, eA) <- TRUE; e })
	if(inherits(o, "error") &&  isTRUE(attr(o, eA))) {
	    cat(## FIXME: only works with "C" (or English) LC_MESSAGES locale!
		if(length(grep("missing|not found", o$message)))
		"<missing>" else o$message, "\n", sep = "")
	}
	else {
	    ## do.call(str, c(list(o), strargs),
	    ##	  quote = is.call(o) || is.symbol(o)) # protect calls from eval.
	    ## ensuring 'obj' is *not* copied:
	    strO <- function(...) str(o, ...)
	    do.call(strO, strargs, quote = is.call(o) || is.symbol(o))
					# protect calls from eval.
	}
    }
    invisible(x)
}
