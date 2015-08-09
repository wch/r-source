#  File src/library/stats/R/models.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

formula <- function(x, ...) UseMethod("formula")
formula.default <- function (x = NULL, env = parent.frame(), ...)
{
    notAtomic <- !is.atomic(x)
    notnull <- function(z) notAtomic && !is.null(z)

    if (notnull(x$formula)) eval(x$formula)
    else if (notnull(x$terms)) {z <- x$terms; oldClass(z) <- "formula"; z}
    else if (notnull(x$call$formula))	eval(x$call$formula)
    else if (!is.null(attr(x, "formula"))) attr(x, "formula")
    else {
        form <- switch(mode(x),
                       NULL = structure(NULL, class = "formula"),
                       character = formula(eval(parse(text = x, keep.source = FALSE)[[1L]])),
                       call = eval(x), stop("invalid formula"))
        environment(form) <- env
        form
    }
}
formula.formula <- function(x, ...) x
formula.terms <- function(x, ...) {
    env <- environment(x)
    attributes(x) <- list(class="formula")
    if (!is.null(env))
    	environment(x) <- env
    else
    	environment(x) <- globalenv()
    x
}

formula.data.frame <- function (x, ...)
{
    nm <- sapply(names(x), as.name)
    if (length(nm) > 1L) {
        rhs <- nm[-1L]
        lhs <- nm[1L]
    } else if (length(nm) == 1L) {
        rhs <- nm[1L]
        lhs <- NULL
    } else stop("cannot create a formula from a zero-column data frame")
    ff <- parse(text = paste(lhs, paste(rhs, collapse = "+"), sep = "~"),
                keep.source = FALSE)
    ff <- eval(ff)
    environment(ff) <- parent.frame()
    ff
}

formula.character <- function(x, env = parent.frame(), ...)
{
    ff <- formula(eval(parse(text=x, keep.source = FALSE)[[1L]]))
    environment(ff) <- env
    ff
}

print.formula <- function(x, showEnv = !identical(e, .GlobalEnv), ...)
{
    e <- environment(.x <- x) ## return(.) original x
    attr(x, ".Environment") <- NULL
    print.default(unclass(x), ...)
    if (showEnv) print(e)
    invisible(.x)
}

`[.formula` <- function(x,i) {
    ans <- NextMethod("[")
    ## as.character gives a vector.
    if(length(ans) == 0L || as.character(ans[[1L]])[1L] == "~") {
	class(ans) <- "formula"
        environment(ans) <- environment(x)
    }
    ans
}

as.formula <- function(object, env = parent.frame())
{
    if(inherits(object, "formula"))
        object
    else {
        rval <- formula(object, env = baseenv())
        if (identical(environment(rval), baseenv()) || !missing(env))
            environment(rval) <- env
        rval
    }
}

terms <- function(x, ...) UseMethod("terms")
terms.default <- function(x, ...) {
    v <- x$terms
    if(is.null(v)) {
        v <- attr(x, "terms")
        if(is.null(v)) stop("no terms component nor attribute")
    }
    v
}

terms.terms <- function(x, ...) x
print.terms <- function(x, ...) {
    print.default(unclass(x), ...)
    invisible(x)
}

## moved from base/R/labels.R
labels.terms <- function(object, ...) attr(object, "term.labels")

### do this `by hand' as previous approach was vulnerable to re-ordering.
delete.response <- function (termobj)
{
    a <- attributes(termobj)
    y <- a$response
    if(!is.null(y) && y) {
        termobj[[2L]] <- NULL
        a$response <- 0
        a$variables <- a$variables[-(1+y)]
        a$predvars <- a$predvars[-(1+y)]
        if(length(a$factors))
            a$factors <- a$factors[-y, , drop = FALSE]
        if(length(a$offset))
            a$offset <- ifelse(a$offset > y, a$offset-1, a$offset)
        if(length(a$specials))
            for(i in seq_along(a$specials)) {
                b <- a$specials[[i]]
                a$specials[[i]] <- ifelse(b > y, b-1, b)
            }
        attributes(termobj) <- a
    }
    termobj
}

reformulate <- function (termlabels, response=NULL, intercept = TRUE)
{
    if(!is.character(termlabels) || !length(termlabels))
        stop("'termlabels' must be a character vector of length at least one")
    has.resp <- !is.null(response)
    termtext <- paste(if(has.resp) "response", "~",
		      paste(termlabels, collapse = "+"),
		      collapse = "")
    if(!intercept) termtext <- paste(termtext, "- 1")
    rval <- eval(parse(text = termtext, keep.source = FALSE)[[1L]])
    if(has.resp) rval[[2L]] <-
        if(is.character(response)) as.symbol(response) else response
    ## response can be a symbol or call as  Surv(ftime, case)
    environment(rval) <- parent.frame()
    rval
}

drop.terms <- function(termobj, dropx = NULL, keep.response = FALSE)
{
    if (is.null(dropx))
	termobj
    else {
        if(!inherits(termobj, "terms"))
            stop(gettextf("'termobj' must be a object of class %s",
                          dQuote("terms")),
                 domain = NA)
	newformula <- reformulate(attr(termobj, "term.labels")[-dropx],
				  if (keep.response) termobj[[2L]] else NULL,
                                  attr(termobj, "intercept"))
        environment(newformula) <- environment(termobj)
	result <- terms(newformula, specials=names(attr(termobj, "specials")))

	# Edit the optional attributes

	response <- attr(termobj, "response")
	if (response && !keep.response)
	    # we have a response in termobj, but not in the result
	    dropOpt <- c(response, dropx + length(response))
	else
	    dropOpt <- dropx + max(response)

	if (!is.null(predvars <- attr(termobj, "predvars"))) {
	    # predvars is a language expression giving a list of
	    # values corresponding to terms in the model
            # so add 1 for the name "list"
	    attr(result, "predvars") <- predvars[-(dropOpt+1)]
	}
	if (!is.null(dataClasses <- attr(termobj, "dataClasses"))) {
	    # dataClasses is a character vector of
	    # values corresponding to terms in the model
	    attr(result, "dataClasses") <- dataClasses[-dropOpt]
	}
	result
    }
}


`[.terms` <- function (termobj, i)
{
    resp <- if (attr(termobj, "response")) termobj[[2L]] else NULL
    newformula <- attr(termobj, "term.labels")[i]
    if (length(newformula) == 0L) newformula <- "1"
    newformula <- reformulate(newformula, resp, attr(termobj, "intercept"))
    environment(newformula) <- environment(termobj)
    result <- terms(newformula, specials = names(attr(termobj, "specials")))

    # Edit the optional attributes

    addindex <- function(index, offset)
        # add a non-negative offset to a possibly negative index
    	ifelse(index < 0, index - offset,
    	       ifelse(index == 0, 0, index + offset))

    if (is.logical(i))
    	i <- which(rep_len(i, length.out = length(attr(termobj, "term.labels"))))

    response <- attr(termobj, "response")
    if (response)
	iOpt <- c(if (max(i) > 0) response, # inclusive indexing
	          addindex(i, max(response)))
    else
	iOpt <- i

    if (!is.null(predvars <- attr(termobj, "predvars")))
	attr(result, "predvars") <- predvars[c(if (max(iOpt) > 0) 1,
	                                     addindex(iOpt, 1))]

    if (!is.null(dataClasses <- attr(termobj, "dataClasses")))
	attr(result, "dataClasses") <- dataClasses[iOpt]

    result
}


## Arguments abb and neg.out are a legacy from S
## simplify=TRUE was the default in R < 1.7.0
terms.formula <- function(x, specials = NULL, abb = NULL, data = NULL,
			  neg.out = TRUE, keep.order = FALSE,
                          simplify = FALSE, ..., allowDotAsName = FALSE)
{
    fixFormulaObject <- function(object) {
        Terms <- terms(object)
	tmp <- attr(Terms, "term.labels")
        ## fix up terms involving | : PR#8462
        ind <- grep("|", tmp, fixed = TRUE)
        if(length(ind)) tmp[ind] <- paste("(", tmp[ind], ")")
        ## need to add back any offsets
        if(length(ind <- attr(Terms, "offset"))) {
            ## can't look at rownames of factors, as not there for y ~ offset(x)
            tmp2 <- as.character(attr(Terms, "variables"))[-1L]
            tmp <- c(tmp, tmp2[ind])
        }
	rhs <- if(length(tmp)) paste(tmp, collapse = " + ") else "1"
	if(!attr(terms(object), "intercept")) rhs <- paste(rhs, "- 1")
        if(length(form <- formula(object)) > 2L) {
            res <- formula(paste("lhs ~", rhs))
            res[[2L]] <- form[[2L]]
            res
        } else formula(paste("~", rhs))
    }

    if (!is.null(data) && !is.environment(data) && !is.data.frame(data))
	data <- as.data.frame(data, optional = TRUE)
    terms <-
        .External(C_termsform, x, specials, data, keep.order, allowDotAsName)
    if (simplify) {
        a <- attributes(terms)
        terms <- fixFormulaObject(terms)
        attributes(terms) <- a
    }
    environment(terms) <- environment(x)
    if(!inherits(terms, "formula"))
        class(terms) <- c(oldClass(terms), "formula")
    terms
}

coef <- function(object, ...) UseMethod("coef")
coef.default <- function(object, ...) object$coefficients
coefficients <- coef

residuals <- function(object, ...) UseMethod("residuals")
residuals.default <- function(object, ...)
    naresid(object$na.action, object$residuals)
resid <- residuals

deviance <- function(object, ...) UseMethod("deviance")
deviance.default <- function(object, ...) object$deviance

fitted <- function(object, ...) UseMethod("fitted")
## we really do need partial matching here
fitted.default <- function(object, ...)
{
    xx <- if("fitted.values" %in% names(object))
        object$fitted.values else object$fitted
    napredict(object$na.action, xx)
}
fitted.values <- fitted

anova <- function(object, ...)UseMethod("anova")

effects <- function(object, ...)UseMethod("effects")

weights <- function(object, ...)UseMethod("weights")
## used for class "lm", e.g. in drop1.
weights.default <- function(object, ...)
{
    wts <-  object$weights
    if (is.null(wts)) wts else napredict(object$na.action, wts)
}

df.residual <- function(object, ...)UseMethod("df.residual")
df.residual.default <- function(object, ...) object$df.residual

variable.names <- function(object, ...) UseMethod("variable.names")
variable.names.default <- function(object, ...) colnames(object)

case.names <- function(object, ...) UseMethod("case.names")
case.names.default <- function(object, ...) rownames(object)

simulate <- function(object, nsim = 1, seed = NULL, ...) UseMethod("simulate")

offset <- function(object) object
## ?

.checkMFClasses <- function(cl, m, ordNotOK = FALSE)
{
    ## when called from predict.nls, vars not match.
    new <- vapply(m, .MFclass, "")
    new <- new[names(new) %in% names(cl)]
     if(length(new) == 0L) return()
    old <- cl[names(new)]
    if(!ordNotOK) {
        old[old == "ordered"] <- "factor"
        new[new == "ordered"] <- "factor"
    }
    ## ordered is OK as a substitute for factor, but not v.v.
    new[new == "ordered" & old == "factor"] <- "factor"
    ## factor is OK as a substitute for character
    ## This probably means the original character got auto-converted to
    ## factor, setting xlevels and causing the conversion of the new
    new[new == "factor" & old == "character"] <- "character"
    if(!identical(old, new)) {
        wrong <- old != new
        if(sum(wrong) == 1)
            stop(gettextf(
    "variable '%s' was fitted with type \"%s\" but type \"%s\" was supplied",
                          names(old)[wrong], old[wrong], new[wrong]),
                 call. = FALSE, domain = NA)
        else
            stop(gettextf(
    "variables %s were specified with different types from the fit",
                 paste(sQuote(names(old)[wrong]), collapse=", ")),
                 call. = FALSE, domain = NA)
    }
}

.MFclass <- function(x)
{
    ## the idea is to identify the relevant classes that model.matrix
    ## will handle differently
    ## logical, factor, ordered vs numeric, and other for future proofing
    if(is.logical(x)) return("logical")
    if(is.ordered(x)) return("ordered")
    if(is.factor(x)) return("factor")
    ## Character vectors may be auto-converted to factors, but keep them separate for now
    if(is.character(x)) return("character")
    if(is.matrix(x) && is.numeric(x))
        return(paste("nmatrix", ncol(x), sep="."))
    ## this is unclear.  Prior to 2.6.0 we assumed numeric with attributes
    ## meant something, but at least for now model.matrix does not
    ## treat it differently.
##    if(is.vector(x) && is.numeric(x)) return("numeric")
    if(is.numeric(x)) return("numeric")
    return("other")
}

model.frame <- function(formula, ...) UseMethod("model.frame")
model.frame.default <-
    function(formula, data = NULL, subset = NULL, na.action = na.fail,
	     drop.unused.levels = FALSE, xlev = NULL,...)
{
    ## first off, establish if we were passed a data frame 'newdata'
    ## and note the number of rows.
    possible_newdata <-
        !missing(data) && is.data.frame(data) &&
        identical(substitute(data), quote(newdata)) &&
        (nr <- nrow(data)) > 0

    ## were we passed just a fitted model object?
    ## the fit might have a saved model object
    if(!missing(formula) && nargs() == 1 && is.list(formula)
       && !is.null(m <- formula$model)) return(m)
    ## if not use the saved call (if there is one).
    if(!missing(formula) && nargs() == 1 && is.list(formula)
       && all(c("terms", "call") %in% names(formula))) {
        fcall <- formula$call
        m <- match(c("formula", "data", "subset", "weights", "na.action"),
                   names(fcall), 0)
        fcall <- fcall[c(1, m)]
        ## need stats:: for non-standard evaluation
        fcall[[1L]] <- quote(stats::model.frame)
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
        return(eval(fcall, env)) # 2-arg form as env is an environment
    }
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") &&
	   length(attr(data, "terms")))
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if(missing(na.action)) {
	if(!is.null(naa <- attr(data, "na.action")) & mode(naa)!="numeric")
	    na.action <- naa
	else if(!is.null(naa <- getOption("na.action")))
	    na.action <- naa
    }
    if(missing(data))
	data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data)
             && !is.null(attr(data, "class")))
        data <- as.data.frame(data)
    else if (is.array(data))
        stop("'data' must be a data.frame, not a matrix or an array")
    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L) #attr(data, "row.names")
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars")
    if(is.null(predvars)) predvars <- vars
    ## Some people have generated longer variable names
    ## https://stat.ethz.ch/pipermail/r-devel/2010-October/058756.html
    varnames <- sapply(vars, function(x) paste(deparse(x,width.cutoff=500),
                                               collapse=' '))[-1L]
    variables <- eval(predvars, data, env)
    resp <- attr(formula, "response")
    if(is.null(rownames) && resp > 0L) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
        rownames <- if(is.matrix(lhs)) rownames(lhs) else names(lhs)
    }
    if(possible_newdata && length(variables)) {
        ## need to do this before subsetting and na.action
        nr2 <- max(sapply(variables, NROW))
        if(nr2 != nr)
            warning(sprintf(paste0(ngettext(nr,
                                            "'newdata' had %d row",
                                            "'newdata' had %d rows"),
                                   " ",
                                  ngettext(nr2,
                                           "but variable found had %d row",
                                           "but variables found have %d rows")),
                            nr, nr2),
                    call. = FALSE, domain = NA)
    }
    if(is.null(attr(formula, "predvars"))) {
        for (i in seq_along(varnames))
            predvars[[i+1L]] <- makepredictcall(variables[[i]], vars[[i+1L]])
        attr(formula, "predvars") <- predvars
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    subset <- eval(substitute(subset), data, env)
    data <- .External2(C_modelframe, formula, rownames, variables, varnames,
                       extras, extranames, subset, na.action)
    ## fix up the levels
    if(length(xlev)) {
	for(nm in names(xlev))
	    if(!is.null(xl <- xlev[[nm]])) {
		xi <- data[[nm]]
                if(is.character(xi))
                    xi <- as.factor(xi)
		if(!is.factor(xi) || is.null(nxl <- levels(xi)))
		    warning(gettextf("variable '%s' is not a factor", nm),
                            domain = NA)
		else {
		    ctr <- attr(xi, "contrasts")
		    xi <- xi[, drop = TRUE] # drop unused levels
                    nxl <- levels(xi)
		    if(any(m <- is.na(match(nxl, xl))))
                        stop(sprintf(ngettext(length(m),
                                              "factor %s has new level %s",
                                              "factor %s has new levels %s"),
                                     nm, paste(nxl[m], collapse=", ")),
                             domain = NA)
		    data[[nm]] <- factor(xi, levels=xl, exclude=NULL)
		    if (!identical(attr(data[[nm]], "contrasts"), ctr))
		    	warning(gettext(sprintf("contrasts dropped from factor %s", nm), domain = NA),
		    	        call. = FALSE)
		}
	    }
    } else if(drop.unused.levels) {
	for(nm in names(data)) {
	    x <- data[[nm]]
	    if(is.factor(x) &&
	       length(unique(x[!is.na(x)])) < length(levels(x))) {
	        ctr <- attr(x, "contrasts")
		data[[nm]] <- x[, drop = TRUE]
		if (!identical(attr(data[[nm]], "contrasts"), ctr))
		    warning(gettext(sprintf("contrasts dropped from factor %s due to missing levels", nm), domain = NA),
		            call. = FALSE)
	    }
	}
    }
    attr(formula, "dataClasses") <- vapply(data, .MFclass, "")
    attr(data, "terms") <- formula
    data
}

## we don't assume weights are numeric or a vector, leaving this to the
## calling application
model.weights <- function(x) x$"(weights)"

## we do check that offsets are numeric.
model.offset <- function(x) {
    offsets <- attr(attr(x, "terms"),"offset")
    if(length(offsets)) {
	ans <- x$"(offset)"
        if (is.null(ans)) ans <- 0
	for(i in offsets) ans <- ans+x[[i]]
	ans
    }
    else ans <- x$"(offset)"
    if(!is.null(ans) && !is.numeric(ans)) stop("'offset' must be numeric")
    ans
}

model.matrix <- function(object, ...) UseMethod("model.matrix")

model.matrix.default <- function(object, data = environment(object),
				 contrasts.arg = NULL, xlev = NULL, ...)
{
    t <- if(missing(data)) terms(object) else terms(object, data=data)
    if (is.null(attr(data, "terms")))
	data <- model.frame(object, data, xlev=xlev)
    else {
        ## need complete deparse, PR#15377
        deparse2 <- function(x)
            paste(deparse(x, width.cutoff = 500L), collapse = " ")
	reorder <- match(sapply(attr(t, "variables"), deparse2)[-1L],
                         names(data))
	if (anyNA(reorder))
	    stop("model frame and formula mismatch in model.matrix()")
	if(!identical(reorder, seq_len(ncol(data))))
	    data <- data[,reorder, drop=FALSE]
    }
    int <- attr(t, "response")
    if(length(data)) {      # otherwise no rhs terms, so skip all this
        contr.funs <- as.character(getOption("contrasts"))
        namD <- names(data)
        ## turn any character columns into factors
        for(i in namD)
            if(is.character(data[[i]]))
                data[[i]] <- factor(data[[i]])
        isF <- vapply(data, function(x) is.factor(x) || is.logical(x), NA)
        isF[int] <- FALSE
        isOF <- vapply(data, is.ordered, NA)
        for(nn in namD[isF])            # drop response
            if(is.null(attr(data[[nn]], "contrasts")))
                contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
        ## it might be safer to have numerical contrasts:
        ##	  get(contr.funs[1 + isOF[nn]])(nlevels(data[[nn]]))
        if (!is.null(contrasts.arg) && is.list(contrasts.arg)) {
            if (is.null(namC <- names(contrasts.arg)))
                stop("invalid 'contrasts.arg' argument")
            for (nn in namC) {
                if (is.na(ni <- match(nn, namD)))
                    warning(gettextf("variable '%s' is absent, its contrast will be ignored", nn),
                            domain = NA)
                else {
                    ca <- contrasts.arg[[nn]]
                    if(is.matrix(ca)) contrasts(data[[ni]], ncol(ca)) <- ca
                    else contrasts(data[[ni]]) <- contrasts.arg[[nn]]
                }
            }
        }
    } else {               # internal model.matrix needs some variable
	isF <- FALSE
	data <- data.frame(x=rep(0, nrow(data)))
    }
    ans <- .External2(C_modelmatrix, t, data)
    cons <- if(any(isF))
	lapply(data[isF], attr, "contrasts") ## else NULL
    attr(ans, "contrasts") <- cons
    ans
}

model.response <- function (data, type = "any")
{
    if (attr(attr(data, "terms"), "response")) {
	if (is.list(data) | is.data.frame(data)) {
	    v <- data[[1L]]
	    if (type == "numeric" && is.factor(v)) {
		warning('using type = "numeric" with a factor response will be ignored')
	    } else if (type == "numeric" | type == "double")
		storage.mode(v) <- "double"
	    else if (type != "any") stop("invalid response type")
	    if (is.matrix(v) && ncol(v) == 1L) dim(v) <- NULL
	    rows <- attr(data, "row.names")
	    if (nrows <- length(rows)) {
		if (length(v) == nrows) names(v) <- rows
		else if (length(dd <- dim(v)) == 2L)
		    if (dd[1L] == nrows && !length((dn <- dimnames(v))[[1L]]))
			dimnames(v) <- list(rows, dn[[2L]])
	    }
	    return(v)
	} else stop("invalid 'data' argument")
    } else return(NULL)
}

model.extract <- function (frame, component)
{
    component <- as.character(substitute(component))
    rval <- switch(component,
		   response = model.response(frame),
		   offset = model.offset(frame),
                   frame[[paste0("(", component, ")")]]
                   )
    if(!is.null(rval)){
	if (length(rval) == nrow(frame))
	    names(rval) <- attr(frame, "row.names")
	else if (is.matrix(rval) && nrow(rval) == nrow(frame)) {
	    t1 <- dimnames(rval)
	    dimnames(rval) <- list(attr(frame, "row.names"), t1[[2L]])
	}
    }
    rval
}

preplot <- function(object, ...) UseMethod("preplot")
update <- function(object, ...) UseMethod("update")

is.empty.model <- function (x)
{
    tt <- terms(x)
    (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 0L)
}

makepredictcall <- function(var, call) UseMethod("makepredictcall")

makepredictcall.default  <- function(var, call)
{
    if(as.character(call)[1L] != "scale") return(call)
    if(!is.null(z <- attr(var, "scaled:center"))) call$center <- z
    if(!is.null(z <- attr(var, "scaled:scale"))) call$scale <- z
    call
}

.getXlevels <- function(Terms, m)
{
    deparse2 <- function(x)
        paste(deparse(x, width.cutoff = 500L), collapse = " ")
    xvars <- sapply(attr(Terms, "variables"), deparse2)[-1L]
    if((yvar <- attr(Terms, "response")) > 0) xvars <- xvars[-yvar]
    if(length(xvars)) {
        xlev <- lapply(m[xvars],
        	    function(x)
        	    	if(is.factor(x)) levels(x)
        	    	else if (is.character(x)) levels(as.factor(x))
        	    	else NULL)
        xlev[!vapply(xlev, is.null, NA)]
    } else NULL
}

get_all_vars <- function(formula, data = NULL, ...)
{
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") &&
	   length(attr(data, "terms")) )
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if(missing(data))
	data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data)
             && !is.null(attr(data, "class")))
        data <- as.data.frame(data)
    else if (is.array(data))
        stop("'data' must be a data.frame, not a matrix or an array")
    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L) #attr(data, "row.names")
    varnames <- all.vars(formula)
    inp <- parse(text = paste("list(", paste(varnames, collapse = ","), ")"),
                 keep.source = FALSE)
    variables <- eval(inp, data, env)
    if(is.null(rownames) && (resp <- attr(formula, "response")) > 0) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
        rownames <- if(is.matrix(lhs)) rownames(lhs) else names(lhs)
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    x <- setNames(as.data.frame(c(variables, extras), optional=TRUE),
		  c(varnames, extranames))
    if (!is.null(rownames))
	attr(x, "row.names") <- rownames # might be short form
    x
}
