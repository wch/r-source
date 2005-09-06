formula <- function(x, ...) UseMethod("formula")
formula.default <- function (x,env=parent.frame(), ...)
{
    if (!is.null(x$formula))		eval(x$formula)
    else if (!is.null(x$terms))		{z <- x$terms; oldClass(z) <- "formula"; z}
    else if (!is.null(x$call$formula))	eval(x$call$formula)
    else if (!is.null(attr(x, "formula"))) attr(x, "formula")
    else {form<-switch(mode(x),
		NULL = structure(NULL, class = "formula"),
		character = formula(eval(parse(text = x)[[1]])),
		call = eval(x), stop("invalid formula"))
        environment(form)<-env
        form
    }
}
formula.formula <- function(x, ...) x
formula.terms <- function(x, ...) {
    env<- environment(x)
    attributes(x) <- list(class="formula")
    environment(x) <- env
    x
}

formula.data.frame <- function (x, ...)
{
    nm <- sapply(names(x), as.name)
    lhs <- nm[1]
    if (length(nm) > 1) {
       rhs <- nm[-1]
    }
    else {
       rhs <- nm[1]
       lhs <- NULL
    }
    ff <- parse(text = paste(lhs, paste(rhs, collapse = "+"), sep = "~"))
    ff<-eval(ff)
    environment(ff)<-parent.frame()
    ff
}

print.formula <- function(x, ...) {
    attr(x, ".Environment") <- NULL
    print.default(unclass(x), ...)
}

"[.formula" <- function(x,i) {
    ans <- NextMethod("[")
    ## as.character gives a vector.
    if(as.character(ans[[1]])[1] == "~"){
	class(ans) <- "formula"
        environment(ans)<-environment(x)
    }
    ans
}

as.formula <- function(object,env=parent.frame()){
    if(inherits(object, "formula"))
           object
    else{
        rval<-formula(object,env=baseenv())
        if (is.null(environment(rval)) || !missing(env))
            environment(rval)<-env
        rval
    }
}

terms <- function(x, ...) UseMethod("terms")
terms.default <- function(x, ...) {
    v <- x$terms
    if(is.null(v))
        stop("no terms component")
    return(v)
}

terms.terms <- function(x, ...) x
print.terms <- function(x, ...) print.default(unclass(x))

## moved from base/R/labels.R
labels.terms <- function(object, ...) attr(object, "term.labels")

### do this `by hand' as previous approach was vulnerable to re-ordering.
delete.response <- function (termobj)
{
    a <- attributes(termobj)
    y <- a$response
    if(!is.null(y) && y) {
        termobj[[2]] <- NULL
        a$response <- 0
        a$variables <- a$variables[-(1+y)]
        a$predvars <- a$predvars[-(1+y)]
        if(length(a$factors))
            a$factors <- a$factors[-y, , drop = FALSE]
        if(length(a$offset))
            a$offset <- ifelse(a$offset > y, a$offset-1, a$offset)
        if(length(a$specials))
            for(i in 1:length(a$specials)) {
                b <- a$specials[[i]]
                a$specials[[i]] <- ifelse(b > y, b-1, b)
            }
        attributes(termobj) <- a
    }
    termobj
}

reformulate <- function (termlabels, response=NULL)
{
    has.resp <- !is.null(response)
    termtext <- paste(if(has.resp)"response", "~",
		      paste(termlabels, collapse = "+"),
		      collapse = "")
    rval <- eval(parse(text = termtext)[[1]])
    if(has.resp) rval[[2]] <-
        if(is.character(response)) as.symbol(response) else response
    ## response can be a symbol or call as  Surv(ftime, case)
    environment(rval) <- parent.frame()
    rval
}

drop.terms <- function(termobj, dropx=NULL, keep.response = FALSE)
{
    if (is.null(dropx))
	termobj
    else {
	newformula <- reformulate(attr(termobj, "term.labels")[-dropx],
				  if (keep.response) termobj[[2]] else NULL)
        environment(newformula)<-environment(termobj)
	terms(newformula, specials=names(attr(termobj, "specials")))
    }
}


"[.terms" <-function (termobj, i) {
        resp <- if (attr(termobj, "response"))
                termobj[[2]]
        else NULL
        newformula <- attr(termobj, "term.labels")[i]
        if (length(newformula) == 0)
                newformula <- 1
        newformula <- reformulate(newformula, resp)
        environment(newformula)<-environment(termobj)
        terms(newformula, specials = names(attr(termobj, "specials")))

}


terms.formula <- function(x, specials = NULL, abb = NULL, data = NULL,
			  neg.out = TRUE, keep.order = FALSE,
                          simplify = FALSE, ..., allowDotAsName = FALSE)
{
    fixFormulaObject <- function(object) {
        Terms <- terms(object)
	tmp <- attr(Terms, "term.labels")
        ## need to add back any offsets
        if(length(ind <- attr(Terms, "offset"))) {
            ## can't look at rownames of factors, as not there y ~ offset(x)
            tmp2 <- as.character(attr(Terms, "variables"))[-1]
            tmp <- c(tmp, tmp2[ind])
        }
	form <- formula(object)
	lhs <- if(length(form) == 2) NULL else paste(deparse(form[[2]]),collapse="")
	rhs <- if(length(tmp)) paste(tmp, collapse = " + ") else "1"
	if(!attr(terms(object), "intercept")) rhs <- paste(rhs, "- 1")
	formula(paste(lhs, "~", rhs))
    }

    if (!is.null(data) && !is.environment(data) && !is.data.frame(data))
	data <- as.data.frame(data, optional=TRUE)
    terms <- .Internal(terms.formula(x, specials, data, keep.order,
                                     allowDotAsName))
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
fitted.default <- function(object, ...)
    napredict(object$na.action, object$fitted)
fitted.values <- fitted

anova <- function(object, ...)UseMethod("anova")

effects <- function(object, ...)UseMethod("effects")

weights <- function(object, ...)UseMethod("weights")

df.residual <- function(object, ...)UseMethod("df.residual")
df.residual.default <- function(object, ...) object$df.residual

variable.names <- function(object, ...) UseMethod("variable.names")
variable.names.default <- function(object, ...) colnames(object)

case.names <- function(object, ...) UseMethod("case.names")
case.names.default <- function(object, ...) rownames(object)

simulate <-
    function(object, nsim = 1,
             seed = as.integer(runif(1, 0, .Machine$integer.max)),
             ...) UseMethod("simulate")

offset <- function(object) object
## ?

.checkMFClasses <- function(cl, m, ordNotOK = FALSE)
{
    new <- sapply(m, .MFclass)
    if(length(new) == 0) return()
    old <- cl[names(new)]
    if(!ordNotOK) {
        old[old == "ordered"] <- "factor"
        new[new == "ordered"] <- "factor"
    }
    ## ordered is OK as a substitute for factor, but not v.v.
    new[new == "ordered" && old == "factor"] <- "factor"
    if(!identical(old, new)) {
        wrong <- old != new
        if(sum(wrong) == 1)
            stop(gettextf(
    "variable '%s' was fitted with class \"%s\" but class \"%s\" was supplied",
                          names(old)[wrong], old[wrong], new[wrong]),
                 call. = FALSE, domain = NA)
        else
            stop(gettextf(
    "variables %s were specified with different classes from the fit",
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
    if(is.factor(x))  return("factor")
    if(is.matrix(x) && is.numeric(x))
        return(paste("nmatrix", ncol(x), sep="."))
    if(is.vector(x) && is.numeric(x)) return("numeric")
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
    identical(deparse(substitute(data)), "newdata") &&
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
        fcall[[1]] <- as.name("model.frame")
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
        return(eval(fcall, env, parent.frame()))
    }
    if(missing(formula)) {
	if(!missing(data) && inherits(data, "data.frame") &&
	   length(attr(data, "terms")) > 0)
	    return(data)
	formula <- as.formula(data)
    }
    else if(missing(data) && inherits(formula, "data.frame")) {
	if(length(attr(formula, "terms")))
	    return(formula)
	data <- formula
	formula <- as.formula(data)
    }
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
    rownames <- attr(data, "row.names")
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars")
    if(is.null(predvars)) predvars <- vars
    varnames <- sapply(vars, deparse, width.cutoff=500)[-1]
    variables <- eval(predvars, data, env)
    if(is.null(rownames) && (resp <- attr(formula, "response")) > 0) {
        ## see if we can get rownames from the response
        lhs <- variables[[resp]]
        rownames <- if(is.matrix(lhs)) rownames(lhs) else names(lhs)
    }
    if(possible_newdata && length(variables)) {
        ## need to do this before subsetting and na.action
        nr2 <- max(sapply(variables, NROW))
        if(nr2 != nr)
            warning(gettextf(
                   "'newdata' had %d rows but variable(s) found have %d rows",
                             nr, nr2), call.=FALSE)
    }
    if(is.null(attr(formula, "predvars"))) {
        for (i in seq(along = varnames))
            predvars[[i+1]] <- makepredictcall(variables[[i]], vars[[i+1]])
        attr(formula, "predvars") <- predvars
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1])
    extras <- eval(extras, data, env)
    subset <- eval(substitute(subset), data, env)
    data <- .Internal(model.frame(formula, rownames, variables, varnames,
				  extras, extranames, subset, na.action))
    ## fix up the levels
    if(length(xlev) > 0) {
	for(nm in names(xlev))
	    if(!is.null(xl <- xlev[[nm]])) {
		xi <- data[[nm]]
		if(is.null(nxl <- levels(xi)))
		    warning(gettextf("variable '%s' is not a factor", nm),
                            domain = NA)
		else {
		    xi <- xi[, drop = TRUE] # drop unused levels
                    nxl <- levels(xi)
		    if(any(m <- is.na(match(nxl, xl))))
			stop(gettextf("factor '%s' has new level(s) %s",
                                      nm, paste(nxl[m], collapse=", ")),
                             domain = NA)
		    data[[nm]] <- factor(xi, levels=xl)
		}
	    }
    } else if(drop.unused.levels) {
	for(nm in names(data)) {
	    x <- data[[nm]]
	    if(is.factor(x) &&
	       length(unique(x)) < length(levels(x)))
		data[[nm]] <- data[[nm]][, drop = TRUE]
	}
    }
    attr(formula, "dataClasses") <- sapply(data, .MFclass)
    attr(data, "terms") <- formula
    data
}

model.weights <- function(x) x$"(weights)"
model.offset <- function(x) {
    offsets <- attr(attr(x, "terms"),"offset")
    if(length(offsets) > 0) {
	ans <- x$"(offset)"
        if (is.null(ans))
	   ans <- 0
	for(i in offsets) ans <- ans+x[[i]]
	ans
    }
    else x$"(offset)"
}

model.matrix <- function(object, ...) UseMethod("model.matrix")
model.matrix.default <- function(object, data = environment(object),
				 contrasts.arg = NULL, xlev = NULL, ...)
{
    t <- if(missing(data)) terms(object) else terms(object, data=data)
    if (is.null(attr(data, "terms")))
	data <- model.frame(object, data, xlev=xlev)
    else {
	reorder <- match(sapply(attr(t,"variables"),deparse,
                                width.cutoff=500)[-1],
                         names(data))
	if (any(is.na(reorder)))
	    stop("model frame and formula mismatch in model.matrix()")
	data <- data[,reorder, drop=FALSE]
    }
    int <- attr(t, "response")
    if(length(data)) { # otherwise no rhs terms, so skip all this
        contr.funs <- as.character(getOption("contrasts"))
        isF <- sapply(data, function(x) is.factor(x) || is.logical(x) )
        isF[int] <- FALSE
        isOF <- sapply(data, is.ordered)
        namD <- names(data)
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
    } else { # internal model.matrix needs some variable
        isF <-  FALSE
        data <- list(x=rep(0, nrow(data)))
    }
    ans <- .Internal(model.matrix(t, data))
    cons <- if(any(isF))
	lapply(data[isF], function(x) attr(x,  "contrasts"))
    else NULL
    attr(ans, "contrasts") <- cons
    ans
}

model.response <- function (data, type = "any")
{
    if (attr(attr(data, "terms"), "response")) {
	if (is.list(data) | is.data.frame(data)) {
	    v <- data[[1]]
	    if (type == "numeric" | type == "double") storage.mode(v) <- "double"
	    else if (type != "any") stop("invalid response type")
	    if (is.matrix(v) && ncol(v) == 1) dim(v) <- NULL
	    rows <- attr(data, "row.names")
	    if (nrows <- length(rows)) {
		if (length(v) == nrows) names(v) <- rows
		else if (length(dd <- dim(v)) == 2)
		    if (dd[1] == nrows && !length((dn <- dimnames(v))[[1]]))
			dimnames(v) <- list(rows, dn[[2]])
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
                   frame[[paste("(", component, ")", sep = "")]]
                   )
    if(!is.null(rval)){
	if (length(rval) == nrow(frame))
	    names(rval) <- attr(frame, "row.names")
	else if (is.matrix(rval) && nrow(rval) == nrow(frame)) {
	    t1 <- dimnames(rval)
	    dimnames(rval) <- list(attr(frame, "row.names"), t1[[2]])
	}
    }
    return(rval)
}

preplot <- function(object, ...) UseMethod("preplot")
update <- function(object, ...) UseMethod("update")

is.empty.model <- function (x)
{
    tt <- terms(x)
    (length(attr(tt, "factors")) == 0) & (attr(tt, "intercept")==0)
}

makepredictcall <- function(var, call) UseMethod("makepredictcall")

makepredictcall.default  <- function(var, call)
{
    if(as.character(call)[1] != "scale") return(call)
    if(!is.null(z <- attr(var, "scaled:center"))) call$center <- z
    if(!is.null(z <- attr(var, "scaled:scale"))) call$scale <- z
    call
}

.getXlevels <- function(Terms, m)
{
    xvars <- sapply(attr(Terms, "variables"),deparse,width.cutoff=500)[-1]
    if((yvar <- attr(Terms, "response")) > 0) xvars <- xvars[-yvar]
    if(length(xvars) > 0) {
        xlev <- lapply(m[xvars], levels)
        xlev[!sapply(xlev, is.null)]
    } else NULL
}
