formula <- function(x, ...) UseMethod("formula")
formula.default <- function (x,env=parent.frame(), ...)
{
    if (!is.null(x$formula))		eval(x$formula)
    else if (!is.null(x$call$formula))	eval(x$call$formula)
    else if (!is.null(x$terms))		x$terms
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
    if(as.character(ans[[1]]) == "~"){
	class(ans) <- "formula"
        environment(ans)<-environment(x)
    }
    ans
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
delete.response <- function (termobj)
{
    f <- formula(termobj)
    if (length(f) == 3) f[[2]] <- NULL
    tt <- terms(f, specials = names(attr(termobj, "specials")))
    attr(tt, "intercept") <- attr(termobj, "intercept")
    if (length(formula(termobj)) == 3)
        attr(tt, "predvars") <- attr(termobj, "predvars")[-2]
    tt
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

terms.formula <- function(x, specials = NULL, abb = NULL, data = NULL,
			  neg.out = TRUE, keep.order = FALSE, ...)
{
    fixFormulaObject <- function(object) {
	tmp <- attr(terms(object), "term.labels")
	form <- formula(object)
	lhs <- if(length(form) == 2) NULL else paste(deparse(form[[2]]),collapse="")
	rhs <- if(length(tmp)) paste(tmp, collapse = " + ") else "1"
	if(!attr(terms(object), "intercept")) rhs <- paste(rhs, "- 1")
	ff <- formula(paste(lhs, "~", rhs))
        environment(ff) <- environment(form)
        ff
    }
    if (!is.null(data) && !is.environment(data) && !is.data.frame(data))
	data <- as.data.frame(data)
    new.specials <- unique(c(specials, "offset"))
    tmp <- .Internal(terms.formula(x, new.specials, abb, data, keep.order))
    ## need to fix up . in formulae in R
    terms <- fixFormulaObject(tmp)
    attributes(terms) <- attributes(tmp)
    environment(terms) <- environment(x)
    offsets <- attr(terms, "specials")$offset
    if (!is.null(offsets)) {
	names <- dimnames(attr(terms, "factors"))[[1]][offsets]
	offsets <- match(names, dimnames(attr(terms, "factors"))[[2]])
	offsets <- offsets[!is.na(offsets)]
	if (length(offsets) > 0) {
	    attr(terms, "factors") <- attr(terms, "factors")[, -offsets, drop = FALSE]
	    attr(terms, "term.labels") <- attr(terms, "term.labels")[-offsets]
	    attr(terms, "order") <- attr(terms, "order")[-offsets]
	    attr(terms, "offset") <- attr(terms, "specials")$offset
	}
    }
    attr(terms, "specials")$offset <- NULL
    if( !inherits(terms, "formula") )
        class(terms) <- c(class(terms), "formula")
    terms
}

coef <- function(object, ...) UseMethod("coef")
coef.default <- function(object, ...) object$coefficients
coefficients <- coef

residuals <- function(object, ...) UseMethod("residuals")
residuals.default <- function(object, ...)
{
    if(is.null(object$na.action)) object$residuals
    else naresid(object$na.action, object$residuals)
}
resid <- residuals

deviance <- function(object, ...) UseMethod("deviance")
deviance.default <- function(object, ...) object$deviance

fitted <- function(object, ...) UseMethod("fitted")
fitted.default <- function(object, ...)
{
    if(is.null(object$na.action)) object$fitted
    else napredict(object$na.action, object$fitted)
}
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

offset <- function(object) object
## ?


model.frame <- function(formula, ...) UseMethod("model.frame")
model.frame.default <-
    function(formula, data = NULL, subset=NULL, na.action = na.fail,
	     drop.unused.levels = FALSE, xlev = NULL,...)
{
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
        stop("`data' must be a data.frame, not a matrix or  array")
    env <- environment(formula)
    if(!inherits(formula, "terms"))
	formula <- terms(formula, data = data)
    rownames <- attr(data, "row.names")
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars")
    if(is.null(predvars)) predvars <- vars
    varnames <- as.character(vars[-1])
    variables <- eval(predvars, data, env)
    if(is.null(attr(formula, "predvars"))) {
        for (i in seq(along = varnames))
            predvars[[i+1]] <- makepredictcall(variables[[i]], vars[[i+1]])
        attr(formula, "predvars") <- predvars
    }
    extranames <- names(substitute(list(...))[-1])
    extras <- substitute(list(...))
    extras <- eval(extras, data, env)
    ##if(length(extras)) { # remove NULL args
    ##    keep <- !sapply(extras, is.null)
    ##    extras <- extras[keep]
    ##    extranames <- extranames[keep]
    ##}
    subset <- eval(substitute(subset), data, env)
    data <- .Internal(model.frame(formula, rownames, variables, varnames,
				  extras, extranames, subset, na.action))
    ## fix up the levels
    if(length(xlev) > 0) {
	for(nm in names(xlev))
	    if(!is.null(xl <- xlev[[nm]])) {
		xi <- data[[nm]]
		if(is.null(nxl <- levels(xi)))
		    warning(paste("variable", nm, "is not a factor"))
		else {
		    xi <- xi[, drop= TRUE] # drop unused levels
		    if(any(m <- is.na(match(nxl, xl))))
			stop(paste("factor", nm, "has new level(s)", nxl[m]))
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
    t <- terms(object)
    if (is.null(attr(data, "terms")))
	data <- model.frame(object, data, xlev=xlev)
    else {
	reorder <- match(as.character(attr(t,"variables"))[-1],names(data))
	if (any(is.na(reorder)))
	    stop("model frame and formula mismatch in model.matrix()")
	data <- data[,reorder, drop=FALSE]
    }
    int <- attr(t, "response")
    if(length(data)) { # no rhs terms, so skip all this
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
                stop("invalid contrasts argument")
            for (nn in namC) {
                if (is.na(ni <- match(nn, namD)))
                    warning(paste("Variable", nn, "absent, contrast ignored"))
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
	} else stop("invalid data argument")
    } else return(NULL)
}

model.extract <- function (frame, component)
{
    component <- as.character(substitute(component))
    rval <- switch(component,
		   response = model.response(frame),
		   offset = model.offset(frame), weights = frame$"(weights)",
		   start = frame$"(start)")
    if (is.null(rval)) {
	name <- paste("frame$\"(", component, ")\"", sep = "")
	rval <- eval(parse(text = name)[1])
    }
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
