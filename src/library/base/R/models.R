formula <- function(x, ...) UseMethod("formula")
formula.default<-function (x)
{
	if (!is.null(x$formula))
		return(eval(x$formula))
	if (!is.null(x$call$formula))
		return(eval(x$call$formula))
	if (!is.null(x$terms))
		return(x$terms)
	switch(mode(x), NULL = structure(NULL, class = "formula"),
		character = formula(eval(parse(text = x)[[1]])),
		call = eval(x), stop("invalid formula"))
}
formula.formula <- function(x) x
formula.terms <- function(x) {
	attributes(x) <- list(class="formula")
	x
}
print.formula <- function(x) print.default(unclass(x))
"[.formula" <- function(x,i) {
	ans <- NextMethod("[")
	if(as.character(ans[[1]]) == "~")
		class(ans) <- "formula"
	ans
}

terms <- function(x, ...) UseMethod("terms")
terms.default <- function(x) x$terms
terms.terms <- function(x, ...) x
print.terms <- function(x) print.default(unclass(x))
delete.response <- function (termobj)
{
	intercept <- if (attr(termobj, "intercept")) "1" else "0"
	terms(reformulate(c(attr(termobj, "term.labels"), intercept), NULL),
	      specials = names(attr(termobj, "specials")))
}

reformulate <- function (termlabels, response=NULL)
{
	termtext <- paste(termlabels, collapse="+")
	if (is.null(response)){
		termtext <- paste("~", termtext, collapse="")
		eval(parse(text=termtext)[[1]])
	}
	else {
		termtext <- paste("response", "~", termtext, collapse="")
		termobj <- eval(parse(text=termtext)[[1]])
		termobj[[2]] <- response
		termobj
	}
}

drop.terms <-function(termobj, dropx=NULL, keep.response=FALSE)
{
 if (is.null(dropx))
	termobj
 else {
	newformula <- reformulate(attr(termobj, "term.labels")[-dropx],
				if (keep.response) termobj[[2]] else NULL)
	terms(newformula, specials=names(attr(termobj, "specials")))
 }
}

terms.formula <-
function (x, specials = NULL, abb = NULL, data = NULL, keep.order = FALSE)
{
	if(!is.null(data) && !is.environment(data) && !is.data.frame(data))
		data <- as.data.frame(data)
	new.specials <- unique(c(specials, "offset"))
	terms <-.Internal(terms.formula(x, new.specials, abb, data, keep.order))
	offsets <- attr(terms,"specials")$offset
	if(!is.null(offsets)) {
		names <- dimnames(attr(terms,"factors"))[[1]][offsets]
		offsets <- match(names, dimnames(attr(terms,"factors"))[[2]])
		offsets <- offsets[!is.na(offsets)]
		if(length(offsets) > 0) {
			attr(terms, "factors") <- attr(terms,"factors")[,-offsets, drop=FALSE]
			attr(terms, "term.labels") <- attr(terms, "term.labels")[-offsets]
			attr(terms, "order") <- attr(terms, "order")[-offsets]
			attr(terms, "offset") <- attr(terms,"specials")$offset
		}
	}
	attr(terms, "specials")$offset <- NULL
	terms
}

coef <- function(x, ...) UseMethod("coef")
coef.default <- function(x, ...) x$coefficients
coefficients <- coef

residuals <- function(x, ...) UseMethod("residuals")
resid <- residuals

deviance <- function(x, ...) UseMethod("deviance")

fitted <- function(x, ...) UseMethod("fitted")
fitted.default <- function(x) x$fitted
fitted.values <- fitted

anova <- function(x, ...)UseMethod("anova")

effects <- function(x, ...)UseMethod("effects")

weights <- function(x, ...)UseMethod("weights")

df.residual <- function(x, ...)UseMethod("df.residual")

variable.names <-function(obj, ...)UseMethod("variable.names")

case.names <-function(obj, ...)UseMethod("case.names")

offset <- function(x) x
## ?

na.action <- function(x, ...)UseMethod("na.action")
na.action.default <- function(x) attr(x, "na.action")

na.fail <- function(frame)
{
	ok <- complete.cases(frame)
	if(all(ok)) frame else stop("missing values in data frame");
}

na.omit <- function(frame)
{
	ok <- complete.cases(frame)
	if (all(ok))
		frame
	else frame[ok, ]
}

##-- used nowhere (0.62)
##- model.data.frame <- function(...) {
##-	cn <- as.character(substitute(list(...))[-1])
##-	rval<-data.frame(..., col.names=cn, as.is=TRUE)
##-	names(rval)<-cn
##-	rval
##- }

model.frame <- function(x, ...)	UseMethod("model.frame")

model.frame.default <-
function(formula, data = NULL, subset=NULL, na.action = na.fail, ...)
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
		if(!is.null(naa <- attr(data, "na.action")))
			na.action <- naa
		else if(!is.null(naa <- options("na.action")[[1]]))
			na.action <- naa
	}
	if(missing(data))
		data <- sys.frame(sys.parent())
	if(!inherits(formula, "terms"))
		formula <- terms(formula, data = data)
	subset<-eval(substitute(subset),data)
	.Internal(model.frame(formula, data, substitute(list(...)),
		subset, na.action))
}

model.weights <- function(x) x$"(weights)"
model.offset <- function(x) {
	offsets <- attr(attr(x, "terms"),"offset")
	if(length(offsets) > 0) {
		ans <- 0
		for(i in offsets) ans <- ans+x[[i]]
		ans
	}
	else NULL
}

model.matrix <- function(object, ...) UseMethod("model.matrix")
model.matrix.default <- function(formula, data, contrasts = NULL)
{
 t <- terms(formula)
 if (missing(data)) {
	vars <- attr(t, "variables")
	# comes out as list(x,y,z), make it data.frame(x,y,z)
	vars[[1]] <- as.name("data.frame")
	data <- eval(vars, sys.frame(sys.parent()))
 }
 contrastsL <- contrasts
 rm(contrasts)
 if (!is.null(contrastsL)) {
	namD <- names(data)
	if (!is.list(contrastsL))
		stop("invalid contrasts")
	if (is.null(namC <- names(contrastsL)))
		stop("invalid contrasts argument")
	for (nn in namC) {
		if (is.na(ni <- match(nn, namD)))
			warning(paste("Variable", nn,
				      "absent, contrast ignored"))
		else contrasts(data[[ni]]) <- contrastsL[[nn]]
	}
 }
 reorder <- match(as.character(attr(t,"variables"))[-1],names(data))
 if (any(is.na(reorder))) stop("invalid model frame in model.matrix()")
 data <- data[,reorder, drop=FALSE]
 .Internal(model.matrix(t, data))
}

model.response <- function (data, type = "any")
{
	if (attr(attr(data, "terms"), "response")) {
		if (is.list(data) | is.data.frame(data)) {
			v <- data[[1]]
			if (type == "numeric" | type == "double") {
				storage.mode(v) <- "double"
			}
			else if (type != "any")
				stop("invalid response type")
			if (is.matrix(v) && ncol(v) == 1)
				dim(v) <- NULL
			return(v)
		}
		else stop("invalid data argument")
	}
	else return(NULL)
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

update <- function(x, ...) UseMethod("update")

is.empty.model<-function (x)
{
	tt <- terms(x)
	(length(attr(tt, "factors")) == 0) & (attr(tt, "intercept")==0)
}
