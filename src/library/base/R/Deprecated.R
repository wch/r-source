###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
		  " is deprecated.\n",
		  if (!missing(new))
		  paste("Use", sQuote(new), "instead.\n"),
		  "See ?Deprecated.",
		  sep = ""),
            call. = FALSE)
}

## consider keeping one (commented) entry here, for easier additions
## <entry>
## Deprecated in 1.8.0
## when it is removed, remove also from stoplists in
## methods (base/R/objects.R) and tools/R/Utils.R
print.coefmat <-
    function(x, digits=max(3, getOption("digits") - 2),
             signif.stars = getOption("show.signif.stars"),
             dig.tst = max(1, min(5, digits - 1)),
             cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
             P.values = NULL,
             has.Pvalue = nc >= 4 && substr(colnames(x)[nc],1,3) == "Pr(",
             eps.Pvalue = .Machine$double.eps,
             na.print = "", ...)
{
    .Deprecated("printCoefmat")
    Call <- match.call(expand.dots = TRUE)
    if(missing(na.print)) Call$na.print <- ""
    Call[[1]] <- as.name("printCoefmat")
    eval.parent(Call)
}
## </entry>

## <entry>
## Deprecated in 1.8.0
codes <- function(x, ...) UseMethod("codes")

codes.factor <- function(x, ...)
{
    ## This is the S-plus semantics.
    ## The deeper meaning? Search me...
    .Deprecated("unclass")
    rank(levels(x))[x]
}

codes.ordered <- function(x, ...)
{
    .Deprecated("unclass")
    as.integer(x)
}

"codes<-" <- function(x, ..., value)
{
    .Deprecated()
    if ( length(value) == 1 )
	value <- rep.int(value, length(x))
    else if ( length(x) != length(value) )
	stop("Length mismatch in \"codes<-\"")
    ## S-plus again...
    if ( !is.ordered(x) ) value <- order(levels(x))[value]
    attributes(value) <- attributes(x)
    value
}
## </entry>

## <entry>
## Deprecated in 1.8.0: unused since 1.2.0
anovalist.lm <- function (object, ..., test = NULL)
{
    objects <- list(object, ...)
    responses <- as.character(lapply(objects,
				     function(x) as.character(x$terms[[2]])))
    sameresp <- responses == responses[1]
    if (!all(sameresp)) {
	objects <- objects[sameresp]
	warning("Models with response ", deparse(responses[!sameresp]),
                " removed because response differs from ", "model 1")
    }
    ## calculate the number of models
    nmodels <- length(objects)
    if (nmodels == 1)
	return(anova.lm(object))

    models <- as.character(lapply(objects, function(x) x$terms))

    ## extract statistics
    df.r <- unlist(lapply(objects, df.residual))
    ss.r <- unlist(lapply(objects, deviance))
    df <- c(NA, -diff(df.r))
    ss <- c(NA, -diff(ss.r))
    ms <- ss/df
    f <- p <- rep.int(NA, nmodels)
    for(i in 2:nmodels) {
	if(df[i] > 0) {
	    f[i] <- ms[i]/(ss.r[i]/df.r[i])
	    p[i] <- pf(f[i], df[i], df.r[i], lower.tail = FALSE)
	}
	else if(df[i] < 0) {
	    f[i] <- ms[i]/(ss.r[i-1]/df.r[i-1])
	    p[i] <- pf(f[i], -df[i], df.r[i-1], lower.tail = FALSE)
	}
	else { # df[i] == 0
	    ss[i] <- 0
	}
    }
    table <- data.frame(df.r,ss.r,df,ss,f,p)
    dimnames(table) <- list(1:nmodels, c("Res.Df", "Res.Sum Sq", "Df",
					 "Sum Sq", "F value", "Pr(>F)"))
    ## construct table and title
    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1:nmodels),": ",
		     models, sep="", collapse="\n")

    ## calculate test statistic if needed
    structure(table, heading = c(title, topnote),
	      class= c("anova", "data.frame"))# was "tabular"
}
## </entry>

## <entry>
## Deprecated in 1.8.0
## for back-compatibility only: POSIXt versions are used as from 1.3.0
"-.POSIXct" <- function(e1, e2)
{
    .Deprecated("-.POSIXt")
    if(!inherits(e1, "POSIXct"))
        stop("Can only subtract from POSIXct objects")
    if (nargs() == 1) stop("unary - is not defined for POSIXct objects")
    res<- NextMethod()
    if(inherits(e2, "POSIXct")) unclass(res) else res
}

"-.POSIXlt" <- function(e1, e2)
{
    .Deprecated("-.POSIXt")
    if (nargs() == 1)
        stop("unary - is not defined for dt objects")
    if(inherits(e1, "POSIXlt")) e1 <- as.POSIXct(e1)
    if(inherits(e2, "POSIXlt")) e2 <- as.POSIXct(e2)
    e1 - e2
}

Ops.POSIXct <- function(e1, e2)
{
    .Deprecated("Ops.POSIXt")
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for POSIXct objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(paste(.Generic, "not defined for POSIXct objects"))
    NextMethod(.Generic)
}

Ops.POSIXlt <- function(e1, e2)
{
    .Deprecated("Ops.POSIXt")
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for POSIXlt objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) stop(paste(.Generic, "not defined for POSIXlt objects"))
    e1 <- as.POSIXct(e1)
    e2 <- as.POSIXct(e2)
    NextMethod(.Generic)
}
## </entry>
