### $Id: selfStart.R,v 1.1 2003/12/11 07:16:02 ripley Exp $
###
###            self-starting nonlinear regression models
###
### Copyright 1997,1999 Jose C. Pinheiro <jcp$research.bell-labs.com>,
###                     Douglas M. Bates <bates$stat.wisc.edu>
###
### This file is part of the nls library for R and related languages
### and was taken from the nlme library for S.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

####* Constructors

selfStart <-
    function(model, initial, parameters, template) UseMethod("selfStart")

selfStart.default <-
  function(model, initial, parameters, template)
{
    value <- structure(as.function(model), initial = as.function(initial),
                       pnames = if(!missing(parameters))parameters)
    class(value) <- "selfStart"
    value
}

selfStart.formula <-
    function(model, initial, parameters, template = NULL)
{
    if (is.null(template)) {		# create a template if not given
        nm <- all.vars(model)
        if (any(msng <- is.na(match(parameters, nm)))) {
            stop(paste("Parameter(s)", parameters[msng],
                       "do not occur in the model formula"))
        }
        template <- function() {}
        argNams <- c( nm[ is.na( match(nm, parameters) ) ], parameters )
        args <- rep( alist( a = , b = 3 )[-2], length( argNams ) )
        names(args) <- argNams
        formals(template) <- args
    }
    value <- structure(deriv(model, parameters, template),
                       initial = as.function(initial),
                       pnames = parameters)
    class(value) <- "selfStart"
    value
}

###*# Methods


##*## Generics and methods specific to selfStart models

getInitial <-
    ## Create initial values for object from data
    function(object, data, ...) UseMethod("getInitial")

getInitial.formula <-
    function(object, data, ...)
{
    if(!is.null(attr(data, "parameters"))) {
        return(attr(data, "parameters"))
    }
    #obj <- object                       # kluge to create a copy inside this
    #object[[1]] <- as.name("~")	 # function. match.call() is misbehaving
    switch (length(object),
            stop("argument 'object' has an impossible length"),
        {				# one-sided formula
	    func <- get(as.character(object[[2]][[1]]))
	    getInitial(func, data,
		       mCall = as.list(match.call(func, call = object[[2]])),
                       ...)
        },
        {				# two-sided formula
	    func <- get(as.character(object[[3]][[1]]))
	    getInitial(func, data,
		       mCall = as.list(match.call(func, call = object[[3]])),
		       LHS = object[[2]], ...)
        })
}

getInitial.selfStart <-
    function(object, data, mCall, LHS = NULL, ...)
{
    (attr(object, "initial"))(mCall = mCall, data = data, LHS = LHS)
}

getInitial.default <-
    function(object, data, mCall, LHS = NULL, ...)
{
    if (is.function(object) && !is.null(attr(object, "initial"))) {
        stop(paste("old-style self-starting model functions\n",
                   "are no longer supported.\n",
                   "New selfStart functions are available.\n",
                   "Use\n",
                   "  SSfpl instead of fpl,\n",
                   "  SSfol instead of first.order.log,\n",
                   "  SSbiexp instead of biexp,\n",
                   "  SSlogis instead of logistic.\n",
                   "If writing your own selfStart model, see\n",
                   "  \"help(selfStart)\"\n",
                   "for the new form of the \"initial\" attribute.", sep="" ))
    }
    stop(paste("No getInitial method found for", data.class(object), "objects"))
}

sortedXyData <-
    ## Constructor of the sortedXyData class
    function(x, y, data) UseMethod("sortedXyData")

sortedXyData.default <-
    function(x, y, data)
{
    ## works for x and y either numeric or language elements
    ## that can be evaluated in data
    #data <- as.data.frame(data)
    if (is.language(x) || ((length(x) == 1) && is.character(x))) {
        x <- eval(asOneSidedFormula(x)[[2]], data)
    }
    x <- as.numeric(x)
    if (is.language(y) || ((length(y) == 1) && is.character(y))) {
        y <- eval(asOneSidedFormula(y)[[2]], data)
    }
    y <- as.numeric(y)
    y.avg <- tapply(y, x, mean, na.rm = TRUE)
    xvals <- as.numeric(names(y.avg))
    ord <- order(xvals)
    value <- na.omit(data.frame(x = xvals[ord], y = as.vector(y.avg[ord])))
    class(value) <- c("sortedXyData", "data.frame")
    value
}

NLSstClosestX <-
    ## find the x value in the xy frame whose y value is closest to yval
    function(xy, yval) UseMethod("NLSstClosestX")

NLSstClosestX.sortedXyData <-
    ## find the x value in the xy frame whose y value is closest to yval
    ## uses linear interpolation in case the desired x falls between
    ## two data points in the xy frame
    function(xy, yval)
{
    deviations <- xy$y - yval
    if (any(deviations <= 0)) {
        dev1 <- max(deviations[deviations <= 0])
        lim1 <- xy$x[match(dev1, deviations)]
        if (all(deviations <= 0)) {
            return(lim1)
        }
    }
    if (any(deviations >= 0)) {
        dev2 <- min(deviations[deviations >= 0])
        lim2 <- xy$x[match(dev2, deviations)]
        if (all(deviations >= 0)) {
            return(lim2)
        }
    }
    dev1 <- abs(dev1)
    dev2 <- abs(dev2)
    lim1 + (lim2 - lim1) * dev1/(dev1 + dev2)
}

NLSstRtAsymptote <-
    ## Find a reasonable value for the right asymptote.
    function(xy) UseMethod("NLSstRtAsymptote")

NLSstRtAsymptote.sortedXyData <-
    function(xy)
{
    ## Is the last response value closest to the minimum or to
    ## the maximum?
    in.range <- range(xy$y)
    last.dif <- abs(in.range - xy$y[nrow(xy)])
    ## Estimate the asymptote as the largest (smallest) response
    ## value plus (minus) 1/8 of the range.
    if(match(min(last.dif), last.dif) == 2) {
        return(in.range[2] + diff(in.range)/8)
    }
    in.range[1] - diff(in.range)/8
}

NLSstLfAsymptote <-
    ## Find a reasonable value for the left asymptote.
    function(xy) UseMethod("NLSstLfAsymptote")

NLSstLfAsymptote.sortedXyData <-
    function(xy)
{
    ## Is the first response value closest to the minimum or to
    ## the maximum?
    in.range <- range(xy$y)
    first.dif <- abs(in.range - xy$y[1])
    ## Estimate the asymptote as the largest (smallest) response
    ## value plus (minus) 1/8 of the range.
    if(match(min(first.dif), first.dif) == 2) {
        return(in.range[2] + diff(in.range)/8)
    }
    in.range[1] - diff(in.range)/8
}

NLSstAsymptotic <-
    ## fit the asymptotic regression model in the form
    ## b0 + b1*exp(-exp(lrc) * x)
    function(xy) UseMethod("NLSstAsymptotic")

NLSstAsymptotic.sortedXyData <-
    function(xy)
{
    xy$rt <- NLSstRtAsymptote(xy)
    ## Initial estimate of log(rate constant) from a linear regression
    value <- coef(nls(y ~ cbind(1, 1 - exp(-exp(lrc) * x)),
                      data = xy,
                      start = list(lrc =
                      as.vector(log(-coef(lm(log(abs(y - rt)) ~ x,
                                             data = xy))[2]))),
                      algorithm = "plinear"))[c(2, 3, 1)]
    names(value) <- c("b0", "b1", "lrc")
    value
}

### Local variables:
### mode: S
### End:

