subset.data.frame <-
function (dfr, subset, select) 
{
	if(missing(subset))
		r<-NULL
	else {
		e <- substitute(subset)
		r <- eval(e,dfr)
		r <- r & !is.na(r)
	}
	if(missing(select))
		vars<-NULL
	else {
		nl<-as.list(1:ncol(dfr))
		names(nl)<-names(dfr)
		vars<-eval(substitute(select),nl)
	}
	dfr[r,vars,drop=F]
}

subset<-
function(x,...)
	UseMethod("subset")

subset.default <- 
function(x,subset) 
	x[subset & !is.na(subset)]

transform.data.frame <-
function (dfr, ...) 
{
        e <- eval(substitute(list(...)), dfr)
        tags <- names(e)
        inx <- match(tags, names(dfr))
        matched <- !is.na(inx)
        if (any(matched)) {
                dfr[inx[matched]] <- e[matched]
		dfr<-data.frame(dfr)
	}
        if (!all(matched)) 
                data.frame(dfr, e[!matched])
        else dfr
}

transform <-
function(x,...)
	UseMethod("transform")

# Actually, I have no idea what to transform(), except dataframes.
# The default converts its argument to a dataframe and transforms
# that. This is probably marginally useful at best. --pd
transform.default <- 
function(x,...)
	transform.data.frame(data.frame(x),...)
