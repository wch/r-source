dist <-
function(x, method="euclidian", diag=FALSE, upper=FALSE)
{
	method <-  pmatch(method, c("euclidian", "maximum",
		"manhattan", "canberra", "binary"))
	if(is.na(method))
		stop("invalid distance method")
	if(method == -1)
		stop("ambiguous distance method")

	x <- as.matrix(x)
	len <- nrow(x)*(nrow(x) - 1)/2

	d <- .C("dist",
		as.double(x),
		nrow(x),
		ncol(x),
		double(len),
		as.integer(method))[[4]]
	attr(d, "Size") <- nrow(x)
	attr(d, "Labels") <- dimnames(x)[[1]]
        attr(d, "Diag") <- diag
        attr(d, "Upper") <- upper
	class(d) <- "dist"
	return(d)
}

names.dist <-
function(d)
attr(d, "Labels")

"names<-.dist" <- function(d, n)
{
	if(length(n) != attr(d, "Size"))
		stop("invalid names for dist object")
	attr(d, "Labels") <- n
	d
}

as.matrix.dist <- function(obj)
{
  size <- attr(obj, "Size")
  df <- matrix(0, size, size)
  df[row(df) > col(df)] <- obj
  df <- df + t(df)
  labels <- attr(obj, "Labels")
  if(is.null(labels))
    dimnames(df) <- list(1:size,1:size)
  else
    dimnames(df) <- list(labels,labels)
  
  df
}

print.dist <-
function(obj, diag=NULL, upper=NULL)
{
  if(missing(diag)){
    if(is.null(attr(obj, "Diag")))
      diag <- FALSE
    else
     diag <-  attr(obj, "Diag")
  }
  
  if(missing(upper)){
    if(is.null(attr(obj, "Upper")))
      upper <- FALSE
    else
      upper <-  attr(obj, "Upper")
  }
  
  size <- attr(obj, "Size")
  df <- as.matrix(obj)
  
  if(!upper) {
    df[row(df) < col(df)] <- NA
  }

  if(!diag) {
    df[row(df) == col(df)] <- NA
  }

  if(diag || upper) {
    print(df, na="")
  }
  else {
    print(df[-1,-size], na="")
  }
}


