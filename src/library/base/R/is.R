is.vector <- function(x, mode="any") .Internal(is.vector(x,mode))

"is.na<-" <- function(x, value) UseMethod("is.na<-")

"is.na<-.default" <- function(x, value)
{
    x[value] <- NA
    x
}

is.primitive <- function(x)
    switch(typeof(x),
	   "special" = , "builtin" = TRUE,
	   FALSE)


