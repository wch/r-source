vector <- function(mode = "logical", length = 0).Internal(vector(mode,length))
logical <- function(length = 0) vector("logical", length)
character <- function(length = 0) vector("character", length)
integer <- function(length = 0) vector("integer", length)
real <- function(length = 0) vector("real", length)
double <- function(length = 0) vector("real", length)
numeric <- double
complex <- function(length = 0, real = numeric(), imaginary = numeric())
.Internal(complex(length, real, imaginary))

