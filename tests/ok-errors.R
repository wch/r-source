#### STRICT test suite in the spirit of no-segfaults,
#### but with explicit statements.

options(error=expression(NULL))
stop("test of `options(error=expression(NULL))'")

if(FALSE) {
## these ought to work on machines with enough memory
## These segfaulted in 1.3.x ,  give "could not allocate" errors now
  integer(2^30+1)
   double(2^30+1)
  complex(2^30+1)
character(2^30+1)
vector("list", 2^30+2)
}

## bad infinite recursion / on.exit / ... interactions
##   catch the error to permit different error texts and different
##   number of the error messages emitted (handling of infinite
##   recursion is different in AST interpretter and byte-code
##   interpretter/compiler

{ g <- function() 1+1 ; f <- function() { on.exit(g()) ; f() } ; tryCatch(f(), error=function(x) TRUE) }

{ f <- function() { f() } ; tryCatch(f(), error=function(x) TRUE) }
{ f <- function() { on.exit(f()) } ; tryCatch(f(), error=function(x) TRUE) }
{ f <- function() { on.exit(f()) ; f() } ; tryCatch(f(), error=function(x) TRUE) }

{ f <- function() f() ; tryCatch(tryCatch(f(), finally=f()), error=function(x) TRUE) }
{ f <- function() f() ; tryCatch(tryCatch(f(), error=f()), error=function(x) TRUE) }
{ f <- function() f() ; tryCatch(tryCatch(f(), error=f(), finally=f()), error=function(x) TRUE) }
{ f <- function() tryCatch(f(), error=f(), finally=f()) ; tryCatch(f(), error=function(x) TRUE) }
