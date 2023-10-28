#### STRICT test suite in the spirit of no-segfaults,
#### but with explicit statements.

tools::assertError(options(catch.script.errors = NA)) # TRUE or FALSE
options(catch.script.errors = TRUE)
stop("test of 'options(catch.script.errors = TRUE)'")

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
##   catch the error to permit different error messages emitted
##   (handling of infinite recursion is different in the AST interpreter
##   and the byte-code interpreter)

bar <- function() 1+1
foo <- function() { on.exit(bar()); foo() }
tryCatch(foo(), error=function(x) TRUE) # now simple "infinite recursion"
