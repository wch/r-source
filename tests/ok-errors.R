#### STRICT test suite in the spirit of no-segfaults,
#### but with explicit statements.

options(error=expression(NULL))

## These segfaulted in 1.3.x ,  give "could not allocate" errors now
  integer(2^30+1)
   double(2^30+1)
  complex(2^30+1)
character(2^30+1)
vector("list", 2^30+2)

