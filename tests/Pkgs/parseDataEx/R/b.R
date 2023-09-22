## a function w/o "{ .. }"  .. this is all about keeping parse data:
g <- function(x) # comment g()
    if(x) TRUE

## with "{ .. }"
h <- function(x) { # argument x
    if(is.atomic(x))
        x # atomic vector
    else list(if(length(x)) x[[1]], # <-- "nothing" when length zero
              le = length(x), cl = class(x)) # something else
}

