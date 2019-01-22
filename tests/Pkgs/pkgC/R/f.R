setGeneric("unique")

nil <- NULL # see if we can use '::' on it

Cfun <- function(x) {
    new("classA", # classA was imported from 'pkgA'
       cbind(x, b=x+2))
}
