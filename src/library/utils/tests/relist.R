## PR#15854
local({
    ## This always worked
    x <- as.relistable(list(integer(), 1:2, double(), 3))
    vec <- unlist(x)
    vec[[2]] <- 10
    stopifnot(identical(relist(vec),
                        as.relistable(list(double(), c(1, 10), double(), 3))))
    ## Used to fail, Error .. The 'flesh' argument does not contain a skeleton attribute. ...
    x <- as.relistable(list(integer(), 1:2, NULL, 3))
    vec <- unlist(x)
    vec[[2]] <- 10
    stopifnot(identical(relist(vec),
                        as.relistable(list(double(), c(1, 10), double(), 3))))
    ## ditto in PR#..:
    x <- list(NULL, a=1:3, b=5:7)
    y <- unlist(as.relistable(x))
    stopifnot(identical(relist(y),
                        as.relistable(list(`names<-`(integer(),character()),
                                           a=1:3, b=5:7))))
    ## relist(y) gave Error ... :
    ##   The 'flesh' argument does not contain a skeleton attribute.
    ##   Either ensure you unlist a relistable object, or specify the skeleton separately.
})
