.hide.get <-
    function (x, pos = -1, envir = as.environment(pos), mode = "any",
              inherits = TRUE)
    .Internal(get(x, envir, mode, inherits))

.hide.exists <-
    function (x, where = -1,
              envir = if(missing(frame)) as.environment(where) else sys.frame(frame),
              frame, mode = "any", inherits = TRUE)
    .Internal(exists(x, envir, mode, inherits))

.hide.assign <-
    function (x, value, pos = -1, envir = as.environment(pos),
              inherits = FALSE, immediate = TRUE)
    .Internal(assign(x, value, envir, inherits))


.hide.objects <- .hide.ls <-
    function (name, pos = -1, envir = as.environment(pos), all.names = FALSE,
              pattern)
{
    if (!missing(name)) {
        nameValue <- try(name)
        if(identical(class(nameValue), "try-error")) {
            name <- substitute(name)
            if (!is.character(name))
                name <- deparse(name)
            pos <- name
        }
        else
            pos <- nameValue
    }
    all.names <- .Internal(ls(envir, all.names))
    if (!missing(pattern)) {
        if ((ll <- length(grep("\\[", pattern))) > 0 && ll !=
            (lr <- length(grep("\\]", pattern)))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern `[' by `\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern) > 0)) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced `[<-' by `\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}

.hide.remove <- .hide.rm <-
    function (..., list = character(0), pos = -1,
              envir = as.environment(pos), inherits = FALSE)
{
    names <- as.character(substitute(list(...)))[-1]
    list <- .Primitive("c")(list, names)
    .Internal(remove(list, envir, inherits))
}

as.environment<- function(object)
{
    switch(typeof(object),
           environment = return(object),
           character = {
               n <- match(object, search())
               if(is.na(n))
                   stop(paste("\"", c(object, "character(0)")[[1]],
                              "\" not found on search list", sep=""))
               object <- n
           },
           integer = {
               if(identical(object, as.integer(-1)))
                   return(sys.frame(sys.parent(2)))
           },
           double =  {
               if(identical(object, -1))
                   return(sys.frame(sys.parent(2)))
           })
    pos.to.env(object)
}
