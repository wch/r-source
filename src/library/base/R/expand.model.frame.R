expand.model.frame <- function(model, extras,
                               envir=environment(formula(model)),
                               na.expand=FALSE)
{
    ## don't use model$call$formula -- it might be a variable name
    f <- formula(model)
    data <- eval(model$call$data, envir)

    # new formula (there must be a better way...)
    ff <- foo ~ bar + baz
    if (is.call(extras))
        gg <- extras
    else
        gg <- parse(text=paste("~", paste(extras, collapse="+")))[[1]]
    ff[[2]] <- f[[2]]
    ff[[3]][[2]] <- f[[3]]
    ff[[3]][[3]] <- gg[[2]]

    if (!na.expand){
        naa <- model$call$na.action
        subset <- model$call$subset
        rval <- eval(call("model.frame",ff, data = data, subset = subset, 
                      na.action = naa),envir )
    } else {
        subset <- model$call$subset
        rval <- eval(call("model.frame",ff, data = data, subset = subset, 
                          na.action = I), envir)
        oldmf <- model.frame(model)
        keep <- match(rownames(oldmf), rownames(rval))
        rval <- rval[keep, ]
        class(rval) <- "data.frame" # drop "AsIs"
    }

    return(rval)
}
