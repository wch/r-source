plot <- function (x, y, ...) 
{
    if (is.null(attr(x, "class")) && is.function(x)) {
        if ("ylab" %in% names(list(...))) 
            plot.function(x,y, ...)
        else plot.function(x,y, ylab = paste(deparse(substitute(x)), 
                              "(x)"), ...)
    }
    else UseMethod("plot")
}
