plot <- function (x, y, ...) 
{
    if (is.null(attr(x, "class")) && is.function(x)) {
        if ("ylab" %in% names(list(...))) 
            plot.function(x, ...)
        else plot.function(x, ylab = paste(deparse(substitute(x)), 
                              "(x)"), ...)
    }
    else UseMethod("plot")
}
