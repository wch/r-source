callCC <- function(fun) {
    value <- NULL
    delayedAssign("throw", return(value))
    fun(function(v) { value <<- v; throw })
}
