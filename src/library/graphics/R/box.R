box <- function(which="plot", lty="solid", ...)
{
    which <- pmatch(which[1], c("plot", "figure", "inner", "outer"))
    .Internal(box(which=which, lty=lty, ...))
}
