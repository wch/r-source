zapsmall <- function(x, digits = getOption("digits"))
{
    if (length(digits) == 0)
        stop("invalid 'digits'")
    if (all(ina <- is.na(x)))
        return(x)
    mx <- max(abs(x[!ina]))
    round(x, digits = if(mx > 0) max(0, digits - log10(mx)) else digits)
}
