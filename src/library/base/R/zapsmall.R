zapsmall <- function(x, digits = .Options$digits)
{
  if(all(ina <- is.na(x))) return(x)
  mx <- max(abs(x[!ina]))
  round(x, digits = if(mx > 0) max(0, digits - log10(mx)) else digits)
}
