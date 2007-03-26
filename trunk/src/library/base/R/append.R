append <- function (x, values, after = length(x))
{
    lengx <- length(x)
    if (after <= 0)
	c(values, x)
    else if (after >= lengx)
	c(x, values)
    else c(x[1:after], values, x[(after + 1):lengx])
}
