# Copyright (C) 1997-1999  Adrian Trapletti
#
### Copyright (C) 1999-2006  The R Development Core Team


kernel <- function (coef, m = length(coef)+1, r, name="unknown")
{
    mkName <- function(name, args)
        paste(name,"(", paste(args, collapse=","), ")", sep="")

    modified.daniell.kernel <- function (m)
    {
        if(length(m) == 1)
            k <- kernel(c(rep(1, m), 0.5)/(2*m), m)
        else {
            k <- Recall(m[1])
            for(i in 2:length(m)) k <- kernapply(k,  Recall(m[i]))
        }
        attr(k,"name") <- mkName("mDaniell", m)
        k
    }

    daniell.kernel <- function (m)
    {
        if(length(m) == 1)
            k <- kernel(rep(1/(2*m+1),m+1), m)
        else {
            k <- Recall(m[1])
            for(i in 2:length(m)) k <- kernapply(k,  Recall(m[i]))
        }
        attr(k,"name") <- mkName("Daniell", m)
        k
    }

    fejer.kernel <- function (m, r)
    {
        if (r < 1) stop ("'r' is less than 1")
        if (m < 1) stop ("'m' is less than 1")
        n <- 2*m+1
        wn <- double(m+1)
        wj <- 2*pi*(1:m)/n
        wn[2:(m+1)] <- sin(r*wj/2)^2 / sin(wj/2)^2 / r
        wn[1] <- r
        wn <- wn / (wn[1] + 2*sum(wn[2:(m+1)]))
        kernel(wn, m, name = mkName("Fejer", c(m,r)))
    }

    dirichlet.kernel <- function (m, r)
    {
        if (r < 0) stop ("'r' is less than 0")
        if (m < 1) stop ("'m' is less than 1")
        n <- 2*m+1
        wn <- double(m+1)
        wj <- 2*pi*(1:m)/n
        wn[2:(m+1)] <- sin((r+0.5)*wj) / sin(wj/2)
        wn[1] <- 2*r+1
        wn <- wn / (wn[1] + 2*sum(wn[2:(m+1)]))
        kernel(wn, m, name = mkName("Dirichlet", c(m,r)))
    }

    if(!missing(m))
	if(!is.numeric(m) || length(m) < 1 || m != round(m) || any(m) < 0)
	    stop("'m' must be numeric with non-negative integers")

    if(is.character(coef)) {
        switch(coef,
               daniell = daniell.kernel(m),
               dirichlet = dirichlet.kernel(m, r),
               fejer = fejer.kernel(m, r),
               modified.daniell = modified.daniell.kernel(m),
               stop("unknown named kernel"))
    } else {
        if (!is.vector(coef))
            stop ("'coef' must be a vector")
        if ((length(coef) != m+1) | (length(coef) <= 0))
            stop ("'coef' does not have the correct length")
        kernel <- list (coef=coef, m=m)
        attr(kernel, "name") <- name
        class(kernel) <- "tskernel"
        sk <- sum(kernel[-m:m]) # via '[.kernel' !
        if (abs(sk - 1) > getOption("ts.eps"))
            stop ("coefficients do not add to 1")
        kernel
    }
}

print.tskernel <- function (x, digits = max(3,getOption("digits")-3), ...)
{
    m <- x$m
    y <- x[i <- -m:m]
    cat(attr(x, "name"), "\n")
    cat(paste("coef[", format(i), "] = ", format(y, digits = digits), sep = ""),
        sep = "\n")
    invisible(x)
}

plot.tskernel <-
    function(x, type = "h", xlab = "k", ylab = "W[k]", main=attr(x,"name"), ...)
{
    i <- -x$m:x$m
    plot(i, x[i], type = type, xlab = xlab, ylab = ylab, main = main, ...)
}

df.kernel <- function (k)
{
    2/sum(k[-k$m:k$m]^2)
}

bandwidth.kernel <- function (k)
{
    i <- -k$m:k$m
    sqrt(sum((1/12 + i^2) * k[i]))
}


"[.tskernel" <- function (k, i)
{
    m1 <- k$m + 1
    y <- k$coef[c(m1:2, 1:m1)]
    y[i+m1]
}

is.tskernel <- function (k)
{
    inherits(k, "tskernel")
}

kernapply <- function (x, ...)
{
    UseMethod("kernapply")
}

kernapply.vector <- function (x, k, circular = FALSE, ...)
{
    if (!is.vector(x)) stop ("'x' is not a vector")
    if (!is.tskernel(k)) stop ("'k' is not a kernel")
    m <- k$m
    if (length(x) <= 2*m)
        stop ("'x' is shorter than kernel 'k'")
    if (m == 0)
        return (x)
    else
    {
        n <- length(x)
        w <- c(k[0:m], rep(0,n-2*m-1), k[-m:-1])
        y <- fft(fft(x)*fft(w), inverse = TRUE)/n
        if (is.numeric(x)) y <- Re(y)
        if (circular)
            return (y)
        else
            return (y[(1+m):(n-m)])
    }
}

kernapply.default <- function (x, k, circular = FALSE, ...)
{
    if (is.vector(x))
        return (kernapply.vector(x, k, circular=circular))
    else if (is.matrix(x))
        return (apply(x, MARGIN=2, FUN=kernapply, k, circular=circular))
    else
        stop ("'kernapply' is not available for object 'x'")
}

kernapply.ts <- function (x, k, circular = FALSE, ...)
{
    if (!is.matrix(x))
        y <- kernapply.vector(as.vector(x), k, circular=circular)
    else
        y <- apply(x, MARGIN=2, FUN=kernapply, k, circular=circular)
    ts (y, end=end(x), frequency=frequency(x))
}

kernapply.tskernel <- function (x, k, ...)
{
    if (!is.tskernel(x))
        stop ("'x' is not a kernel")
    if (!is.tskernel(k))
        stop ("'k' is not a kernel")
    n <- k$m
    xx <- c(rep(0,n), x[-x$m:x$m], rep(0,n))
    coef <- kernapply(xx, k, circular = TRUE)
    m <- length(coef)%/%2
    kernel(coef[(m+1):length(coef)],m,
           paste("Composite(", attr(x, "name"), ",",
                 attr(k, "name"), ")", sep=""))
}
