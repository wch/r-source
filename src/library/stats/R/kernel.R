# Copyright (C) 1997-1999  Adrian Trapletti
#

kernel <- function (coef, m = length(coef)+1, r, name="unknown")
{
    modified.daniell.kernel <- function (m)
    {
        if (any(m) < 0) stop ("'m' is negative")
        if(length(m) == 1)
            return (kernel(c(rep(1, m), 0.5)/(2*m), m,
                           name=paste("mDaniell(",m,")",sep="")))
        else {
            k <- Recall(m[1])
            for(i in 2:length(m)) k <- kernapply(k,  Recall(m[i]))
        }
        k
    }

    daniell.kernel <- function (m)
    {
        if (any(m) < 0) stop ("'m' is negative")
        if(length(m) == 1)
            return (kernel(rep(1/(2*m+1),m+1),m,
                           name=paste("Daniell(",m,")",sep="")))
        else {
            k <- Recall(m[1])
            for(i in 2:length(m)) k <- kernapply(k,  Recall(m[i]))
        }
        k
    }

    fejer.kernel <- function (m, r)
    {
        if (r < 1) stop ("'r' is less than 1")
        if (m < 1) stop ("'m' is less than 1")
        n <- 2*m+1
        wn <- double(m+1)
        for (j in (1:m))
        {
            wj <- 2*pi*j/n
            wn[j+1] <- sin(r*wj/2)^2/sin(wj/2)^2
        }
        wn <- wn/(n*r)
        wn[1] <- r/n
        wn <- wn/sum(c(rev(wn[2:(m+1)]),wn))
        kernel(wn, m, name=paste("Fejer(",m,",",r,")", sep=""))
    }

    dirichlet.kernel <- function (m, r)
    {
        if (r < 0) stop ("'r' is less than 0")
        if (m < 1) stop ("'m' is less than 1")
        n <- 2*m+1
        wn <- double(m+1)
        for (j in (1:m))
        {
            wj <- 2*pi*j/n
            wn[j+1] <- sin((r+0.5)*wj)/sin(wj/2)
        }
        wn <- wn/n
        wn[1] <- (2*r+1)/n
        wn <- wn/sum(c(rev(wn[2:(m+1)]),wn))
        return (kernel(wn, m, name=paste("Dirichlet(",m,",",r,")",sep="")))
    }

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
        eps <- getOption("ts.eps")
        if ((sum(kernel[-m:m]) > 1.0+eps) || (sum(kernel[-m:m]) < 1.0-eps))
            stop ("coefficients do not add to 1")
        kernel
    }
}

print.tskernel <- function (x, digits = max(3,getOption("digits")-3), ...)
{
    y <- c(rev(x$coef[2:(x$m + 1)]), x$coef)
    i <- -x$m:x$m
    cat(attr(x, "name"), "\n")
    cat(paste("coef[", format(i), "] = ", format(y, digits = digits),
              sep = ""), sep = "\n")
}

plot.tskernel <- function (x, ...)
{
    y <- c(rev(x$coef[2:(x$m+1)]),x$coef)
    plot ((-x$m:x$m), y, xlab="k", ylab="W[k]", type="h", main=attr(x,"name"))
}

df.kernel <- function (k)
{
    2/sum(k[-k$m:k$m]^2)
}

bandwidth.kernel <- function (k)
{
    sqrt(sum((1/12 + (-k$m:k$m)^2) * k[-k$m:k$m]))
}


"[.tskernel" <- function (k, i)
{
    y <- c(rev(k$coef[2:(k$m+1)]), k$coef)
    y[i+k$m+1]
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
    if (length(x) <= 2*k$m)
        stop ("'x' is shorter than kernel 'k'")
    if (k$m == 0)
        return (x)
    else
    {
        n <- length(x)
        w <- c(k[0:k$m], rep(0,n-2*k$m-1), k[-k$m:-1])
        y <- fft(fft(x)*fft(w), inverse = TRUE)/n
        if (is.numeric(x)) y <- Re(y)
        if (circular)
            return (y)
        else
            return (y[(1+k$m):(n-k$m)])
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

kernapply.tskernel <- function (x, k2, ...)
{
    if (!is.tskernel(x))
        stop ("'k1' is not a kernel")
    if (!is.tskernel(x))
        stop ("'k2' is not a kernel")
    n <- k2$m
    xx <- c(rep(0,n), x[-x$m:x$m], rep(0,n))
    coef <- kernapply(xx, k2, circular = TRUE)
    m <- length(coef)%/%2
    kernel(coef[(m+1):length(coef)],m,
           paste("Composite(", attr(x, "name"), ",",
                 attr(k2, "name"), ")", sep=""))
}







