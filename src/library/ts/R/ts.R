is.mts <- function (x) inherits(x, "mts")

if(F) {
print.ts <- function (x, calendar, ...)
{
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if (missing(calendar))
        calendar <- any(fr.x == c(4, 12))
    if (calendar) {
        if (fr.x > 1) {
            dn2 <- if (fr.x == 12) month.abb
            else if (fr.x == 4) {
                    c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
            }
            else paste("p", 1:fr.x, sep = "")
            if(NROW(x) <= fr.x) { # not more than one period
                dn1 <- start(x)[1]
                dn2 <- dn2[1 + (start(x)[2] - 2 + seq(along=x))%%fr.x]
                x <- format(x, ...)
                x <- matrix(x, nrow = 1 , byrow = TRUE,
                            dimnames = list(dn1, dn2))
            } else { # more than one period
                start.pad <- start(x)[2] - 1
                end.pad <- fr.x - end(x)[2]
                dn1 <- start(x)[1]:end(x)[1]
                x <- c(rep("", start.pad), format(x, ...), rep("", end.pad))
                x <- matrix(x, nc = fr.x, byrow = TRUE,
                            dimnames = list(dn1, dn2))
            }
        } else {
            ## fr.x == 1
            tx <- time(x)
            attributes(x) <- NULL
            names(x) <- tx
        }
        print(x, quote=F)
    } else {
        ##-- no `calendar' --
        st <- start(x)
        en <- end(x)
        if(fr.x == 1) {
            st <- st[1]
            en <- en[1]
        } else if(fr.x == 12) {
            st <- paste(month.abb[st[2]], st[1])
            en <- paste(month.abb[en[2]], en[1])
        } else if(fr.x == 4) {
            st <- paste(st[1], " Q", st[2], sep="")
            en <- paste(en[1], " Q", en[2], sep="")
        } else if(fr.x == 4) {
            st <- paste(st[1],  st[2], sep=":")
            en <- paste(en[1],  en[2], sep=":")
        }
        cat("Time Series:\nStart =", st, "\nEnd =",
            en, "\nFrequency =", deparse(fr.x), "\n")
        attr(x, "tsp") <- NULL
        attr(x, "class") <- NULL
        print(drop(x))
    }
    invisible(x.orig)
}

print.mts <- function (x, calendar, ...)
{
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if (missing(calendar))
        calendar <- any(fr.x == c(4, 12))
    if(calendar) {
        if (fr.x > 1) {
            tm <- time(x)
            t2 <- 1 + floor(fr.x*(tm %%1))
            p1 <- format(floor(tm))
            if(fr.x == 12) {
                    p2 <- month.abb[t2]
                rownames(x) <- paste(p2, p1, sep=" ")
            } else {
                if(fr.x == 4)
                    p2 <- c("Q1", "Q2", "Q3", "Q4")[t2]
                else p2 <- format(t2)
                rownames(x) <- paste(p1, p2, sep=" ")
            }
        } else rownames(x) <- format(time(x))
        class(x) <- tsp(x) <- NULL
    } else {
        ##-- no `calendar' --
        st <- start(x)
        en <- end(x)
        if(fr.x == 1) {
            st <- st[1]
            en <- en[1]
        } else if(fr.x == 12) {
            st <- paste(month.abb[st[2]], st[1])
            en <- paste(month.abb[en[2]], en[1])
        } else if(fr.x == 4) {
            st <- paste(st[1], " Q", st[2], sep="")
            en <- paste(en[1], " Q", en[2], sep="")
        } else if(fr.x == 4) {
            st <- paste(st[1],  st[2], sep=":")
            en <- paste(en[1],  en[2], sep=":")
        }
        cat("Time Series:\nStart =", st, "\nEnd =",
            en, "\nFrequency =", deparse(fr.x), "\n")
        rownames(x) <- format(time(x))
        class(x) <- tsp(x) <- NULL
    }
    print(x)
    invisible(x.orig)
}
}


ts.plot <- function(..., gpars=list())
{
    sers <- ts.union(...)
    do.call("plot.ts", c(list(sers, plot.type="single"), gpars))
}

Ops.ts <- function(e1, e2)
{
    if(missing(e2)) {
        ## univariate operator
        NextMethod(.Generic)
    } else if(any(nchar(.Method) == 0)) {
        ## one operand is not a ts
        NextMethod(.Generic)
    } else {
        ## use ts.intersect to align e1 and e2
        nc1 <- NCOL(e1)
        nc2 <- NCOL(e2)
        e12 <- ts.intersect(e1, e2)
        e1 <- if(is.matrix(e1)) e12[, 1:nc1, drop = FALSE] else e12[, 1]
        e2 <- if(is.matrix(e2)) e12[, nc1 + (1:nc2), drop = FALSE]
        else e12[, nc1 + 1]
        NextMethod(.Generic)
    }
}
