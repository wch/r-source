pairwise.t.test <-
function(x, g, p.adjust.method = p.adjust.methods, pool.sd = TRUE, ...)
{
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    p.adjust.method <- match.arg(p.adjust.method)
    if (pool.sd)
    {
        METHOD <- "t tests with pooled SD"
        xbar <- tapply(x, g, mean, na.rm = TRUE)
        s <- tapply(x, g, sd, na.rm = TRUE)
        n <- tapply(!is.na(x), g, sum)
        degf <- n - 1
        total.degf <- sum(degf)
        pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
        compare.levels <- function(i, j) {
            dif <- xbar[i] - xbar[j]
            se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
            t.val <- dif/se.dif
            2 * pt(-abs(t.val), total.degf)
        }
    } else {
        METHOD <- "t tests with non-pooled SD"
        compare.levels <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, ...)$p.value
        }
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}


pairwise.wilcox.test <-
function(x, g, p.adjust.method = p.adjust.methods, ...)
{
    p.adjust.method <- match.arg(p.adjust.method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    METHOD <- "Wilcoxon rank sum test"
    compare.levels <- function(i, j) {
        xi <- x[as.integer(g) == i]
        xj <- x[as.integer(g) == j]
        wilcox.test(xi, xj, ...)$p.value
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

pairwise.prop.test <-
function (x, n, p.adjust.method = p.adjust.methods, ...)
{
    p.adjust.method <- match.arg(p.adjust.method)
    METHOD <- "Pairwise comparison of proportions"
    DNAME <- deparse(substitute(x))
    if (is.matrix(x)) {
        if (ncol(x) != 2)
            stop("x must have 2 columns")
        n <- rowSums(x)
        x <- x[, 1]
    }
    else {
        DNAME <- paste(DNAME, "out of", deparse(substitute(n)))
        if (length(x) != length(n))
            stop("x and n must have the same length")
    }
    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if (length(x) < 2)
        stop("too few groups")
    compare.levels <- function(i, j) {
        prop.test(x[c(i,j)], n[c(i,j)], ...)$p.value
    }
    level.names <- names(x)
    if (is.null(level.names)) level.names <- seq(along=x)
    PVAL <- pairwise.table(compare.levels, level.names, p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

pairwise.table <-
function(compare.levels, level.names, p.adjust.method)
{
    ix <- seq(along=level.names)
    names(ix) <- level.names
    pp <- outer(ix[-1], ix[-length(ix)],function(ivec, jvec)
          sapply(seq(along=ivec), function(k) {
              i<-ivec[k]
              j<-jvec[k]
              if (i > j) compare.levels(i, j) else NA
          }))
    pp[lower.tri(pp, TRUE)] <- p.adjust(pp[lower.tri(pp, TRUE)],
                                        p.adjust.method)
    pp
}

print.pairwise.htest <-
function(x, ...)
{
    cat("\n\tPairwise comparisons using", x$method, "\n\n")
    cat("data: ", x$data.name, "\n\n")
    pp <- format.pval(x$p.value, 2, na.form="-")
    attributes(pp) <- attributes(x$p.value)
    print(pp, quote=FALSE)
    cat("\nP value adjustment method:", x$p.adjust.method, "\n")
}
