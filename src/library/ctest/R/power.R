power.t.test <-
    function(n=NULL, delta=NULL, sd=1, sig.level=0.05, power=NULL,
             type=c("two.sample", "one.sample", "paired"),
             alternative=c("two.sided", "one.sided"))
{
    if ( sum(sapply(list(n, delta, sd, power, sig.level), is.null)) != 1 )
        stop("exactly one of n, delta, sd, power, and sig.level must be NULL")

    type <- match.arg(type)
    alternative <- match.arg(alternative)

    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)

    p.body <- quote(1 - pt( qt(1 - sig.level/tside, (n - 1) * tsample),
                           (n - 1) * tsample, ncp = sqrt(n/tsample) *
                           delta/sd))
    if (is.null(power))
        power <- eval(p.body)
    else if (is.null(n))
        n <- uniroot(function(n) eval(p.body) - power,
                     c(2,1e7))$root
    else if (is.null(sd))
        sd <- uniroot(function(sd) eval(p.body) - power,
                      delta * c(1e-7,1e+7))$root
    else if (is.null(delta))
        delta <- uniroot(function(delta) eval(p.body) - power,
                      sd * c(1e-7,1e+7))$root
    else if (is.null(sig.level))
        sig.level <- uniroot(function(sig.level) eval(p.body) - power,
                      c(1e-10,1-1e-10))$root
    else # Shouldn't happen
        stop("internal error")
    NOTE <- switch(type,
                   paired = "n is number of *pairs*, sd is std.dev. of *differences* within pairs",
                   two.sample = "n is number in *each* group", NULL)

    METHOD <- switch(type,
                     one.sample = "One-sample t test power calculation",
                     two.sample =  "Two-sample t test power calculation",
                     paired = "Paired t test power calculation")

    structure(list(n=n, delta=delta, sd=sd,
                   sig.level=sig.level, power=power,
                   alternative=alternative, note=NOTE, method=METHOD),
              class="power.htest")
}

power.prop.test <-
    function(n=NULL, p1=NULL, p2=NULL, sig.level=0.05, power=NULL,
             alternative=c("two.sided", "one.sided"))
{
    if ( sum(sapply(list(n, p1, p2, power, sig.level), is.null)) != 1 )
        stop("exactly one of n, p1, p2, power, and sig.level must be NULL")

    alternative <- match.arg(alternative)

    tside <- switch(alternative, one.sided = 1, two.sided = 2)

    p.body <- quote(pnorm(((sqrt(n) * abs(p1 - p2)
                            - (qnorm(1 - (sig.level/tside))
                             * sqrt((p1 + p2) * (1 - (p1 + p2)/2))))
                           /sqrt(p1 * (1 - p1) + p2 * (1 - p2)))))

    if (is.null(power))
        power <- eval(p.body)
    else if (is.null(n))
        n <- uniroot(function(n) eval(p.body) - power,
                     c(1,1e7))$root
    else if (is.null(p1))
        p1 <- uniroot(function(p1) eval(p.body) - power,
                      c(0,p2))$root
    else if (is.null(p2))
        p2 <- uniroot(function(p2) eval(p.body) - power,
                      c(p1,1))$root
    else if (is.null(sig.level))
        sig.level <- uniroot(function(sig.level) eval(p.body) - power,
                      c(1e-10,1-1e-10))$root
    else # Shouldn't happen
        stop("internal error")

    NOTE <- "n is number in *each* group"

    METHOD <-  "Two-sample comparison of proportions power calculation"

    structure(list(n=n, p1=p1, p2=p2,
                   sig.level=sig.level, power=power,
                   alternative=alternative, note=NOTE, method=METHOD),
              class="power.htest")
}

print.power.htest <- function(x)
{
    cat("\n    ", x$method, "\n\n")
    note<-x$note
    x[c("method","note")] <- NULL
    cat(paste(format.char(names(x), width=15, flag="+"),
              format(x), sep=" = "),sep="\n")
    if(!is.null(note))
        cat("\n", "NOTE:", note, "\n\n")
    else
        cat("\n")
}
