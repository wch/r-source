power.t.test <-
    function(n=NULL, delta=NULL, sd=1, sig.level=0.05, power=NULL,
	     type=c("two.sample", "one.sample", "paired"),
	     alternative=c("two.sided", "one.sided"), strict=FALSE)
{
    if ( sum(sapply(list(n, delta, sd, power, sig.level), is.null)) != 1 )
	stop("exactly one of 'n', 'delta', 'sd', 'power', and 'sig.level' must be NULL")
    if(!is.null(sig.level) && !is.numeric(sig.level) ||
       any(0 > sig.level | sig.level > 1))
	stop("'sig.level' must be numeric in [0, 1]")

    type <- match.arg(type)
    alternative <- match.arg(alternative)

    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)
    if (tside == 2 && !is.null(delta)) delta <- abs(delta)

    p.body <- quote({nu <- (n - 1) * tsample
		     pt(qt(sig.level/tside, nu, lower = FALSE),
			nu, ncp = sqrt(n/tsample) * delta/sd, lower = FALSE)})
    if (strict & tside == 2) # count rejections in opposite tail
      p.body <- quote({
	  nu <- (n - 1) * tsample
	  qu<-qt(sig.level/tside, nu, lower = FALSE)
	  pt(qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower = FALSE) +
	    pt(-qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower = TRUE)
      })

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

    METHOD <- paste(switch(type,
			   one.sample = "One-sample",
			   two.sample = "Two-sample",
			   paired = "Paired"),
		    "t test power calculation")

    structure(list(n=n, delta=delta, sd=sd,
		   sig.level=sig.level, power=power,
		   alternative=alternative, note=NOTE, method=METHOD),
	      class="power.htest")
}

power.prop.test <-
    function(n=NULL, p1=NULL, p2=NULL, sig.level=0.05, power=NULL,
	     alternative=c("two.sided", "one.sided"), strict=FALSE)
{
    if ( sum(sapply(list(n, p1, p2, power, sig.level), is.null)) != 1 )
	stop("exactly one of 'n', 'p1', 'p2', 'power', and 'sig.level' must be NULL")
    if(!is.null(sig.level) && !is.numeric(sig.level) ||
       any(0 > sig.level | sig.level > 1))
	stop("'sig.level' must be numeric in [0, 1]")

    alternative <- match.arg(alternative)

    tside <- switch(alternative, one.sided = 1, two.sided = 2)

    p.body <- quote(pnorm(((sqrt(n) * abs(p1 - p2)
			    - (qnorm(sig.level/tside, lower = FALSE)
			       * sqrt((p1 + p2) * (1 - (p1 + p2)/2))))
			   / sqrt(p1 * (1 - p1) + p2 * (1 - p2)))))

    if (strict & tside == 2) # count rejections in opposite tail
      p.body <- quote({
	  qu <- qnorm(sig.level/tside, lower = FALSE)
	  d <- abs(p1 - p2)
	  q1 <- 1 - p1
	  q2 <- 1 - p2
	  pbar <- (p1 + p2)/2
	  qbar <- 1 - pbar
	  v1 <- p1 * q1
	  v2 <- p2 * q2
	  vbar <- pbar * qbar
	  pnorm((sqrt(n)*d - qu * sqrt(2 * vbar) )
		/ sqrt(v1 + v2)) +
	  pnorm((sqrt(n)*d + qu * sqrt(2 * vbar) )
		/ sqrt(v1 + v2), lower=FALSE)
      })
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

print.power.htest <-
function(x, ...)
{
    cat("\n    ", x$method, "\n\n")
    note <- x$note
    x[c("method","note")] <- NULL
    cat(paste(format(names(x), width= 15, justify = "right"),
	      format(x), sep= " = "), sep= "\n")
    if(!is.null(note))
	cat("\n", "NOTE:", note, "\n\n")
    else
	cat("\n")
}
