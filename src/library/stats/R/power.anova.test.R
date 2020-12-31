#  File src/library/stats/R/power.anova.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

power.anova.test <-
function (groups = NULL, n = NULL, between.var = NULL, within.var = NULL,
	  sig.level = 0.05, power = NULL)
{
    ## Check parameters
    if (sum(vapply(list(groups, n, between.var, within.var, power, sig.level),
		   is.null, NA)) != 1)
	stop("exactly one of 'groups', 'n', 'between.var', 'within.var', 'power', and 'sig.level' must be NULL")
    if (!is.null(groups) && groups < 2)
      stop("number of groups must be at least 2")
    if (!is.null(n) && n < 2)
      stop("number of observations in each group must be at least 2")
    if(!is.null(sig.level) && !is.numeric(sig.level) ||
       any(0 > sig.level | sig.level > 1))
	stop("'sig.level' must be numeric in [0, 1]")

    p.fun <- function(groups, n, within.var, between.var, sig.level) {
	lambda <- (groups-1)*n*(between.var/within.var)
	pf(qf(sig.level, groups-1, (n-1)*groups, lower.tail = FALSE),
	   groups-1, (n-1)*groups, lambda, lower.tail = FALSE)
    }

    if (is.null(power))
	power <- p.fun(groups, n, within.var, between.var, sig.level)
    else if (is.null(groups)) {
        fun.groups <- function(groups)
            p.fun(groups, n, within.var, between.var, sig.level) - power
	groups <- uniroot(fun.groups, c(2, 1e+02))$root
    }
    else if (is.null(n)) {
        fun.n <- function(n)
            p.fun(groups, n, within.var, between.var, sig.level) - power
	n <- uniroot(fun.n, c(2, 1e+05))$root
    }
    else if (is.null(within.var)) {
        fun.wv <- function(within.var)
            p.fun(groups, n, within.var, between.var, sig.level) - power
	within.var <- uniroot(fun.wv, between.var * c(1e-07, 1e+07))$root
    }
    else if (is.null(between.var)) {
        fun.var <- function(between.var)
            p.fun(groups, n, within.var, between.var, sig.level) - power
	between.var <- uniroot(fun.var, within.var * c(1e-07, 1e+07))$root
    }
    else if (is.null(sig.level)) {
        fun.sl <- function(sig.level)
            p.fun(groups, n, within.var, between.var, sig.level) - power
	sig.level <- uniroot(fun.sl, c(1e-10, 1 - 1e-10))$root
    }
    else stop("internal error", domain = NA)
    NOTE <- "n is number in each group"
    METHOD <- "Balanced one-way analysis of variance power calculation"
    structure(list(groups = groups, n = n, between.var = between.var,
		   within.var = within.var, sig.level = sig.level,
		   power = power, note = NOTE, method = METHOD),
	      class = "power.htest")
}
