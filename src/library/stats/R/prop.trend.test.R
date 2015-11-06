#  File src/library/stats/R/prop.trend.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

prop.trend.test <- function (x, n, score = seq_along(x))
{
    method <- "Chi-squared Test for Trend in Proportions"
    dname <- paste(deparse(substitute(x)), "out of", deparse(substitute(n)))
    dname <- paste(dname, ",\n using scores:", paste(score, collapse = " "))

    ## Tabular input has caused grief, get rid of dim() attributes:
    x <- as.vector(x)
    n <- as.vector(n)

    p <- sum(x)/sum(n)
    a <- anova(lm(freq ~ score, data = list(freq = x/n, score = as.vector(score),
                                            w = n/p/(1 - p)),
                  weights = w))
    chisq <- c("X-squared" = a["score", "Sum Sq"])
    structure(list(statistic = chisq,
                   parameter = c(df = 1),
                   p.value = pchisq(as.numeric(chisq), 1, lower.tail = FALSE),
                   method = method, data.name = dname),
              class = "htest")
}
