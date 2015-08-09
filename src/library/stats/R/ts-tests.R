#  File src/library/stats/R/ts-tests.R
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

Box.test <- function (x, lag = 1, type=c("Box-Pierce", "Ljung-Box"), fitdf=0)
{
    if (NCOL(x) > 1)
        stop ("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    type <- match.arg(type)
    cor <- acf (x, lag.max = lag, plot = FALSE, na.action = na.pass)
    n <- sum(!is.na(x))
    PARAMETER <- c(df = lag-fitdf)
    obs <- cor$acf[2:(lag+1)]
    if (type=="Box-Pierce")
    {
        METHOD <- "Box-Pierce test"
        STATISTIC <- n*sum(obs^2)
        PVAL <- 1-pchisq(STATISTIC, lag-fitdf)
    }
    else
    {
        METHOD <- "Box-Ljung test"
        STATISTIC <- n*(n+2)*sum(1/seq.int(n-1, n-lag)*obs^2)
        PVAL <- 1-pchisq(STATISTIC, lag-fitdf)
    }
    names(STATISTIC) <- "X-squared"
    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME),
              class = "htest")
}

PP.test <- function (x, lshort = TRUE)
{
    if (NCOL(x) > 1)
        stop ("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    z <- embed (x, 2)
    yt <- z[,1]
    yt1 <- z[,2]
    n <- length (yt)
    tt <- (1L:n)-n/2
    res <- lm (yt~1+tt+yt1)
    if (res$rank < 3)
        stop ("singularities in regression")
    res.sum <- summary (res)
    tstat <- (res.sum$coefficients[3,1]-1)/res.sum$coefficients[3,2]
    u <- residuals (res)
    ssqru <- sum(u^2)/n
    l <- if (lshort) trunc(4*(n/100)^0.25) else trunc(12*(n/100)^0.25)
    ssqrtl <- ssqru + .Call(C_pp_sum, u, l)
    n2 <- n^2
    trm1 <- n2*(n2-1)*sum(yt1^2)/12
    trm2 <- n*sum(yt1*(1L:n))^2
    trm3 <- n*(n+1)*sum(yt1*(1L:n))*sum(yt1)
    trm4 <- (n*(n+1)*(2*n+1)*sum(yt1)^2)/6
    Dx <- trm1-trm2+trm3-trm4
    STAT <- sqrt(ssqru)/sqrt(ssqrtl)*tstat-(n^3)/(4*sqrt(3)*sqrt(Dx)*sqrt(ssqrtl))*(ssqrtl-ssqru)
    table <- cbind(c(4.38,4.15,4.04,3.99,3.98,3.96),
                   c(3.95,3.80,3.73,3.69,3.68,3.66),
                   c(3.60,3.50,3.45,3.43,3.42,3.41),
                   c(3.24,3.18,3.15,3.13,3.13,3.12),
                   c(1.14,1.19,1.22,1.23,1.24,1.25),
                   c(0.80,0.87,0.90,0.92,0.93,0.94),
                   c(0.50,0.58,0.62,0.64,0.65,0.66),
                   c(0.15,0.24,0.28,0.31,0.32,0.33))
    table <- -table
    tablen <- dim(table)[2L]
    tableT <- c(25,50,100,250,500,100000)
    tablep <- c(0.01,0.025,0.05,0.10,0.90,0.95,0.975,0.99)
    tableipl <- numeric(tablen)
    for (i in (1L:tablen))
        tableipl[i] <- approx (tableT,table[,i],n,rule=2)$y
    PVAL <- approx (tableipl,tablep,STAT,rule=2)$y
    PARAMETER <- l
    METHOD <- "Phillips-Perron Unit Root Test"
    names(STAT) <- "Dickey-Fuller"
    names(PARAMETER) <- "Truncation lag parameter"
    structure(list(statistic = STAT, parameter = PARAMETER,
                   p.value = PVAL, method = METHOD, data.name = DNAME),
              class = "htest")
}
