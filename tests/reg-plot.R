#### Regression tests for GRAPHICS & PLOTS

postscript("reg-plot.ps")
## consider doing an ``approximate'' (AFM fonts!) diff  reg*.ps  reg*.ps.save

options(warn = 1)# print them as they occur

plot(0) # this should remain constant
str(par(c("usr","xaxp","yaxp")))


## PR 390 (axis for small ranges)

relrange <- function(x) {
    ## The relative range in EPS units
    r <- range(x)
    diff(r)/max(abs(r))/.Machine$double.eps
}

x <- c(0.12345678912345678,
       0.12345678912345679,
       0.12345678912345676)
relrange(x) ## 1.0125
plot(x) # `extra horizontal' ;  +- ok on Solaris; label off on Linux

y <- c(0.9999563255363383973418,
       0.9999563255363389524533,
       0.9999563255363382863194)
## The relative range number:
relrange(y) ## 3.000131
plot(y)# once gave infinite loop on Solaris [TL];  y-axis too long

## Comments: The whole issue was finally deferred to main/graphics.c l.1944
##    error("relative range of values is too small to compute accurately");
## which is not okay.

set.seed(101)
par(mfrow = c(3,3))
for(j.fac in 1e-12* c(10, 1, .7, .3, .2, .1, .05, .03, .01)) {
##           ====
    #set.seed(101) # or don't
    x <- pi + jitter(numeric(101), f = j.fac)
    rrtxt <- paste("rel.range =", formatC(relrange(x), dig = 4),"* EPS")
    cat("j.f = ", format(j.fac)," ;  ", rrtxt,"\n",sep="")
    plot(x, type = "l", main = rrtxt)
    cat("par(\"usr\")[3:4]:", formatC(par("usr")[3:4], wid = 10),"\n",
        "par(\"yaxp\") :   ", formatC(par("yaxp"), wid = 10),"\n\n", sep="")
}
## The warnings from inside GScale() will differ in their  relrange() ...
## >> do sloppy testing

par(mfrow = c(1,1))

### Test for centring of chars.  All the chars which are plotted should
### be centred, and there should be no warnings about
### font metrics unknown for character `?'

par(pty="s")
plot(c(0,15), c(0,15), type="n", xlab="", ylab="")
title("Centred chars in default char set (ISO Latin1)")
grid(15, 15, lty=1)
for(i in c(32:126, 144:152, 154, 155, 157:255)) {
    x <- i %% 16
    y <- i %/% 16
    points(x, y, pch=i)
}

par(pty="m")

## PR 816 (label sizes in dotchart)

### Prior to 1.2.2, the label sizes were unaffected by cex.

data(VADeaths)
dotchart(VADeaths, main = "Death Rates in Virginia - 1940", cex = 0.5)
dotchart(VADeaths, main = "Death Rates in Virginia - 1940", cex = 1.5)

## killed by 0 prior to 1.4.0 and in 1.4.1:
t1 <- ts(0:100)
## only warnings about values <= 0
plot(t1, log = "y")
plot(cbind(t1, 10*t1, t1 - 4), log="y", plot.type = "single")
stopifnot(par("usr")[4] > 3) # log10: ylim[2] = 1000


## This one needs to be looked at.
## lty = "blank" killed the fill colour too.
plot(1:10, type="n")
polygon(c(1, 3, 3, 1), c(1, 1, 3, 3), col="yellow", border="red", lty="blank")
rect(6, 6, 10, 10,  col="blue", border="red", lty="blank")
## in 1.5.0 all omit the fill colours.
data(trees)
with(trees, symbols(Height, Volume, circles=Girth/24, inches=FALSE,
                    lty="blank", bg="blue"))
## in 1.5.0 ignored the lty.

## axis() and par(mgp < 0) {keep this example S+ compatible!}:
lt <- if(is.R()) "31" else 2
x <- seq(-2,3, len=1001)
op <- par(tck= +0.02, mgp = -c(3,2,0))
plot(x, x^2 - 1.2, xaxt = "n", xlab="", type ='l', col = 2,
     main = "mgp < 0: all ticks and labels inside `frame'")
x <- -2:3
lines(x, x^2 - 1.2, type ="h", col = 3, lwd=3)
axis(1, pos = 0, at=-1:1, lty = lt, col=4)## col & lty work only from R 1.6
par(op)
axis(1, pos = 0, at=c(-2,2,3), lty = lt, col=4)
mtext(side=1,"note the x-ticks on the other side of the bars")

## explicit xlab and ylab for non-1D  plot.table():
data(UCBAdmissions)
plot(UCBAdmissions)# default x- and y-lab
plot(UCBAdmissions, xlab = "x label", ylab = "YY")# wrong in 1.5.1
