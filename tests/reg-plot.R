#### Regression tests for GRAPHICS & PLOTS

postscript("reg-plot.ps", paper="a4", horizontal=TRUE,
           encoding ="ISOLatin1.enc")
## since we supply the font metrics, the results depend only on
## the encoding used: Windows is different from Unix by default.
## As from 2.1.0 we only test genuine Latin-1 chars: Adobe has dotlessi
## and accents at 144:152, 154, 155, 157:159, but Unicode does not.

options(warn = 1)# print them as they occur

plot(0) # this should remain constant
str(par(c("usr","xaxp","yaxp")))



### Test for centring of chars.  All the chars which are plotted should
### be centred, and there should be no warnings about
### font metrics unknown for character `?'

par(pty="s")
plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="", xaxs="i", yaxs="i")
title("Centred chars in default char set (ISO Latin1)")
grid(17, 17, lty=1)
known <- c(32:126, 160:255)

for(i in known) {
    x <- i %% 16
    y <- i %/% 16
    points(x, y, pch=i)
}

par(pty="m")

## PR 816 (label sizes in dotchart)

### Prior to 1.2.2, the label sizes were unaffected by cex.

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

## plot.table(): explicit xlab and ylab for non-1D
plot(UCBAdmissions)# default x- and y-lab
plot(UCBAdmissions, xlab = "x label", ylab = "YY")# wrong in 1.5.1
##   axis suppression
plot(tt <- table(c(rep(0,7), rep(1,4), rep(5, 3))), axes = FALSE)
plot(tt, xaxt = "n")
## wrong till (incl.) 1.6.x

## legend with call
lo <- legend(2,2, substitute(hat(theta) == that, list(that= pi)))
stopifnot(length(lo$text$x) == 1)
## length() was 3 till 1.7.x

plot(ecdf(c(1:4,8,12)), ylab = "ECDF", main=NULL)
## ylab didn't work till 1.8.0

plot(1:10, pch = NA) # gave error till 1.9.0
points(1:3, pch=c("o",NA,"x"))# used "N"
try(points(4, pch=c(NA,FALSE)))# still give an error

## 'lwd' should transfer to plot symbols
legend(1,10, c("A","bcd"), lwd = 2:3, pch= 21:22, pt.bg="skyblue",
       col = 2:3, bg = "thistle")
## (gave an error for 2 days in "2.0.0 unstable")
