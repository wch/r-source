####
####
#### to be run as
####
####	R < print-tests.R  >&  print-tests.out-__version__

options(echo = T, warn = 1)#-- for Splus
print(search())#---- for Splus

DIG <- function(d) if(missing(d)) options('digits')$dig else
                                options(digits=as.integer(d))

.Options$digits <- 7; DIG(7)#-- the default; just to make sure ...

## ?print.default   in  S-plus  (3.4)  contains

##?p> digits:   the number of significant digits that should be printed
##?p>        in  numeric  data.   Since  all  numbers in any vector are
##?p>        printed in the  same  format,  this  may  mean  that  some
##?p>        numbers  will be printed with more than digits significant
##?p>        digits.  Use digits=17 to print  all  of  the  significant
##?p>        digits  of  a double precision number.  If the argument is
##?p>        omitted, the digits option is used; see options.



n1 <- 2^(4*1:7)
i1 <- as.integer(n1)

v1 <- 2^c(-12, 2*(-4:-2),3,6,9)
v2 <- v1^(63/64)

v3 <- pi*100^(-1:3)
v4 <- (0:2)/1000 + 1e-10 #-- tougher one

digs1 <- c(1,2*(1:5),11:16,20)# ,30 gives 'warning' everytime !
digs2 <- c(1:20)#,30) gives 'error' in R  (should give WARNING!)

all(i1 == n1)# TRUE
i1# prints nicely
n1# does not

round(v3,3)#S+ & R 0.49:
##[1]    0.031       3.142     314.159   31415.927 3141592.654
signif(v3, 3)
##R.49:[1] 0.0314       3.1400     314.0000   31400.0000 3140000.0000
#S+    [1] 3.14e-02 3.14e+00 3.14e+02 3.14e+04 3.14e+06

###----------------------------------------------------------------
##- Date: Tue, 20 May 97 17:11:18 +0200

##- From: Martin Maechler <maechler@stat.math.ethz.ch>
##- To: R-devel@stat.math.ethz.ch
##- Subject: R-alpha: print 'problems': print(2^30, digits=12); comments at start of function()
##-
##- Both of these bugs are not a real harm,
##- however, they have been annoying me for too long ... ;-)
##-
##- 1)
print(2^30, digits = 12) #-  exponential form; unnecessarily!

formatC(2^30, digits = 12) #- shows you what you'd want above

## S-plus is okay here; note that the problem also affects
##	paste(.)  & format(.) :

DIG(10); paste(n1); DIG(7)
##-  S-plus gives
##-
##- [1] "16"        "256"       "4096"      "65536"     "1048576"
##- [6] "16777216"  "268435456"

##-  whereas R 0.49 gives
##-
##- [1] "16"              "256"             "4096"            "65536"
##- [5] "1048576"         "1.6777216e+07"   "2.68435456e+08"




## .Options$digits works for  print(.) &  cat(.)
for(i in digs1) { .Options$digits <- i; cat(i,":"); print (v1[-1])}
for(i in digs1) { .Options$digits <- i; cat(i,":", formatC(v1[-1], digits=i, width=8),"\n")}

for(i in digs1) { .Options$digits <- i; cat(i,":"); print (v3)}
for(i in digs1) { .Options$digits <- i; cat(i,":", formatC(v3, digits=i, width=8),"\n")}


for(i in digs1) { cat(i,":");  print(v1, digits=i)}#-R0.50: switches to NON-exp
					# at 14, but should only at 15...
					# S-plus: does not switch at all..
for(i in digs1) { cat(i,":");  print(v1[-1], digits=i)}
					# R 0.50-a1: switches at 10 inst. 11
					# S-plus: does not switch at all..

for(i in digs1) { .Options$digits <- i; cat(i,":", formatC(v2, digits=i, width=8),"\n")}

for(i in digs2) { cat(i,":");  print(v2, digits=i)} #-- exponential all thru
for(i in digs2) { cat(i,":", formatC(v2, digits=i, width=8),"\n")}

.Options$digits <- 7; options(digits = 7)#-- the default; just to make sure ...

N1 <- 10; N2 <- 7; n <- 8
x <- 0:N1
Mhyp <- rbind(phyper(x, N1, N2, n), dhyper(x, N1, N2, n))
Mhyp
##-      [,1]         [,2]       [,3]     [,4]      [,5]      [,6]      [,7]
##- [1,]    0 0.0004113534 0.01336898 0.117030 0.4193747 0.7821884 0.9635952
##- [2,]    0 0.0004113534 0.01295763 0.103661 0.3023447 0.3628137 0.1814068
##-            [,8]       [,9] [,10] [,11]
##- [1,] 0.99814891 1.00000000     1     1
##- [2,] 0.03455368 0.00185109     0     0

m11 <- c(-1,1)
Mm <- pi*outer(m11, 10^(-5:5))
Mm <- cbind(Mm, outer(m11, 10^-(5:1)))
Mm
do.p <- TRUE
do.p <- FALSE
for(di in 1:10) {
  options(digits=di)
  cat(if(do.p)"\n",formatC(di,w=2),":", format.info(Mm),"\n")
  if(do.p)print(Mm)
}
##-- R-0.49 (4/1997)     R-0.50-a1 (7.7.97)
##-  1 : 13 5 0		 1 :  6 0 1
##-  2 :  8 1 1	=	 2 :  8 1 1
##-  3 :  9 2 1	=	 3 :  9 2 1
##-  4 : 10 3 1	=	 4 : 10 3 1
##-  5 : 11 4 1	=	 5 : 11 4 1
##-  6 : 12 5 1	=	 6 : 12 5 1
##-  7 : 13 6 1	=	 7 : 13 6 1
##-  8 : 14 7 1	=	 8 : 14 7 1
##-  9 : 15 8 1	=	 9 : 15 8 1
##- 10 : 16 9 1	=	10 : 16 9 1


##-- Ok now, everywhere
for(d in 1:9) {cat(d,":"); print(v4, digits=d)}
DIG(7)





###------------ Very big and very small  (--> ./signif.R )
umach <- unlist(.Machine)[paste("double.x", c("min","max"), sep='')]
xmin <- umach[1]
xmax <- umach[2]
tx <- unique(outer(-1:1,c(.1,1e-3,1e-7)))# 7 values  (out of 9)
tx <- unique(sort(c(outer(umach,1+tx))))# 11 values  (out of 14)
tx <- tx[is.finite(tx)] #-- all kept
txp <- tx[tx >= 1]#-- Positive exponent -- 4 values
txp
txn <- tx[tx <  1]#-- Negative exponent -- 7 values
txn

##------ Use  Emacs screen width 134 ;  Courier 12 ----
cat("dig|  formatC(signif(txp, d=dig)\n")
for(dig in 1:18)
  cat(formatC(dig,w=2), formatC(signif(txp, d=dig), dig=dig+2, wid=-26),"\n")

cat("dig|  formatC(signif(txn, d=dig)\n")
for(dig in 1:15)
  cat(formatC(dig,w=2), formatC(signif(txn, d=dig), dig=dig+2, wid=-20),"\n")

##-- Testing  'print' / digits :
for(dig in 1:18) { cat("dig=",formatC(dig,w=2),": "); print(signif(txp, d=dig),dig=dig+2) }


##-- Wrong alignment when printing character matrices with  quote = FALSE
m1 <- matrix(letters[1:24],6,4)
m1
noquote(m1)
