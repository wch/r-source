### Regression tests for which the printed output is the issue

## PR 715 (Printing list elements w/attributes)
##
l <- list(a=10)
attr(l$a, "xx") <- 23
l
## Comments:
## should print as
# $a:
# [1] 10
# attr($a, "xx"):
# [1] 23

## On the other hand
m <- matrix(c(1, 2, 3, 0, 10, NA), 3, 2)
na.omit(m)
## should print as
#      [,1] [,2]
# [1,]    1    0
# [2,]    2   10
# attr(,"na.action")
# [1] 3
# attr(,"na.action")attr(,"class")
# [1] "omit"

## and
x <- 1
attr(x, "foo") <- list(a="a")
x
## should print as
# [1] 1
# attr(,"foo")
# attr(,"foo")$a
# [1] "a"


## PR 746 (printing of lists)
##
test.list <- list(A = list(formula=Y~X, subset=TRUE),
                  B = list(formula=Y~X, subset=TRUE))

test.list
## Comments:
## should print as
# $A
# $A$formula
# Y ~ X
#
# $A$subset
# [1] TRUE
#
#
# $B
# $B$formula
# Y ~ X
#
# $B$subset
# [1] TRUE

## Marc Feldesman 2001-Feb-1.  Precision in summary.data.frame & *.matric
data(attenu)
summary(attenu)
summary(attenu, digits = 5)
summary(data.matrix(attenu), digits = 5)# the same for matrix
## Comments:
## No difference between these in 1.2.1 and earlier
set.seed(1)
x <- c(round(runif(10), 2), 10000)
summary(x)
summary(data.frame(x))
## Comments:
## All entries show all 3 digits after the decimal point now.

## Chong Gu 2000-Feb-16.  step on binomials
"detg1" <-
structure(list(Temp = structure(c(2, 1, 2, 1, 2, 1, 2, 1, 2,
1, 2, 1), .Label = c("High", "Low"), class = "factor"), M.user = structure(c(1,
1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2), .Label = c("N", "Y"), class = "factor"),
    Soft = structure(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3), .Label = c("Hard",
    "Medium", "Soft"), class = "factor"), M = c(42, 30, 52, 43,
    50, 23, 55, 47, 53, 27, 49, 29), X = c(68, 42, 37, 24, 66,
    33, 47, 23, 63, 29, 57, 19)), .Names = c("Temp", "M.user",
"Soft", "M", "X"), class = "data.frame", row.names = c("1", "3",
"5", "7", "9", "11", "13", "15", "17", "19", "21", "23"))
detg1.m0 <- glm(cbind(X,M)~1,binomial,detg1)
detg1.m0
step(detg1.m0,scope=list(upper=~M.user*Temp*Soft))

## PR 829 (empty values in all.vars)
## This example by Uwe Ligges <ligges@statistik.uni-dortmund.de>

temp <- matrix(1:4, 2)
all.vars(temp ~ 3) # OK
all.vars(temp[1, ] ~ 3) # wrong in 1.2.1

## 2001-Feb-22 from David Scott.
## rank-deficient residuals in a manova model.
gofX.df<-
  structure(list(A = c(0.696706709347165, 0.362357754476673,
-0.0291995223012888,
0.696706709347165, 0.696706709347165, -0.0291995223012888, 0.696706709347165,
-0.0291995223012888, 0.362357754476673, 0.696706709347165, -0.0291995223012888,
0.362357754476673, -0.416146836547142, 0.362357754476673, 0.696706709347165,
0.696706709347165, 0.362357754476673, -0.416146836547142, -0.0291995223012888,
-0.416146836547142, 0.696706709347165, -0.416146836547142, 0.362357754476673,
-0.0291995223012888), B = c(0.717356090899523, 0.932039085967226,
0.999573603041505, 0.717356090899523, 0.717356090899523, 0.999573603041505,
0.717356090899523, 0.999573603041505, 0.932039085967226, 0.717356090899523,
0.999573603041505, 0.932039085967226, 0.909297426825682, 0.932039085967226,
0.717356090899523, 0.717356090899523, 0.932039085967226, 0.909297426825682,
0.999573603041505, 0.909297426825682, 0.717356090899523, 0.909297426825682,
0.932039085967226, 0.999573603041505), C = c(-0.0291995223012888,
-0.737393715541246, -0.998294775794753, -0.0291995223012888,
-0.0291995223012888, -0.998294775794753, -0.0291995223012888,
-0.998294775794753, -0.737393715541246, -0.0291995223012888,
-0.998294775794753, -0.737393715541246, -0.653643620863612, -0.737393715541246,
-0.0291995223012888, -0.0291995223012888, -0.737393715541246,
-0.653643620863612, -0.998294775794753, -0.653643620863612,
-0.0291995223012888,
-0.653643620863612, -0.737393715541246, -0.998294775794753),
    D = c(0.999573603041505, 0.67546318055115, -0.0583741434275801,
    0.999573603041505, 0.999573603041505, -0.0583741434275801,
    0.999573603041505, -0.0583741434275801, 0.67546318055115,
    0.999573603041505, -0.0583741434275801, 0.67546318055115,
    -0.756802495307928, 0.67546318055115, 0.999573603041505,
    0.999573603041505, 0.67546318055115, -0.756802495307928,
    -0.0583741434275801, -0.756802495307928, 0.999573603041505,
    -0.756802495307928, 0.67546318055115, -0.0583741434275801
    ), groups = structure(c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
    2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3), class = "factor", .Label = c("1",
    "2", "3"))), .Names = c("A", "B", "C", "D", "groups"), row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
"14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"
), class = "data.frame")

gofX.manova <- manova(formula = cbind(A, B, C, D) ~ groups, data = gofX.df)
try(summary(gofX.manova))
## should fail with error message


## summary.matrix(*, digits) 
