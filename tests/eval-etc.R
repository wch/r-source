##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- To: r-core
##- Date: 22 Jan 1999 11:47
##-   ...
##- This is what didn't work before:

e <- quote(c(F=1,"tail area"=pf(1,1,1)))
e

eval(e)
names(e)

names(e)[2] <- "Variance ratio"
e
eval(e)
