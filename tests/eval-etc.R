####  eval / parse / deparse  etc

##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- Subject: Re: source() / eval() bug ??? (PR#96)
##- Date: 20 Jan 1999 14:56:24 +0100
e1 <- parse(text='c(F=(f <- .3), "Tail area" = 2 * if(f < 1) 30 else 90)')[[1]]
e1
str(eval(e1))
mode(e1)

( e2 <- quote(c(a=1,b=2)) )
names(e2)[2] <- "a b c"
e2
parse(text=deparse(e2))

##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- To: r-core
##- Date: 22 Jan 1999 11:47
##-   ...
##- This is what didn't work before:

( e3 <- quote(c(F=1,"tail area"=pf(1,1,1))) )
eval(e3)
names(e3)

names(e3)[2] <- "Variance ratio"
e3
eval(e3)



##- From: Peter Dalgaard BSA <p.dalgaard@biostat.ku.dk>
##- To: r-core
##- Date: 2 Sep 1999

## The first failed in 0.65.0 :
attach(list(x=1))
evalq(dim(x) <- 1,pos.to.env(2))
dput(get("x", env=pos.to.env(2)))

e <- local({x <- 1;environment()})
evalq(dim(x) <- 1,e)
dput(get("x",env=e))

