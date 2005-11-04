#### Which functions in R  are	.Primitive()  / which should be ?
#### ------------------------------------------------------------
#### M.Maechler, May, 1998.
####
#### text was in ../doc/manual/primitive-funs.tex , now
#### ==> ../doc/manual/R-exts.texi "@appendix R (internal) ...
####	 ~~~~~~~~~~~~~~~~~~~~~~~~~

bpos <- match("package:base",search())
nn <- ls(pos=bpos, all = TRUE)
length(nn) # 844 [R 0.62.0, March 25, 1998;  1067 R 0.64.0 March 1999]

is.primitive <- function (obj) is.function(obj) && is.null(args(obj))
is.special <- function(obj) typeof(obj) == "special"

Primf <- nn[sapply(nn, function(N) is.primitive(get(N, pos=bpos)))]
length(Primf) ## 195  R 0.62.0, March 25, 1998
## 132	R 0.62.2+
Real.primitives <-
    list(
	 specials = c("{", "(", "if", "for", "while", "repeat", "break", "next",
	 "return", "function", "quote",
	 "on.exit" ## on.exit(expression, add=F) has two arguments in S-plus !
	 ),
	 language = c("$", "$<-", "@", "<-", "=", "<<-", "[", "[<-", "[[", "[[<-"),
	 arith = c("%%", "%*%", "%/%","*", "+", "-", "/", "^"),
	 logic = c("!",	 "&", "&&",  "|", "||",
	 "<", "<=", "==", ">", ">=", "!="),
	 arithF =
	 c("sign", "abs",
	   "floor", "ceiling", "trunc",
	   "sqrt",
	   "exp",	## NOT: "log",
	   "cos",	  "sin",  "tan",
	   "acos",	 "asin", "atan", ## Not:  atan2
	   "cosh",	 "sinh", "tanh",
	   "acosh","asinh","atanh",
	   "cumsum","cumprod",
	   "cummax", "cummin"
	   ),
	 arithC = c("Arg", "Conj", "Im", "Mod", "Re"),

	 programming =
	 c("nargs", "missing", # actually args(.) could be as formal(.)
	   "interactive",
	   ".Primitive", ".Internal", ".External", ".Call",
           ".External.graphics", ".Call.graphics",
	   ".C", ".Fortran", "symbol.C", "symbol.For",
	   "emptyenv", "baseenv",
	   "globalenv", "pos.to.env", "unclass",
           "as.character", "as.environment",
	   ##maybe ? "gc", "gcinfo",
	   ##
	   "debug", "undebug", ".primTrace", ".primUntrace",
	   "browser",  "proc.time", "gc.time", #"traceback",
	   ),

	 language2= c(":", "~", "c", "list", #nomore (Sep.9,1998): "unlist",
	 # ".Alias", removed for 1.5.0
	 "call", "as.call", "expression", "substitute",
	 "UseMethod", ## ? really ?
         "standardGeneric",
	 "invisible",
	 ),

	 language3=
	 c("environment<-",
	   "length",	"length<-",
	   "class",	"class<-",
	   "attr", # again
	   "attr<-",
	   "attributes",	"attributes<-",
	   "dim",		"dim<-",
	   "dimnames",	"dimnames<-",
	   ## MM:	"comment", "comment<-",
	   ## New data.frame code
	   ##	   "levels", "levels<-",
	   ##	   "codes",  "codes<-",
	   ##	   "[.data.frame",  "[<-.data.frame",
	   ##	   "[[.data.frame", "[[<-.data.frame"
	   ),
	 )
real.primitives <- unlist(Real.primitives)
##names(real.primitives) <- rep("",length(real.primitives))

!any(duplicated(real.primitives)) # TRUE
all(real.primitives %in% Primf)	  # TRUE

"%w/o%" <- function(a,b)  a[! a %in% b]	 ## a without b

prim.f <- Primf %w/o% real.primitives
## see below: contains the is.xxxx(.) funtions
length(prim.f) == length(Primf) - length(real.primitives)# TRUE

Specf <- Primf[iPsp <- sapply(Primf, function(N) is.special(get(N, pos=bpos)))]
length(Specf) ## 36 [ R 0.63 ]
Specf
## the non-"special" ones:
all("builtin" == sapply(Primf[!iPsp], function(N) typeof(get(N, pos=bpos))))


ncpf <- nchar(prim.f)
table(ncpf)
prim.f[ncpf <= 6]#  only is.na, is.nan

( prim.isf <- prim.f[p.isis <- substr(prim.f,1,2) == "is"] )

length(prim.f2 <- prim.f[! p.isis]) # down to 87   [R 0.62.0, March 25, 1998]
					# 61  [March 27]
					# 43  [April 17];  38 [April 22]
##-> 0	[May 1, 1998]

prim.f2 # character(0) --- none left! --

for(n in prim.f2)
    cat(n," <- function(*) .Internal(",n,"(*))\n", sep="")
