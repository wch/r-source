source <-
    function(file, local=FALSE, echo = verbose, print.eval=echo,
             verbose= .Options$verbose, prompt.echo = .Options$prompt,
             max.deparse.length=150)
{
    envir <- if (local) sys.frame(sys.parent()) else .GlobalEnv
    if(!missing(echo)) {
        if(!is.logical(echo)) stop("echo must be logical")
        if(!echo && verbose) {
            warning("verbose is TRUE, echo not; ... coercing 'echo <- TRUE'")
            echo <- TRUE
        }
    }
    if(verbose) { cat("'envir' chosen:"); print(envir) }
    Ne <- length(exprs <- parse(n = -1, file = file))
    if(verbose)
	cat("--> parsed", Ne, "expressions; now eval(.)ing them:\n")
    if (Ne == 0) return(invisible())
    ass1 <- expression(y <- x)[[1]][[1]] #-- ass1 :  the  '<-' symbol/name
    if(echo) {
        ## Reg.exps for string delimiter/ NO-string-del / odd-number-of-str.del
        ## needed, when truncating below
        sd <- "\""; nos <- "[^\"]*"
        oddsd <- paste("^",nos,sd,"(",nos,sd,nos,sd,")*",nos,"$", sep="")
    }
    for (i in 1:Ne) {
	if(verbose)
	    cat("\n>>>> eval(expression_nr.",i,")\n\t  =================\n")
	ei <- exprs[i]
	if(echo) {
	    dep <- substr(paste(deparse(ei), collapse="\n"),
			  12, 1e6)# drop "expression("
	    nd <- nchar(dep) -1 # -1: drop ")"
	    do.trunc <- nd > max.deparse.length
            dep <- substr(dep, 1, if(do.trunc)max.deparse.length else nd)
            cat("\n", prompt.echo, dep,
                if(do.trunc)
                paste(if(length(grep(sd,dep)) && length(grep(oddsd,dep)))
                      " ...\" ..." else " ....", "[TRUNCATED] "),
                "\n", sep="")
	}
	yy <- eval(ei, envir)
	i.symbol <- mode(ei[[1]]) == "name"
	if(!i.symbol) {
	    curr.fun <- ei[[1]][[1]]## ei[[1]] : the function "<-" or other
	    if(verbose) { cat('curr.fun:'); str(curr.fun) }
	}
	if(verbose >= 2) {
	    cat(".... mode(ei[[1]])=", mode(ei[[1]]),"; paste(curr.fun)=");
	    str(paste(curr.fun))
	}
	if(print.eval &&
	   (i.symbol|| (length(pf <- paste(curr.fun))==1 &&
			all(paste(curr.fun) != c("<-","cat", "str", "print")))))
	    print(yy)
	if(verbose) cat(" .. after `", deparse(ei), "'\n", sep="")
    }
    invisible(yy)
}

sys.source <- function (file)
{
    exprs <- parse(n = -1, file = file)
    if (length(exprs) == 0) return(invisible())
    for (i in exprs) {
	yy <- eval(i, NULL)
    }
    invisible(yy)
}

demo <- function(topic, device = x11, directory.sep = "/")
{
    Topics <-cbind(graphics = c("graphics","graphics.R",	"G"),
		   image	 = c("graphics","image.R",	"G"),
		   lm.glm	 = c("models",	"lm+glm.R",	"G"),
		   glm.vr	 = c("models",	"glm-v+r.R",	""),
		   nlm	 = c("nlm",	"valley.R",	""),
		   recursion= c("language","recursion.R",	"G"),
		   scoping	 = c("language","scoping.R",	""),
		   is.things= c("language","is-things.R",	"")
		   )
    dimnames(Topics)[[1]] <- c("dir", "file", "flag")
    topic.names <- dimnames(Topics)[[2]]
    demo.help <- function() {
	cat("Use ``demo(topic)'' where choices for argument `topic' are:\n")
	cbind(topics = topic.names)
    }
    if(missing(topic)) return(demo.help())
    topic <- substitute(topic)
    if (!is.character(topic)) topic <- deparse(topic)[1]
    i.top <- pmatch(topic, topic.names)
    if (is.na(i.top) || i.top == 0) {
	cat("unimplemented `topic' in demo.\n")
	print(demo.help())
	stop()
    } else {
	topic <- topic.names[i.top]
	cat("\n\n\tdemo(",topic,")\n\t---- ",rep("~",nchar(topic)),
	    "\n\nType  <Return>	 to start : ",sep="")
	readline()
	if(dev.cur()<=1 && Topics["flag",i.top] == "G")
	    device()
	source(paste(getenv("RHOME"),
		     "demos",
		     Topics["dir",  i.top],
		     Topics["file", i.top], sep= directory.sep),
	       echo = TRUE, max.deparse.length=10000)
    }
}

example <- function(topic, package= .packages(), lib.loc = .lib.loc,
                    echo = TRUE, verbose = .Options$verbose,
                    prompt.echo = paste(abbreviate(topic, 6),"> ", sep=""),
                    directory.sep = "/")
{
    topic <- substitute(topic)
    if (!is.character(topic)) topic <- deparse(topic)[1]

    for (lib in lib.loc)
        for (pkg in package) {
            file <- system.file(paste("R-ex",directory.sep,topic,".R", sep=""),
                                pkg = pkg, lib = lib)
            if(file != "") break
        }
    if(file == "") stop(paste("Couldn't find '", topic, "' example", sep=""))
    if(pkg != "base") library(pkg, lib = lib, character.only = TRUE)
    source(file, echo = echo, prompt.echo = prompt.echo,
           verbose = verbose, max.deparse.length=10000)
}

