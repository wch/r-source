source <-
  function (file, local = FALSE, echo = verbose, print.eval = echo,
            verbose = .Options$verbose, prompt.echo = .Options$prompt,
            max.deparse.length = 150, chdir = FALSE)
{
    eval.with.vis <-
        function (expr, envir = parent.frame(),
		  enclos = if (is.list(envir) || is.pairlist(envir))
                  parent.frame())
        .Internal(eval.with.vis(expr, envir, enclos))

    envir <- if (local)
        sys.frame(sys.parent())
    else .GlobalEnv
    if (!missing(echo)) {
        if (!is.logical(echo))
            stop("echo must be logical")
        if (!echo && verbose) {
            warning("verbose is TRUE, echo not; ... coercing `echo <- TRUE'")
            echo <- TRUE
        }
    }
    if (verbose) {
        cat("`envir' chosen:")
        print(envir)
    }
    Ne <- length(exprs <- parse(n = -1, file = file))
    if (verbose)
        cat("--> parsed", Ne, "expressions; now eval(.)ing them:\n")
    if (Ne == 0)
        return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
        owd <- getwd()
        on.exit(setwd(owd))
        setwd(path)
    }
    #-- ass1 :  the  `<-' symbol/name
    ass1 <- expression(y <- x)[[1]][[1]]
    if (echo) {
        ## Reg.exps for string delimiter/ NO-string-del / odd-number-of-str.del
        ## needed, when truncating below
        sd <- "\""
        nos <- "[^\"]*"
        oddsd <- paste("^", nos, sd, "(", nos, sd, nos, sd, ")*",
                       nos, "$", sep = "")
    }
    for (i in 1:Ne) {
        if (verbose)
            cat("\n>>>> eval(expression_nr.", i, ")\n\t  =================\n")
        ei <- exprs[i]
        if (echo) {
            # drop "expression("
            dep <- substr(paste(deparse(ei), collapse = "\n"),
                          12, 1e+06)
            # -1: drop ")"
            nd <- nchar(dep) - 1
            do.trunc <- nd > max.deparse.length
            dep <- substr(dep, 1, if (do.trunc)
                          max.deparse.length
                          else nd)
            cat("\n", prompt.echo, dep, if (do.trunc)
                paste(if (length(grep(sd, dep)) && length(grep(oddsd,
                                                               dep)))
                      " ...\" ..."
                      else " ....", "[TRUNCATED] "), "\n", sep = "")
        }
        yy <- eval.with.vis(ei, envir)
        i.symbol <- mode(ei[[1]]) == "name"
        if (!i.symbol) {
            ## ei[[1]] : the function "<-" or other
            curr.fun <- ei[[1]][[1]]
            if (verbose) {
                cat("curr.fun:")
                str(curr.fun)
            }
        }
        if (verbose >= 2) {
            cat(".... mode(ei[[1]])=", mode(ei[[1]]), "; paste(curr.fun)=")
            str(paste(curr.fun))
        }
        if (print.eval && yy$visible)
            print(yy$value)
        if (verbose)
            cat(" .. after `", deparse(ei), "'\n", sep = "")
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

demo <- function(topic, device = .Options$device)
{
    if (is.character(device)) device <- get(device)
    Topics <-cbind(graphics	= c("graphics", "graphics.R",	"G"),
		   image	= c("graphics", "image.R",	"G"),
		   lm.glm	= c("models",	"lm+glm.R",	"G"),
		   glm.vr	= c("models",	"glm-v+r.R",	""),
		   nlm		= c("nlm",	"valley.R",	""),
		   recursion	= c("language", "recursion.R",	"G"),
		   scoping	= c("language", "scoping.R",	""),
		   is.things	= c("language", "is-things.R",	""),
		   dyn.load	= c("dynload",	"zero.R",	"")
		   )
    dimnames(Topics)[[1]] <- c("dir", "file", "flag")
    topic.names <- dimnames(Topics)[[2]]
    demo.help <- function() {
	cat("Use `demo(topic)' where choices for argument `topic' are:\n")
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
	source(file.path(R.home(),
		     "demos",
		     Topics["dir",  i.top],
		     Topics["file", i.top]),
	       echo = TRUE, max.deparse.length=250)
    }
}

example <-
function (topic, package = .packages(), lib.loc = .lib.loc, echo = TRUE,
          verbose = .Options$verbose,
          prompt.echo = paste(abbreviate(topic, 6), "> ", sep = ""))
{
    topic <- substitute(topic)
    if (!is.character(topic))
        topic <- deparse(topic)[1]
    INDICES <- system.file(pkg = package, lib = lib.loc)
    file <- index.search(topic, INDICES, "AnIndex", "R-ex")
    if (file == "") {
        warning(paste("No help file found for `", topic, "'", sep = ""))
        return(invisible())
    }
    comp <- strsplit(file, .Platform$file.sep)[[1]]
    pkg <- comp[length(comp) - 2]
    if(length(file) > 1)
        warning(paste("More than one help file found: using package", pkg))
    lib <- sub(file.path("", pkg, "R-ex", ".*\\.R"), "", file[1])
    ## experimental code
    zfile <- zip.file.extract(file, "Rex.zip")
    if(zfile != file) on.exit(unlink(zfile))
    ## end of experimental code
    if (!file.exists(zfile)) {
        warning(paste("`", topic, "' has a help file but no examples file",
                      sep = ""))
        return(invisible())
    }
    if (pkg != "base")
        library(pkg, lib = lib, character.only = TRUE)
    source(zfile, echo = echo, prompt.echo = prompt.echo, verbose =
           verbose, max.deparse.length = 250)
}
