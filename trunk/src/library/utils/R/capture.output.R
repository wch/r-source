"capture.output" <-
  function(..., file=NULL, append=FALSE)
{
    args<-substitute(list(...))[-1]

    if (is.null(file)){
      file<-textConnection("rval",ifelse(append,"a","w"), local=TRUE)
      sink(file)
      on.exit({sink();close(file)})
    }else if (inherits(file,"connection")){
	rval<-invisible(NULL)
	if (!isOpen(file)){
	  open(file,ifelse(append,"a","w"))
	  sink(file)
	  on.exit({sink();close(file)})
	} else{
	   sink(file)
	   on.exit(sink())
	}
    } else {
      file <- file(file, ifelse(append,"a","w"))
      rval <- invisible(NULL)
      sink(file)
      on.exit({sink();close(file)})
    }

    pf<-parent.frame()
    evalVis<-function(expr)
      .Internal(eval.with.vis(expr, pf, baseenv()))

    for(i in seq(length=length(args))){
      expr<-args[[i]]
      if(mode(expr)=="expression")
        tmp<-lapply(expr, evalVis)
      else if (mode(expr)=="call")
        tmp<-list(evalVis(expr))
      else if (mode(expr)=="name")
          tmp<-list(evalVis(expr))
      else stop("bad argument")

      for(item in tmp){
        if (item$visible)
          print(item$value)
      }
    }
    rval
  }
