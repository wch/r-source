autoload <- function (name, file)
{
	if (exists(name,envir=.GlobalEnv,inherits=FALSE))
        	stop("Object already exists")
        newcall <- paste("delay(autoloader(\"", name, "\",\"", file, "\"))",
                sep = "")
	if (is.na(match(file,.Autoloaded)))
  	        assign(".Autoloaded",c(file,.Autoloaded),env=.AutoloadEnv)
        assign(name, parse(text = newcall), env = .AutoloadEnv)
}
autoloader <- function (name, file)
{
	name<-paste(name,"",sep="")
	rm(list=name,envir=.AutoloadEnv,inherits=FALSE)
        where <- length(search)
        eval(parse(text = paste("library(\"", file, "\")", sep = "")),
                .GlobalEnv)
	autoload(name,file)
        where <- length(search) - where + 2
 	if (exists(name,where=where,inherits=FALSE))
           eval(as.name(name), pos.to.env(where))
	else
	   stop(paste("autoloader didn't find `",name,"' in `",file,"'.",sep=""))
}


