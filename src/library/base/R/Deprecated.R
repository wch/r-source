###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""), call. = FALSE)
}

## consider keeping one (commented) entry here, for easier additions

## <entry>
## Deprecated in 1.6.0
machine <- function()
{
    .Deprecated(".Platform$OS.type")
    .Internal(machine())
}

Machine <- function()
{
    .Deprecated(".Machine")
    get(".Machine", "package:base")
}

Platform <- function()
{
    .Deprecated(".Platform")
    get(".Platform", "package:base")
}

restart<-function (on = TRUE){
    .Deprecated("try")
    .Internal(restart(on))
}

## </entry>

