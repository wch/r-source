memory.size <- function(max = FALSE)
{
    warning("'memory.size()' is Windows-specific", call.=FALSE)
    Inf
}

memory.limit <- function(size = NA)
{
   warning("'memory.limit()' is Windows-specific", call.=FALSE)
   Inf
}
