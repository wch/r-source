"?" <- function(e1, e2, topicSeparator = "_")
{
  if(is.name(substitute(e1)))
    e1 <- substitute(e1)
  e1 <- as.character(e1)
  if(nargs() >= 2) {
    if(is.name(substitute(e2)))
      e2 <- substitute(e2)
    e2 <- as.character(e2)
    e1 <- paste(e1, e2, sep=topicSeparator)
  }
  eval(substitute(help(TOPIC), list(TOPIC = e1)))
}

  
   
