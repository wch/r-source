"?" <- function(e1, e2)
{
  if(is.name(substitute(e1)))
    e1 <- substitute(e1)
  e1 <- as.character(e1)
  if(nargs() >= 2) {
    if(is.name(substitute(e2)))
      e2 <- substitute(e2)
    e2 <- as.character(e2)
    e1 <- topicName(e1, e2)
  }
  eval(substitute(help(TOPIC), list(TOPIC = e1)))
}

topicName <- function(type, topic)
    paste(topic, type, sep = "-")

   
