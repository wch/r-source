"?" <- function(e1, e2)
{
  if(is.name(substitute(e1)))
    e1 <- substitute(e1)
  e1 <- as.character(e1)
  if(nargs() >= 2) {
    if(is.name(substitute(e2)))
      e2 <- substitute(e2)
    e2 <- as.character(e2)
    topic <- topicName(e1, e2)
    opts <- options(error= function()TRUE, show.error.messages = FALSE)
    on.exit(options(opts))
    doHelp <- try(eval(substitute(help(TOPIC), list(TOPIC = topic))))
    if(inherits(doHelp, "try-error")) {
        options(opts)
        on.exit()
        stop(paste("no documentation of type \"", e1,
                   "\" and topic \"", e2,
                   "\" (or error in processing help)", sep=""))
    }
  }
  else
      eval(substitute(help(TOPIC), list(TOPIC = e1)))
}

topicName <- function(type, topic) {
    if((length(type) == 0) || (length(topic) == 0))
        character(0)
    else
        paste(paste(topic, collapse = ","), type, sep = "-")
}
