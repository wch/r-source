index.search <- function(topic, path, file = "AnIndex", type = "help")
    .Internal(index.search(topic, path, file, .Platform$file.sep, type))
