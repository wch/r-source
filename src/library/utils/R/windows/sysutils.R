memory.size <- function(max = FALSE) .Internal(memory.size(max))

memory.limit <- function(size = NA) .Internal(memory.size(size))

DLL.version <- function(path) .Internal(DLL.version(path))

select.list <- function(list, preselect=NULL, multiple=FALSE)
    .Internal(select.list(list, preselect, multiple))

flush.console <- function() .Internal(flush.console())

readClipboard <- function()
    strsplit(.Internal(readClipboard()), "\r\n")[[1]]


writeClipboard <- function(str)
    .Internal(writeClipboard(str))

