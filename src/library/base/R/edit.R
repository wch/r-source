edit <- function(name=NULL, file="", editor=options()$editor)
    .Internal(edit(name,file, editor))

vi <- function(name=NULL, file="") edit(name, file, editor="vi")

emacs <- function(name=NULL, file="") edit(name, file, editor="emacs")

xemacs <- function(name=NULL, file="") edit(name, file, editor="xemacs")

xedit <- function(name=NULL, file="") edit(name, file, editor="xedit")

pico <- function(name=NULL, file="") edit(name, file, editor="pico")

