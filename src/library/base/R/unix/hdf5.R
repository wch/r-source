# Don't wrap hdf5save yet.  It doesn't work ...
# hdf5save <- function(file, ...) .Internal(hdf5save(file, ...))

hdf5load <- function(file, load=TRUE) .Internal(hdf5load(file, load))
