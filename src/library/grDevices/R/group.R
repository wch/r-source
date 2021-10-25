
.defineGroup <- function(source, op, destination) {
    .External(C_defineGroup, source, as.integer(op), destination)
}

.useGroup <- function(ref, trans) {
    .External(C_useGroup, ref, trans)
}

.devUp <- function() {
    .External(C_devUp)
}
