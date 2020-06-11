
.setMask <- function(mask, ref) {
    .External(C_setMask, mask, ref)
}
