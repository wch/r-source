## [Bug 18476] alpha handling in palette functions  (23 Feb 2023)
##             https://bugs.r-project.org/show_bug.cgi?id=18476
## Attachment 3131 https://bugs.r-project.org/attachment.cgi?id=3131
## and comment #3  by Achim Zeileis


## from attachment #3131 :
check_alpha <- function(colors = "topo.colors", ncolor = 3, nalpha = 3, ...) {
  ## alpha sequence of length nalpha
  alpha <- seq(0, 1, length.out = nalpha)

  ## generate colors with alpha=...
  col1 <- tryCatch(do.call(colors, c(list(n = ncolor, alpha = alpha), list(...))),
                   error = identity)
  if(inherits(col1, "error")) return(FALSE)

  ## generate colors without alpha= and add manually afterwards
  alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
  col2 <- paste0(do.call(colors, c(list(n = ncolor), list(...))),
                 rep_len(alpha, ncolor))

  ## check whether both strategies yield identical output
  identical(col1, col2)
}

expndGrid <- function(...)
          expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

iSamp <- function(n, f=1/4, nS = max(min(n, 24L), f*n), full = interactive())
    if(full) seq_len(n) else sample.int(n, nS)

chkALLalpha <- function(d)
    vapply(iSamp(nrow(d)), function(i) do.call(check_alpha, d[i,]), NA)

## Check old palettes ------------------
d1 <- expndGrid(colors = c("rainbow", "topo.colors", "terrain.colors",
                           "heat.colors", "cm.colors", "gray.colors"),
  ncolor = c(1, 3, 9, 100),
  nalpha = c(2, 3, 9, 100))
table(L <- chkALLalpha(d1)) ## R-4.2.x: 71 FALSE, 25 TRUE -- now 96 TRUE
if(!all(L)) stop("---> not all ok")


## Check the new palettes -----------------

d2 <- expndGrid(colors = "palette.colors",
  ncolor = c(1, 3, 7),
  nalpha = c(2, 3, 7),
  palette = print(palette.pals()))
table(L <- chkALLalpha(d2)) ## R-4.2.x: 64 FALSE, 80 TRUE -- now 144 TRUE
if(!all(L)) stop("---> not all ok")

d3 <- expndGrid(colors = "hcl.colors",
  ncolor = c(1, 3, 9, 100),
  nalpha = c(2, 3, 9, 100),
  palette = print(hcl.pals()))
table(L <- chkALLalpha(d3)) ## R-4.2.x: 1057 FALSE, 783 TRUE -- now 1840 TRUE
if(!all(L)) stop("---> not all ok")
