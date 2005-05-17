n2mfrow <- function(nr.plots)
{
  if      (nr.plots <=  3)  c(nr.plots,1) # 1, 2, 3
  else if (nr.plots <=  6)  c((nr.plots+1)%/%2,2)#-- n.. = 4,5,6
  else if (nr.plots <= 12)  c((nr.plots+2)%/%3,3)
  else c(nrow <- ceiling(sqrt(nr.plots)),
         ceiling( nr.plots / nrow))
}
