## fft():
options(digits=7)
for(n in 1:30) cat("\nn=",n,":", round(fft(1:n), 8),"\n")
