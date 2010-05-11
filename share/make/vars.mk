## Static (i.e., not determined by configure) Make variables to be
## shared *and* grepped from m4/R.m4 and tools/*-recommended.

R_PKGS_BASE = base tools utils grDevices graphics stats datasets methods grid splines stats4 tcltk

R_PKGS_RECOMMENDED =  MASS lattice Matrix nlme survival boot cluster codetools foreign KernSmooth rpart class nnet spatial mgcv
# there are dependencies in src/library/Recommended/Makefile*
# which this order respects
