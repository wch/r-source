## Static (i.e., not determined by configure) Make variables to be
## shared *and* grepped from m4/R.m4 and tools/*-recommended.

R_PKGS_BASE = base tools utils grDevices graphics stats datasets methods grid splines stats4 tcltk

R_PKGS_RECOMMENDED =            boot cluster codetools foreign KernSmooth lattice mgcv nlme rpart survival MASS class nnet spatial Matrix
# mgcv requires nlme and MASS, but that is handled in src/library/Recommended/Makefile*
