## Static (i.e., not determined by configure) Make variables to be
## shared between 'Makeconf' and 'etc/Makeconf'.

R_PKGS_BASE = base tools utils grDevices graphics stats datasets methods grid splines stats4 tcltk
R_PKGS_STUBS = ctest eda lqs mle modreg mva nls stepfun ts
# mgcv requires nlme and MASS
R_PKGS_RECOMMENDED_SOURCES = VR boot cluster foreign KernSmooth lattice nlme mgcv rpart survival
R_PKGS_RECOMMENDED = boot cluster foreign KernSmooth lattice nlme mgcv rpart survival MASS class nnet spatial

