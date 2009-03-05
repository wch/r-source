#
# ${R_HOME}/bin/INSTALL for installing add-on packages

## quote each argument here, unquote in R code.
args=
while test -n "${1}"; do
  args="${args}nextArg${1}"
  shift
done

## NB: Apple's ICU needs LC_COLLATE set when R is started.
echo 'tools:::.install_packages()' | R_DEFAULT_PACKAGES= LC_COLLATE=C "${R_HOME}/bin/R" --no-restore --slave --args ${args}
