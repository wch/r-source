revision='$Rev$'
version=`set - ${revision}; echo ${2}`
version="Sweave front-end r${version}

Copyright (C) 2006 The R Core Development Team.
This is free software; see the GNU General Public Licence version 2
or later for copying conditions.  There is NO warranty."

usage="Usage: R CMD Sweave file

A simple front-end for Sweave()

Options:
  -h, --help		print short help message and exit
  -v, --version		print Sweave version info and exit

Report bugs to <r-bugs@r-project.org>."

case ${1} in
  -h|--help)
     echo "${usage}"; exit 0 ;;
  -v|--version)
     echo "${version}"; exit 0 ;;
esac

R_EXE="${R_HOME}/bin/rterm.exe"
echo "library(\"utils\"); Sweave(\"$1\")" | \
  "${R_EXE}" --no-restore --slave
