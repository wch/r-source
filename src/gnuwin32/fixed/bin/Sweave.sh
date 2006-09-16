R_EXE="${R_HOME}/bin/rterm.exe"
echo "library(\"utils\"); Sweave(\"$1\")" | \
  "${R_EXE}" --no-restore --slave
