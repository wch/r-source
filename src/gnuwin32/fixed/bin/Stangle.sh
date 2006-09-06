R_EXE="${R_HOME}/bin/rterm.exe"
echo "library(\"utils\"); Stangle(\"$1\")" | \
  "${R_EXE}" --no-save --no-restore --quiet
