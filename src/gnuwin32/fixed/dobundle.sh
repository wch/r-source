
# get the full path names to all packages contained in $1
  if grep -s "^Contains:" "${2}/${1}/DESCRIPTION"; then
    BUNDLEPKG=`grep "^Contains:" "${2}/${1}/DESCRIPTION" | \
      sed "s/^Contains://; s/,/ /g"`
    for p in ${BUNDLEPKG}; do
      if test -f "$2}/${1}/${p}/DESCRIPTION.in"; then
        cat "${2}/${1}/${p}/DESCRIPTION.in"  > "${2}/${1}/${p}/DESCRIPTION"
        grep -v "^Contains:" "${2}/${1}/DESCRIPTION" >> "${2}/${1}/${p}/DESCRIPTION"
      fi
      echo "------- installing package ${p} for bundle ${1} --------"
      make PKGDIR=${2}/${1} RLIBS=${3} pkg-${p}
    done
  else 
      echo "not a bundle"
      exit 1
  fi

