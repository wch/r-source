## R_GNOME
## -------
AC_DEFUN([R_GNOME],
[ GNOME_INIT_HOOK([], [cont])
  if test "${GNOMEUI_LIBS}"; then
    AM_PATH_LIBGLADE([use_gnome="yes"
                      GNOME_IF_FILES="gnome-interface.glade"],
                     [warn_libglade_version="GNOME support requires libglade ver
sion >= 0.3"
                      AC_MSG_WARN([${warn_libglade_version}])],
                     [gnome])
  fi
if test "${use_gnome}" != yes; then
  use_gnome="no"
  GNOME_IF_FILES=
fi
AC_SUBST(GNOME_IF_FILES)
])# R_GNOME

## R_PROG_CC_M
## -----------
## Check whether we can figure out C Make dependencies.
AC_DEFUN([R_PROG_CC_M],
[AC_MSG_CHECKING([whether we can compute C Make dependencies])
AC_CACHE_VAL([r_cv_prog_cc_m],
[echo "#include <math.h>" > conftest.c
## No real point in using AC_LANG_* and ${ac_ext}, as we need to create
## hard-wired suffix rules.
## Another obvious candidate to try is '${MAKEDEPEND-makedepend} -f-'.
## However, this does not work out of the box when srcdir and builddir
## are different, as it creates dependencies of the form
##   ${srcdir}/foo.o: /path/to/bar.h
## Could be made to work, of course ...
## Note also that it does not create a 'conftest.o: conftest.c' line.
## For gcc 3.2 or better, we want to use '-MM' in case this works.
cc_minus_MM=false
if test "${GCC}" = yes; then
  gcc_version=`${CC} -v 2>&1 | grep "^.*g.. version" | \
    sed -e 's/^.*g.. version *//'`
  case "${gcc_version}" in
    1.*|2.*|3.[[01]]*) ;;
    *) cc_minus_MM="${CC} -MM" ;;
  esac
fi
for prog in "${cc_minus_MM}" "${CC} -M" "${CPP} -M" "cpp -M"; do
  if ${prog} conftest.c 2>/dev/null | \
      grep 'conftest.o: conftest.c' >/dev/null; then
    r_cv_prog_cc_m="${prog}"
    break
  fi
done])
if test -z "${r_cv_prog_cc_m}"; then
  AC_MSG_RESULT([no])
else
  AC_MSG_RESULT([yes, using ${r_cv_prog_cc_m}])
fi
])# R_PROG_CC_M
