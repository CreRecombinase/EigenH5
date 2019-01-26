# Process this file with autoconf to produce a configure script.
# author: Nicholas Knoblauch (nwknoblauch@gmail.com)
AC_PREREQ([2.65])
AC_INIT([EigenH5], [1.2], [nwknoblauch@gmail.com])
#AC_CONFIG_SRCDIR([src])


#m4_include([inst/m4/ax_lib_hdf5.m4])
#m4_include([inst/m4/ax_compare_version.m4])
#m4_include([inst/m4/hh_hdf5rlib.m4])


#AX_LIB_HDF5()


dnl if test "$with_hdf5" = "yes"; then
dnl AC_MSG_RESULT([Found hdf5 with version: $HDF5_VERSION])
dnl AX_COMPARE_VERSION([$HDF5_VERSION], [ge], [1.10.2], [hdf5_version_sufficient="yes"], AC_MSG_ERROR([The version of hdf5 installed on your system is not sufficient. Please ensure that at least version 1.10.2 is installed]))
dnl else
dnl   AC_MSG_ERROR([hdf5 does not seem to be installed on your platform.
dnl   Please install the hdf5 library.
dnl   The required HDF5 library files can be installed using conda 'conda install -c crerecombinase r-eigenh5'])
dnl fi


LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`

# mkdir _build
# wget "https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.2/src/hdf5-1.10.2.tar.gz" -o _build/hdf5-1.10.2.tar.gz
# tar -xvzf _build/hdf5-1.10.2.tar.gz -O _build/
# _build/hdf5-1.10.2/configure --enable-build-mode=production --with-default-plugindir=inst/plugin/

#PKG_CHECK_MODULES([blosc]

dnl if test "$hdf5_version_sufficient" = "yes"; then
dnl   AX_COMPARE_VERSION(
dnl 	[$HDF5_VERSION],
dnl 	[ge],
dnl 	[1.10.3],
dnl 	[HDF5_VERSION_USE="1_10_3"])
dnl fi

dnl AC_SUBST(HDF5_VERSION_USE)
dnl AC_SUBST(DEFINE_H5FREE_MEMORY)

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi



dnl if test -z "${BLOSC_LIBS}"; then
dnl     BLOSC_CFLAGS = `echo 'libblosc::pkgconfig("PKG_CFLAGS")'| "${R_HOME}/bin/R" --vanilla --slave`
dnl     BLOSC_LIBS = `echo 'libblosc::pkgconfig("PKG_LIBS_static")' | "${R_HOME}/bin/R" --vanilla --slave`
dnl fi
dnl AC_MSG_RESULT([looking for .pc files in $PKG_CONFIG_PATH])

dnl PKG_CHECK_MODULES([BLOSC], [blosc], [
dnl LIBS="${BLOSC_LIBS} ${LIBS}"
dnl AC_MSG_RESULT([found the blosc package: $BLOSC_LIBS])],[
dnl ])


dnl PKG_CHECK_MODULES([HDF5], [hdf5], [
dnl LIBS="${HDF5_LIBS} ${LIBS}"
dnl AC_MSG_RESULT([found the hdf5 package: $HDF5_LIBS])],[
dnl ])

if test -z "{HDF5_LIBS}"; then
   HDF5_LIBS = `echo 'Rhdf5lib::pkgconfig("PKG_C_LIBS")'|\
    "${R_HOME}/bin/R" --vanilla --slave`
fi

dnl PKG_CHECK_MODULES([LZF], [lzf], [
dnl LIBS="${LZF_LIBS} ${LIBS}"
dnl AC_MSG_RESULT([found the lzf package: LZF_LIBS])],[
dnl AC_MSG_ERROR([unable to find the lzf package])
dnl ])



dnl PKG_CHECK_MODULES([ZSTD], [zstd], [
dnl LIBS="${ZSTD_LIBS} ${LIBS}"
dnl AC_MSG_RESULT([found the zstd package: $ZSTD_LIBS])],[
dnl AC_MSG_ERROR([unable to find the zstd package])
dnl ])

dnl AC_SEARCH_LIBS([H5open], [hdf5], [], [
dnl    AC_MSG_ERROR([hdf5 open function])
dnl    ])

dnl AC_SEARCH_LIBS([H5Oget_info_by_name], [hdf5], [], [
dnl    AC_MSG_ERROR([hdf5 info_by_name])
dnl    ])


dnl if test -z "${BLOSC_LIBS}"; then
dnl    AC_SEARCH_LIBS([blosc_compress], [blosc], [], [
dnl    AC_MSG_ERROR([unable to find the blosc_compress() function])
dnl    ])
dnl fi

dnl if test -z "${ZSTD_LIBS}"; then
dnl AC_SEARCH_LIBS([ZSTD_compress], [zstd], [], [
dnl   AC_MSG_ERROR([unable to find the ZSTD_compress() function])
dnl ])
dnl fi

dnl AC_SEARCH_LIBS([std::filesystem::path::path],[std++fs],[],[
dnl   AC_MSG_ERROR([unable to find the c++ filesystem library])
dnl   ])




AC_MSG_RESULT([Libs will be : $LIBS])
AC_MSG_RESULT([HDF5Libs will be : $HDF5_LIBS])
AC_SUBST(LIBS)
#AC_SUBST(BLOSC_LIBS)
#AC_SUBST(BLOSC_CFLAGS)
AC_SUBST(HDF5_LIBS)
#AC_SUBST(BLOSC_CFLAGS)





dnl if test -z "${LIBS}"; then
dnl   dnl prefer ggrep if installed
dnl   AC_CHECK_PROG(GREP,ggrep,`which ggrep`, `which grep`)

dnl   dnl check if $GREP supports option -o
dnl   AC_MSG_CHECKING([whether ${GREP} accepts -o])
dnl   if `echo '' | ${GREP} -o '' >/dev/null 2>&1`
dnl   then
dnl     AC_MSG_RESULT([yes])
dnl   else
dnl     AC_MSG_RESULT([no])
dnl     AC_MSG_ERROR([${GREP} does not support option -o])
dnl   fi
dnl   [
dnl   LIBS=`${H5CC} -show | ${GREP} -o "\-L[^[:space:]]\+" | tr "\n" " "`
dnl   ]

dnl   dnl Fix for newer Debian versions
dnl   HDF5R_LIBS="${LIBS} -L. -lhdf5_hl -lhdf5 -lz -lm"

dnl fi

dnl if test -z "${CPPFLAGS}"; then
dnl   dnl prefer ggrep if installed
dnl   AC_CHECK_PROG(GREP,ggrep,`which ggrep`, `which grep`)

dnl   dnl check if $GREP supports option -o
dnl   AC_MSG_CHECKING([whether ${GREP} accepts -o])
dnl   if `echo '' | ${GREP} -o '' >/dev/null 2>&1`
dnl   then
dnl     AC_MSG_RESULT([yes])
dnl   else
dnl     AC_MSG_RESULT([no])
dnl     AC_MSG_ERROR([${GREP} does not support option -o])
dnl   fi

dnl   echo 'int main() { return; }' > myconftest.c


dnl   [
dnl   HDF5R_CPPFLAGS=`${H5CC} -show -c myconftest.c | ${GREP} -o "\-I[^[:space:]]\+" | tr "\n" " "`
dnl   ]
dnl   rm -f myconftest.c
dnl fi

dnl dummy substitute HDF5R_CFLAGS
#HDF5R_CFLAGS=""
#AC_SUBST(HDF5R_CFLAGS)

dnl substitute CPPFLAGS and LIBS
#AC_SUBST(HDF5R_CPPFLAGS)
#AC_SUBST(HDF5R_LIBS)
dnl and do subsitution in the src/Makevars.in
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
