
#############################################################################
##
## Copyright 2016 Novartis Institutes for BioMedical Research Inc.
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
## http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
#############################################################################

AC_DEFUN([HH_HDF5RLIB], [

AC_REQUIRE([AC_PROG_SED])
AC_REQUIRE([AC_PROG_AWK])
AC_REQUIRE([AC_PROG_GREP])



with_rhdf5lib="yes"
HDF5R_LIBS=$(echo 'cat(Rhdf5lib::pkgconfig("PKG_C_HL_LIBS"))' | "${R_HOME}/bin/R" --vanilla --slave)
HDF5R_CPPFLAGS=$(echo 'cat("-I",system.file("include/",package = "Rhdf5lib"))' | "${R_HOME}/bin/R" --vanilla --slave)
HDF5_VERSION=$(echo 'cat(Rhdf5lib::getHdf5Version())' | "${R_HOME}/bin/R" --vanilla --slave)
HDF5_MAJOR_VERSION=$(echo $HDF5_VERSION | $AWK -F \. '{print $[]1}')
HDF5_MINOR_VERSION=$(echo $HDF5_VERSION | $AWK -F \. {'print $[]2'})
HDF5_REVISION_VERSION=$(echo $HDF5_VERSION | $AWK -F \. {'print $[]3'})
AC_MSG_RESULT([Found Rhdf5lib with version: $HDF5_MAJOR_VERSION.$HDF5_MINOR_VERSION])
AC_SUBST([HDF5R_LIBS])
AC_SUBST([HDF5R_CPPFLAGS])
AC_SUBST([HDF5_MAJOR_VERSION])
AC_SUBST([HDF5_MINOR_VERSION])


])
