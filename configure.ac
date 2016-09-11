# Copyright (C) 2014 - 2016  Jack O. Wasey
# Copyright (C) 2016 - 2016  Anton Bossenbroek
#
# This file is part of GeneralTree.
#
# GeneralTree is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# GeneralTree is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with icd9. If not, see <http:#www.gnu.org/licenses/>.

AC_PREREQ([2.62])
AC_INIT(GeneralTree)
AC_COPYRIGHT(Copyright (C) 2014 - 2016  Anton Bossenbroek)
AC_CONFIG_SRCDIR([src])
AC_CONFIG_HEADERS([src/config.h:src/config.h.in])
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_MACRO_DIR([tools/m4])

echo "Checking whether R_HOME already set? R_HOME = ${R_HOME}"
: ${R_HOME:=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
R_BIN="${R_HOME}/bin/R"
CC=`"${R_BIN}" CMD config CC`
CXX=`"${R_BIN}" CMD config CXX`
CPP=`"${R_BIN}" CMD config CPP`
CFLAGS=`"${R_BIN}" CMD config CFLAGS`
CPPFLAGS=`"${R_BIN}" CMD config CPPFLAGS`
CXXFLAGS=`"${R_BIN}" CMD config CXXFLAGS`
# Writing R Extensions recommends the following, but I don't seem to need it, perhaps because I disable C++11 in some systems when it won't work with OpenMP:
CXX1X=`"${R_BIN}" CMD config CXX1X`
CXX1XSTD=`"${R_BIN}" CMD config CXX1XSTD`
CXXFLAGS=`"${R_BIN}" CMD config CXXFLAGS`
LIBS="${PKG_LIBS}"

AC_CHECK_HEADER([string.h])
AC_CHECK_HEADER_STDBOOL
AC_PROG_CC
AC_PROG_CXX
AC_TYPE_SIZE_T
AC_C_INLINE

#BOOST_REQUIRE([1.60.0])

# this only works in C++, since it is a C++ system header...
AC_LANG_PUSH([C++])

# C++11, autoconf should determine the correct flags
# https://www.gnu.org/software/autoconf-archive/ax_cxx_compile_stdcxx_11.html
AX_CXX_COMPILE_STDCXX_11(noext, optional)

if test "${HAVE_CXX11}" = 1; then
  AC_MSG_CHECKING([Have C++11, so set Makevars flag for R])
  # the following 'switch' variable is created by the AC CXX test. This is how R configure gets the flag itself.
  CXX11_FLAG=${switch}
fi
AC_SUBST(CXX11_FLAG)

AC_MSG_NOTICE([getting path to test for testthat Catch headers])
TESTTHAT_INCLUDE_DIR=`"${R_BIN}" --vanilla --slave -q -e "suppressWarnings(try(cat(tools::file_path_as_absolute(base::system.file(package='testthat','include','testthat',mustWork=FALSE))),silent=TRUE))"`
BOOST_INCLUDE_DIR=`"${R_BIN}" --vanilla --slave -q -e "suppressWarnings(try(cat(tools::file_path_as_absolute(base::system.file(package='BH','include',mustWork=FALSE))),silent=TRUE))"`

# maybe a better way, but I need to have the testthat package include dir in the pre-processor include directories:
CPPFLAGS="$CPPFLAGS -I$TESTTHAT_INCLUDE_DIR -I$BOOST_INCLUDE_DIR"
AC_LANG_PUSH([C++])
# this gives a config var with full path in it when built via check: AC_CHECK_HEADERS([testthat.h])
# these don't work:
## AC_CHECK_HEADERS([testthat])
## AC_CHECK_HEADERS([testthat.h])

AC_CHECK_HEADER(testthat.h, [AC_DEFINE(HAVE_TESTTHAT_H, 1, [define as 1 if testthat catch available])],
  [AC_MSG_NOTICE([testthat headers for Catch tests not found])])
AC_CHECK_HEADER(boost/bimap.hpp, [AC_DEFINE(HAVE_BOOST_BIMAP_HPP, 1, [define as 1 if boost bimap available])],
  [AC_MSG_NOTICE([boost bimap headers not found])])

AC_LANG_POP([C++])
# end testthat test

AC_OUTPUT
