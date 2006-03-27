dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.
dnl
dnl As a special exception, the Free Software Foundation gives unlimited
dnl permission to copy, distribute and modify the configure scripts that
dnl are the output of Autoconf.  You need not follow the terms of the GNU
dnl General Public License when using or distributing such scripts, even
dnl though portions of the text of Autoconf appear in them.  The GNU
dnl General Public License (GPL) does govern all other use of the material
dnl that constitutes the Autoconf program.
dnl
dnl Certain portions of the Autoconf source text are designed to be copied
dnl (in certain cases, depending on the input) into the output of
dnl Autoconf.  We call these the "data" portions.  The rest of the Autoconf
dnl source text consists of comments plus executable code that decides which
dnl of the data portions to output in any given case.  We call these
dnl comments and executable code the "non-data" portions.  Autoconf never
dnl copies any of the non-data portions into its output.
dnl
dnl This special exception to the GPL applies to versions of Autoconf
dnl released by the Free Software Foundation.  When you make and
dnl distribute a modified version of Autoconf, you may extend this special
dnl exception to the GPL to apply to your modified version as well, *unless*
dnl your modified version has the potential to copy into its output some
dnl of the text that was the non-data portion of the version that you started
dnl with.  (In other words, unless your change moves or copies text from
dnl the non-data portions to the data portions.)  If your modification has
dnl such potential, you must delete any notice of this special exception
dnl to the GPL from your modified version.
dnl
dnl Copyright Toby White <tow21@cam.ac.uk>  2004-2006     

dnl
AC_DEFUN([TW_FIND_FC_BLAS], [
AC_PREREQ(2.50)
acx_blas_ok=no

AC_LANG_PUSH([Fortran])

AC_ARG_WITH(blas,
	[AC_HELP_STRING([--with-blas=<lib>], [use BLAS library <lib>])])
case $with_blas in
	yes | "") ;;
	no) acx_blas_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) BLAS_LIBS="$with_blas" ;;
	*) BLAS_LIBS="-l$with_blas" ;;
esac

acx_blas_save_LIBS="$LIBS"
LIBS="$LIBS $FLIBS"

sgemm=sgemm
dgemm=dgemm 

# First, check BLAS_LIBS environment variable
if test "$acx_blas_ok" = no; then
  if test "x$BLAS_LIBS" != x; then
    save_LIBS="$LIBS"; LIBS="$BLAS_LIBS $LIBS"
    AC_MSG_CHECKING([for $sgemm in $BLAS_LIBS])
    AC_TRY_LINK_FUNC($sgemm, [acx_blas_ok=yes], [BLAS_LIBS=""])
    AC_MSG_RESULT($acx_blas_ok)
    LIBS="$save_LIBS"
  fi
fi

# BLAS linked to by default?  (happens on some supercomputers)
if test "$acx_blas_ok" = no; then
  AC_MSG_CHECKING([is BLAS linked by default])
  save_LIBS="$LIBS"; LIBS="$LIBS"
  AC_LINK_IFELSE(
           AC_LANG_SOURCE([
      Program test_blas
      Call sgemm
      End Program
  ]), [acx_blas_ok=yes])
  AC_MSG_RESULT($acx_blas_ok)
  LIBS="$save_LIBS"
fi

# BLAS in libblasmt.a? (shipped with Lahey Fortran)
if test "$acx_blas_ok" = no; then
  AC_MSG_CHECKING([for BLAS in -lblasmt])
  save_LIBS="$LIBS"; LIBS="$LIBS -lblasmt"
  AC_LINK_IFELSE(
           AC_LANG_SOURCE([
      Program test_blas
      Call sgemm
      End Program
  ]),
      [BLAS_LIBS="-lblasmt"
       acx_blas_ok=yes])
  AC_MSG_RESULT($acx_blas_ok)
  LIBS="$save_LIBS"
fi


# BLAS in ATLAS library? (http://math-atlas.sourceforge.net/)
if test "$acx_blas_ok" = no; then
  AC_MSG_CHECKING([for BLAS in ATLAS])
  save_LIBS="$LIBS"; LIBS="$LIBS -lcblas -lf77blas -latlas"
  AC_LINK_IFELSE(
           AC_LANG_SOURCE([
      Program test_blas
      Call sgemm
      End Program
  ]),
      [BLAS_LIBS="-lcblas -lf77blas -latlas"
       acx_blas_ok=yes])
  if test "$acx_blas_ok" = no; then
  LIBS="$save_LIBS"
  save_LIBS="$LIBS"; LIBS="$LIBS -lcblas -lf77blas -latlas -lg2c"
  AC_LINK_IFELSE(
           AC_LANG_SOURCE([
      Program test_blas
      Call sgemm
      End Program
  ]),
      [BLAS_LIBS="-lcblas -lf77blas -latlas -lg2c"
       acx_blas_ok=yes])
  fi
  AC_MSG_RESULT($acx_blas_ok)
  LIBS="$save_LIBS"
fi

# BLAS in PhiPACK libraries? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm,
		[AC_CHECK_LIB(dgemm, $dgemm,
		[AC_CHECK_LIB(sgemm, $sgemm,
			[acx_blas_ok=yes; BLAS_LIBS="-lsgemm -ldgemm -lblas"],
			[], [-lblas])],
			[], [-lblas])])
fi

# BLAS in Alpha CXML library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(cxml, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-lcxml"])
fi

# BLAS in Alpha DXML library? (now called CXML, see above)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(dxml, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-ldxml"])
fi

# BLAS in Sun Performance library?
if test $acx_blas_ok = no; then
	if test "x$GCC" != xyes; then # only works with Sun CC
		AC_CHECK_LIB(sunmath, acosp,
			[AC_CHECK_LIB(sunperf, $sgemm,
        			[BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
                                 acx_blas_ok=yes],[],[-lsunmath])])
	fi
fi

# BLAS in SCSL library?  (SGI/Cray Scientific Library)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(scs, $sgemm, [acx_blas_ok=yes; BLAS_LIBS="-lscs"])
fi

# BLAS in SGIMATH library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(complib.sgimath, $sgemm,
		     [acx_blas_ok=yes; BLAS_LIBS="-lcomplib.sgimath"])
fi

# BLAS in IBM ESSL library? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm,
		[AC_CHECK_LIB(essl, $sgemm,
			[acx_blas_ok=yes; BLAS_LIBS="-lessl -lblas"],
			[], [-lblas $FLIBS])])
fi

# BLAS in vecLib Framework (MacOSX 10.2 onwards)
if test $acx_blas_ok = no; then
  LDFLAGS_save="$LDFLAGS"
  for ac_flag in "-framework vecLib" "-Wl,-framework -Wl,vecLib"
  do
    LDFLAGS="$LDFLAGS $ac_flag"
    AC_LINK_IFELSE(
           AC_LANG_SOURCE([
      Program test_blas
      Call sgemm
      End Program
    ]),
    [acx_blas_ok=yes;BLAS_LIBS="$ac_flag"],[])
  LDFLAGS="$LDFLAGS_save"
  done
fi

# Generic BLAS library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm, [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi
dnl
AC_SUBST(BLAS_LIBS)
dnl
LIBS="$acx_blas_save_LIBS"
dnl
# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
AS_IF([test x"$acx_blas_ok" = xyes],
      [$1],
      [m4_default([$2],[AC_MSG_ERROR([Could not find BLAS library])])]
      )
AC_LANG_POP
])
