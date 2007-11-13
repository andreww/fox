AC_INIT([FoX],[3.0],[tow@uszla.me.uk])
AC_CONFIG_SRCDIR([README.FoX.txt])
dnl
dnl All macros are kept in ./m4:
dnl AC_CONFIG_MACRO_DIR([m4])
dnl FIXME that doesn't work. Why not?
dnl
dnl Find system type.
dnl Uses rewritten AC_CONFIG_AUX_DIR to look for config.sub
dnl instead of install script
AC_CONFIG_AUX_DIR([config])
AC_CANONICAL_HOST
dnl
dnl FIXME: just for the moment to work with NG's fpp detection:
dnl m4_pattern_allow([AM_CPPFLAGS])
dnl this is no longer necessary in my current fortran.m4. Upstream needs it, though.
dnl
AC_LANG(Fortran)
AC_PROG_FC
dnl
AC_FC_FREEFORM([f90])
AC_SUBST(FCFLAGS_free_f90)
dnl
AC_LANG_PUSH(Preprocessed Fortran)
AC_PROG_FPP
dnl
AC_FPP_FREEFORM([F90])
AC_SUBST(FPPFLAGS_free_F90)
AC_LANG_POP(Preprocessed Fortran)
dnl
if test $ac_cv_fpp_build_rule = direct; then
     F90_RULE=['$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_free_F90) $< ']
elif test $ac_cv_fpp_build_rule = indirect; then
  if test $ac_cv_fc_cifs = yes; then
     F90_RULE=['while [ 0 ]; do FPPFILE=$*.$$RANDOM.f90; if ! test -f $$FPPFILE; then break; fi; done; $(FPP) $(FPPFLAGS) $< > $$FPPFILE && $(FC) -c -o $*.$(OBJEXT) $(FFLAGS) $(INCFLAGS) $(FCFLAGS_free_f90) $$FPPFILE; rm $$FPPFILE']
  elif test x$FPP_OUTPUT = x; then
     F90_RULE=['$(FPP) $(FPPFLAGS) $< && $(FC) -c $(FFLAGS) $(INCFLAGS) $(FCFLAGS_free_f90) $*.f90; rm $*.f90']
  else
     F90_RULE=['$(FPP) $(FPPFLAGS) $< > $*.f90 && $(FC) -c $(FFLAGS) $(INCFLAGS) $(FCFLAGS_free_f90) $*.f90; rm $*.f90']
  fi
fi
dnl Generate dependency rules according to whether we need
dnl separate preprocessing stage.
AC_SUBST(F90_RULE)
dnl
AC_PROG_RANLIB
dnl
dnl Compile-type options.
dnl
AC_ARG_ENABLE([debug],
  AC_HELP_STRING([--enable-debug], [Compile with debugging support]),
  [use_debug=$enableval], [use_debug=no])
if test x$use_debug = xyes; then
  FCFLAGS="$FCFLAGS $FFLAGS_DEBUG"
fi
dnl
AC_ARG_ENABLE([fast],
  AC_HELP_STRING([--enable-fast], [Compile with best known optimization flags]),
  [use_debug=$enableval], [use_debug=no])
if test x$use_debug = xyes; then
  FCFLAGS="$FCFLAGS $FFLAGS_FAST"
fi
dnl
dnl Generate host/compiler ID string to encode in program
TW_FC_ID
dnl
dnl Also set FFLAGS_DEBUG and FFLAGS_FAST
TW_FC_ID_FLAGS
dnl
AC_ARG_ENABLE([debug],
  AC_HELP_STRING([--enable-debug], [Compile with debugging support]),
  [use_debug=$enableval], [use_debug=no])
if test x$use_debug = xyes; then
  FCFLAGS="$FCFLAGS $FFLAGS_DEBUG"
fi

AC_ARG_ENABLE([fast],
  AC_HELP_STRING([--enable-fast], [Compile with best known optimization flags]),
  [use_debug=$enableval], [use_debug=no])
if test x$use_debug = xyes; then
  FCFLAGS="$FCFLAGS $FFLAGS_FAST"
fi
dnl
dnl Now check for whether we have been asked for any particular
dnl subsystems - wcml, wkml, sax, dom.
AC_ARG_ENABLE([wxml],
  AC_HELP_STRING([--enable-wxml], [Compile wxml subsystem (for XML output)]),
  [eval WXML_$enableval=wxml_lib])
AC_ARG_ENABLE([wcml],
  AC_HELP_STRING([--enable-wcml], [Compile wcml subsystem (for CML output)]),
  [eval WCML_$enableval=wcml_lib])
dnl AC_ARG_ENABLE([wkml],
dnl   AC_HELP_STRING([--enable-wkml], [Compile wkml subsystem (for KML output)]),
dnl   [eval WKML_$enableval=wkml_lib])
AC_ARG_ENABLE([sax],
  AC_HELP_STRING([--enable-sax], [Compile SAX parser]),
  [eval SAX_$enableval=sax_lib])
AC_ARG_ENABLE([dom],
  AC_HELP_STRING([--enable-dom], [Compile dom subsystem (for DOM output)]),
  [eval DOM_$enableval=dom_lib])
dnl
dnl If nothing was explicitly enabled, then enable everything that wasn't disabled.n
dnl Otherwise we only enable whatever was explicitly enabled.
if test x$WXML_yes$WCML_yes$WKML_yes$SAX_yes$DOM_yes = x; then
  if test x$WXML_no = x; then WXML_yes=wxml_lib; fi
  if test x$WCML_no = x; then WCML_yes=wcml_lib; fi
dnl  if test x$WKML_no = x; then WKML_yes=wkml_lib; fi
  if test x$SAX_no = x; then SAX_yes=sax_lib; fi
  if test x$DOM_no = x; then DOM_yes=dom_lib; fi
fi
dnl Note that if we try to exclude something which an
dnl included module depends upon, then the inclusion
dnl will happen anyway. eg if we exclude wxml, but include
dnl wkml, then wxml will get built as a dependency by the
dnl makefile. 
dnl
dnl Finally, construct the build targets
dnl BUILD_TARGETS="$WXML_yes $WCML_yes $WKML_yes $SAX_yes $DOM_yes"
BUILD_TARGETS="$WXML_yes $WCML_yes $SAX_yes $DOM_yes"
AC_SUBST(BUILD_TARGETS)
dnl
dnl Check for compiler kind flags
dnl
AC_FC_GET_REAL_KINDS
SP_KIND=$ac_fc_real_kind_sp
DP_KIND=$ac_fc_real_kind_dp
AC_SUBST(SP_KIND)
AC_SUBST(DP_KIND)
dnl
dnl Check whether we need to pass a flag to get at the flush intrinsic.
TW_FC_CHECK_FLUSH([],[AC_MSG_RESULT([cannot compile call to FLUSH])])
TW_FC_CHECK_ABORT([],[AC_MSG_RESULT([cannot compile call to ABORT])])
dnl
dnl Fix up DEFS -> FPPFLAGS conversion.
for i in $DEFS; do
   FPPFLAGS="$FPPFLAGS $FPPFLAGS_DEF$i"
done
dnl
dnl Library functionality ...
dnl How to make a static library ...
dnl we should test for this, but I don't know how yet.
dnl
dnl Most command-line windows compilers will want to use
dnl the MS linking tools, which require a .lib extension
dnl g95 & gfortran wont, they use normal Unix conventions.
dnl Everyone else everywhere uses unix conventions
case $host_os in
  *cygwin*)
     if test $FC_ID = G95 -o $FC_ID = Gfortran; then
       LIBEXT=a
     else
       LIBEXT=lib
     fi
     ;;
  *)
     LIBEXT=a
     ;;
esac
AC_SUBST(LIBEXT)
dnl
dnl Installation instructions ..
TW_PROG_CYGPATH_W
OBJSDIR=`pwd`/objs
AC_SUBST(OBJSDIR)
AC_FC_MOD_PATH_FLAG
AC_FC_MOD_SUFFIX
#
AC_SUBST(FPPFLAGS)
AC_SUBST(FFLAGS)
dnl
AC_SUBST(FCFLAGS)
AC_SUBST(LDFLAGS)
dnl
AC_CONFIG_FILES([arch.make])
AC_CONFIG_FILES([FoX-config])
AC_OUTPUT
chmod +x FoX-config
touch .config
dnl and to stop over-eager makes trying to recreate f90 files
dnl when m4 is not gnu m4 ..
touch */*.f90 
dnl
