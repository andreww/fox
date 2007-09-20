AC_INIT([FoX],[2.0],[tow21@cam.ac.uk])
AC_CONFIG_SRCDIR([config.sub])
dnl
dnl All macros are kept in ./m4:
dnl AC_CONFIG_MACRO_DIR([m4])
dnl FIXME that doesn't work. Why not?
dnl
dnl Find system type.
dnl Uses rewritten AC_CONFIG_AUX_DIR to look for config.sub
dnl instead of install script
AC_CONFIG_AUX_DIR(.)
AC_CANONICAL_HOST
dnl
dnl FIXME: just for the moment to work with NG's fpp detection:
dnl m4_pattern_allow([AM_CPPFLAGS])
dnl this is no longer necessary in my current fortran.m4. Upstream needs it, though.
dnl
AC_LANG(Fortran)
AC_PROG_FC
dnl
AC_FC_FIXEDFORM([f])
AC_SUBST(FCFLAGS_fixed_f)
dnl
AC_FC_FREEFORM([f90])
AC_SUBST(FCFLAGS_free_f90)
dnl
AC_LANG_PUSH(Preprocessed Fortran)
AC_PROG_FPP
dnl
AC_FPP_FIXEDFORM([F])
AC_SUBST(FPPFLAGS_fixed_F)
dnl
AC_FPP_FREEFORM([F90])
AC_SUBST(FPPFLAGS_free_F90)
AC_LANG_POP(Preprocessed Fortran)
dnl
if test $ac_cv_fpp_build_rule = direct; then
     F_RULE=['$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_fixed_F)  $< ']
     F90_RULE=['$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_free_F90) $< ']
elif test $ac_cv_fpp_build_rule = indirect; then
     F_RULE=['$(FPP) $(FPPFLAGS) $< $(FPP_OUTPUT) aux_$*.f && $(FC) -c -o $*.o $(FFLAGS) $(INCFLAGS) $(FCFLAGS_fixed_f) aux_$*.f']
     F90_RULE=['$(FPP) $(FPPFLAGS) $< $(FPP_OUTPUT) aux_$*.f90 && $(FC) -c -o $*.o $(FFLAGS) $(INCFLAGS) $(FCFLAGS_free_f90) aux_$*.f90']
fi
dnl Generate dependency rules according to whether we need
dnl separate preprocessing stage.
AC_SUBST(F_RULE)
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
SIESTA_ARCH="$host--$FC_ID"
AC_SUBST(SIESTA_ARCH)
dnl
dnl Check for any extra compiler-specific flags necessary
dnl Also set FFLAGS_DEBUG and FFLAGS_FAST
dnl SYS will be "nag" (ie standard f95) for almost all cases
SYS=nag
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
AC_ARG_ENABLE([wcml],
  AC_HELP_STRING([--enable-wcml], [Compile wcml subsystem (for CML output)]),
  [eval WCML_$enableval=wcml_lib])
AC_ARG_ENABLE([wkml],
  AC_HELP_STRING([--enable-wkml], [Compile wkml subsystem (for KML output)]),
  [eval WKML_$enableval=wkml_lib])
AC_ARG_ENABLE([sax],
  AC_HELP_STRING([--enable-sax], [Compile SAX parser]),
  [eval SAX_$enableval=sax_lib])
AC_ARG_ENABLE([dom],
  AC_HELP_STRING([--enable-dom], [Compile dom subsystem (for DOM output)]),
  [eval DOM_$enableval=dom_lib])
dnl
dnl If nothing was explicitly enabled, then enable everything that wasn't disabled.n
dnl Otherwise we only enable whatever was explicitly enabled.
if test x$WCML_yes$WKML_yes$SAX_yes$DOM_yes = x; then
  if test x$WCML_no = x; then WCML_yes=wcml_lib; fi
  if test x$WKML_no = x; then WKML_yes=wcml_lib; fi
  if test x$SAX_no = x; then SAX_yes=wcml_lib; fi
  if test x$DOM_no = x; then DOM_yes=wcml_lib; fi
fi
dnl ensure that if sax is disabled, so is dom
dnl so if the user explicitly disables sax but enables dom
dnl they'll get neither. So don't do that then!
if test x$SAX_no = xsax_lib; then
  $DOM_no=dom_lib
  $DOM_yes=
fi
dnl Finally, construct the build targets
dnl wxml_lib is always included.
BUILD_TARGETS="wxml_lib $WCML_yes $WKML_yes $SAX_yes $DOM_yes"
AC_SUBST(BUILD_TARGETS)
dnl
dnl Check compiler characteristics; it must support F95
dnl to use the cpu_time intrinsic. Otherwise, SYS must
dnl be set appropriately. Only pgf90 should fail this.
dnl Actually, we should simply check to see if cpu_time is supported.
dnl TW_CHECK_FC_95([SYS=nag],dnl
dnl   TW_CHECK_FC_90([SYS=cputim],[])
dnl                   )
AC_SUBST(SYS)
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
TW_FC_CHECK_FLUSH
TW_FC_CHECK_ABORT
dnl
dnl Fix up DEFS -> FPPFLAGS conversion.
for i in $DEFS; do
   FPPFLAGS="$FPPFLAGS $FPPFLAGS_DEF$i"
done
dnl
dnl Installation instructions ..
TW_PROG_CYGPATH_W
AC_PREFIX_DEFAULT([`pwd`/objs])
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
dnl
