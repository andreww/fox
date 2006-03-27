AC_INIT([generic],[1.0],[wdpgaara@lg.ehu.es])
AC_CONFIG_SRCDIR([config.sub])
dnl
dnl All macros are kept in Src/m4:
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
if test $use_debug = yes; then
  FCFLAGS="$FCFLAGS $FFLAGS_DEBUG"
fi
dnl
AC_ARG_ENABLE([fast],
  AC_HELP_STRING([--enable-fast], [Compile with best known optimization flags]),
  [use_debug=$enableval], [use_debug=no])
if test $use_debug = yes; then
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
if test $use_debug = yes; then
  FCFLAGS="$FCFLAGS $FFLAGS_DEBUG"
fi

AC_ARG_ENABLE([fast],
  AC_HELP_STRING([--enable-fast], [Compile with best known optimization flags]),
  [use_debug=$enableval], [use_debug=no])
if test $use_debug = yes; then
  FCFLAGS="$FCFLAGS $FFLAGS_FAST"
fi
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
dnl this next flag only actually needed if compiling siesta blas & lapack.
TW_FC_CHECK_DCFUNS
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
AC_CONFIG_FILES([xmlf90-config.sh])
AC_OUTPUT
dnl
