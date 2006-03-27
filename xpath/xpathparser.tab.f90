
module parse_bison90
! A Bison parser, made from xpathparser.y

! line 17 "xpathparser.y"


  use tokenizer
  use m_xpath_types, only: YYVALTYPE => xpath_type
  use m_xpath_types, only: currentLocation, rootNode

  implicit none

! bison.simple.f90 - Fortran skeleton output parser for Bison v1.16.
! Copyright (C) 1984, 1989, 1990, 1991 Bob Corbett and Richard Stallman
! Copyright (C) 1991, 1992 Thorsten Ohl
! Copyright (C) 2005 Toby White
!
! This program is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
! This is the parser code that is written into each Fortran77 bison parser
! when the %semantic_parser declaration is not specified in the grammar.
! It was written by Richard Stallman by simplifying the hairy parser
! used when %semantic_parser is specified.
! It has been unmercifully hacked by Thorsten Ohl to accomodate Fortran77.
! ... and then further by Toby White for Fortran 95.
!
! Note: there must be only one dollar and one at sign in this file.
! The former is replaced by the list of actions, each action
! as one case of the switch.  The latter is replaced by the
! parser tables.  (This means in particular, that dollar signs from RCS
! keywords must be removed before installation!  For this reason, I
! actually contemplate to replace the dollar by a hash mark.)
!
! Revision 1.16.2.1  1992/05/31 01:05:34  ohl
! GNU Bison Version 1.16, with Fortran-77 support
!
! Revision 1.1  1992/04/13  20:02:25  ohl
! Put the FORTRAN changes under CVS control.
!
! Revision 1.16.1.2  1992/03/18  20:50:19  ohl
! RCS Keywords
!
! Revision 1.16.1.1  1992/03/18  20:23:44  ohl
! Severe bug (noticed with VAX Fortran):
! FORTRAN-77 is entitled to change the order of evaluation
! of logical expressions, therefore
!       if ( <bounds-check> .and. <array-access>) then
!         ...
! is wrong, as the array access might come BEFORE the bounds
! check.
!

! Dictionary for the jump labels:
! 90000: yybackup
! 90001: yydefault
! 90002: yyerrdefault
! 90003: yyerrhandle
! 90004: yyerrlab
! 90005: yyerrlab1
! 90006: yyerrpop
! 90007: yynewstate
! 90008: yyreduce
! 90009: yyresume

  ! YYDEPTH indicates the size of the parser's stacks
  integer, parameter ::  YYDPTH = 200

  integer, parameter ::  stderr = 0

  integer, parameter :: YYEMPT = -2 
  integer, parameter :: YYEOF  =  0

  integer, parameter :: YYACPT  = 0
  integer, parameter :: YYABRT  = 1

  integer, parameter :: YYTERR  = 1
  integer, parameter :: YYERRC  = 256

  logical, save :: yydbg  = .false.
  integer, save :: yynerr = 1

  ! number of tokens to shift before error messages enabled
  integer, save :: yyerst

  ! The lookahead symbol, together with its semantic value and location
  integer, save :: yychar
  type(YYVALTYPE), save :: yylval
  
  type YYLTYPE
    integer :: first_line
    integer :: last_line
    integer :: first_column
    integer :: last_column
  end type YYLTYPE

  type(YYLTYPE), save :: yylloc

!  interface assignment(=)
!    module procedure yylloc_assign
!  end interface assignment(=)

  private

  public :: yyparse
  public :: yylval
  public :: YYACPT
  public :: YYABRT

contains

  integer function yyparse ()

    ! temporary variables
    integer i1

    ! The current parser state
    integer yystat
    integer yyn


    ! lookahead token as an internal (translated) token number
    integer yychai

    ! the state stack
    integer yyssp, yyssa(YYDPTH)

    ! the semantic value stack
    integer :: yyvsp
    type(YYVALTYPE) :: yyvsa(YYDPTH)

    ! The location stack.
    type(YYLTYPE), dimension(YYDPTH) :: yylsa
    integer :: yylsp

    ! the variable used to return semantic values
    ! from the action routines
    type(YYVALTYPE) :: yyval

    integer yylen

    ! ==========< Start of the parser tables computed by Bison >============
    !integer yystos(0:103)

integer, parameter ::  YYLAST = 112

integer, parameter :: YYFIN = 103
integer, parameter :: YYFLAG = -32768
integer, parameter :: YYNTBA = 35

integer, parameter, dimension(0:275) :: yytranslate = (/0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2&
&,2,2,22,23,2,33,24,34,27,21,2,2,2,2,2,2,2,2,2,2,2,2,31,30,32,2,28,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,25,2,26,2,2,&
&2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,29,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2&
&,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2&
&,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20/)

integer,parameter,dimension(0:66)::yyprhs = (/0,0,2,4,6,8,11,13,15,19,21,24,26,27,30,34,36,38,43,44,46,48,52,56,58,62,67,69,71,73,7&
&6,78,80,84,86,88,90,95,97,101,103,105,109,114,116,119,121,125,127,131,133,137,141,143,147,151,155,159,161,165,169,171,175,179,183,&
&187,189/)

integer,parameter,dimension(0:191)::yyrhs=(/51,0,38,0,37,0,21,0,21,38,0,47,0,39,0,38,21,39,0,48,0,41,40,0,49,0,0,40,45,0,6,7,42,0,5&
&0,0,3,0,4,22,43,23,0,0,44,0,51,0,44,24,51,0,25,46,26,0,51,0,21,21,38,0,38,21,21,39,0,27,0,11,0,42,0,28,42,0,57,0,10,0,22,51,23,0,8&
&,0,9,0,53,0,5,22,43,23,0,55,0,54,29,55,0,36,0,56,0,56,21,38,0,56,21,21,38,0,52,0,56,45,0,58,0,57,13,58,0,59,0,58,12,59,0,60,0,59,3&
&0,60,0,59,16,60,0,61,0,60,31,61,0,60,32,61,0,60,14,61,0,60,15,61,0,62,0,61,33,62,0,61,34,62,0,63,0,62,17,63,0,62,18,63,0,62,19,63,&
&0,62,20,63,0,54,0,34,63,0/)

integer,parameter,dimension(0:66)::yyrline=(/0,40,46,47,52,57,62,67,70,73,78,83,86,91,100,105,114,115,122,127,130,135,144,153,158,1&
&67,176,181,190,195,204,209,210,215,221,227,235,247,248,257,258,259,264,273,274,283,284,293,294,303,304,309,318,319,324,329,334,343&
&,344,349,358,359,364,369,374,383,384/)


character(len=31),parameter,dimension(0:64)::yytname=(/'$                              ','error                          ','$illega&
&l.                      ','WILDCARDNAME                   ','NODETYPE                       ','FUNCTIONNAME                   ','A&
&XISNAME                       ','AXISSUF                        ','LITERAL                        ','NUMBER                       &
&  ','VARIABLEREFERENCE              ','PARENT                         ','AND                            ','OR                     &
&        ','LE                             ','GE                             ','NE                             ','MULTIPLYOPERATOR &
&              ','DIV                            ','MOD                            ','QUO                            ','''/''      &
&                      ','''(''                            ',''')''                            ',''',''                            &
&','''[''                            ',''']''                            ','''.''                            ','''@''              &
&              ','''|''                            ','''=''                            ','''<''                            ','''>''&
&                            ','''+''                            ','''-''                            ','TopExpr                    &
&    ','LocationPath                   ','AbsoluteLocationPath           ','RelativeLocationPath           ','Step                 &
&          ','predicates                     ','Basis                          ','NodeTest                       ','arglist        &
&                ','args                           ','Predicate                      ','PredicateExpr                  ','Abbreviat&
&edAbsoluteLocationPath','AbbreviatedRelativeLocationPath','AbbreviatedStep                ','AbbreviatedBasis               ','Exp&
&r                           ','PrimaryExpr                    ','FunctionCall                   ','UnionExpr                      &
&','PathExpr                       ','FilterExpr                     ','OrExpr                         ','AndExpr                  &
&      ','EqualityExpr                   ','RelationalExpr                 ','AdditiveExpr                   ','MultiplicativeExpr &
&            ','UnaryExpr                      ','                               '/)

integer,parameter,dimension(0:66)::yyr1=(/0,35,36,36,37,37,37,38,38,38,39,39,40,40,41,41,42,42,43,43,44,44,45,46,47,48,49,49,50,50,&
&51,52,52,52,52,52,53,54,54,55,55,55,55,56,56,57,57,58,58,59,59,59,60,60,60,60,60,61,61,61,62,62,62,62,62,63,63/)

integer,parameter,dimension(0:66)::yyr2=(/0,1,1,1,1,2,1,1,3,1,2,1,0,2,3,1,1,4,0,1,1,3,3,1,3,4,1,1,1,2,1,1,3,1,1,1,4,1,3,1,1,3,4,1,2&
&,1,3,1,3,1,3,3,1,3,3,3,3,1,3,3,1,3,3,3,3,1,2/)


integer,parameter,dimension(0:103)::yydefact=(/0,16,0,0,0,33,34,31,27,4,0,26,0,0,39,3,2,7,12,28,6,9,11,15,1,43,35,65,37,40,30,45,47&
&,49,52,57,60,18,18,0,0,5,0,29,66,0,10,0,0,0,44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19,20,0,14,24,32,0,8,13,38,0,41,0,23,46,48,51,50,55,5&
&6,53,54,58,59,61,62,63,64,17,0,36,25,42,22,21,0,0,0/)

integer,parameter,dimension(0:28)::yydefgoto=(/101,14,15,16,17,46,18,19,65,66,50,78,20,21,22,23,67,25,26,27,28,29,30,31,32,33,34,35&
&,36/)

integer,parameter,dimension(0:103)::yypact=(/3,-32768,7,19,12,-32768,-32768,-32768,-32768,17,3,-32768,1,3,-32768,-32768,18,-32768,-&
&32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,29,-32768,-3,33,53,-13,79,-7,16,-32768,3,3,1,80,18,44,-32768,-32768,&
&60,55,51,71,3,-32768,3,3,3,3,3,3,3,3,3,3,3,3,3,3,62,52,-32768,66,-32768,18,-32768,80,-32768,-32768,-32768,80,18,64,-32768,53,-13,7&
&9,79,-7,-7,-7,-7,16,16,-32768,-32768,-32768,-32768,-32768,3,-32768,-32768,18,-32768,-32768,96,97,-32768/)

integer,parameter,dimension(0:28)::yypgoto=(/-32768,-32768,-32768,-8,-30,-32768,-32768,4,67,-32768,58,-32768,-32768,-32768,-32768,-&
&32768,0,-32768,-32768,-32768,59,-32768,-32768,61,57,-6,45,10,-11/)

integer,parameter,dimension(0:112)::yytable=(/24,41,44,53,1,2,1,2,3,4,42,5,6,7,8,73,43,54,48,39,1,2,49,4,9,10,59,60,8,37,11,12,70,6&
&1,62,63,64,13,40,45,77,38,97,69,11,12,51,82,83,79,90,91,92,93,1,2,3,4,47,5,6,7,8,1,2,52,4,71,98,88,89,8,9,10,1,2,95,4,11,12,49,72,&
&8,1,2,94,4,11,12,96,99,8,76,55,56,100,102,103,11,12,84,85,86,87,74,68,75,11,12,81,57,58,80/)

integer,parameter,dimension(0:112)::yycheck=(/0,9,13,16,3,4,3,4,5,6,10,8,9,10,11,45,12,30,21,7,3,4,25,6,21,22,33,34,11,22,27,28,40,&
&17,18,19,20,34,21,21,48,22,72,39,27,28,13,53,54,49,61,62,63,64,3,4,5,6,29,8,9,10,11,3,4,12,6,23,76,59,60,11,21,22,3,4,24,6,27,28,2&
&5,21,11,3,4,23,6,27,28,23,26,11,21,14,15,95,0,0,27,28,55,56,57,58,46,38,47,27,28,52,31,32,51/)

    ! ===========< End of the parser tables computed by Bison >=============

    if (yydbg) write (stderr, *) "Toby White's version."

    ! We start parsing in state 0 (the same is true for the error
    ! status) and naturally we haven't seen an error yet.
    yystat = 0
    yyerst = 0
    yynerr = 0

    ! Cause a token to be read.
    yychar = YYEMPT

    ! Initialize the stack pointers. Waste one element of the value and
    ! the location stack so that they stay on the same level as the
    ! state stack.
    yyssp = 0
    yyvsp = 1
    yylsp = 1

    ! Push a new state, which is found in  YYSTAT.  In all cases,
    ! when you get here, the value and location stacks have just
    ! been pushed.  So pushing a state here evens the stacks.

    ! label yynewstate:
90007 continue

    yyssp = yyssp + 1
    yyssa(yyssp) = yystat

    if (yyssp.ge.YYDPTH) then
      call yyerro ('Parser stack overflow')
      return
    endif

    if (yydbg) write (stderr, *) 'Entering state ', yystat

    ! label yybackup:
90000 continue

    ! Do appropriate processing given the current state.
    ! Read a lookahead token if we need one and don't already have one.
    ! yyresume continue;
    ! First try to decide what to do without reference to lookahead token.
    yyn = yypact(yystat)

    ! In case of an error, jump to the default action.
    if (yyn.eq.YYFLAG) goto 90001

    ! Not known => get a lookahead token if don't already have one.
    ! YYCHAR is either YYEMPTY or YYEOF or a valid token in
    ! external form.
    if (yychar.eq.YYEMPT) then
      if (yydbg) write (stderr, *) 'Reading a token: '
      yychar = yylex ()
    endif

    ! Convert token to internal form (in yychai) for indexing tables with
    if (yychar.le.0) then
      yychai = 0
      ! YYCHAR <= 0 means end of input, so don't call YYLEX any more.
      yychar = YYEOF
      if (yydbg) write (stderr, *) 'Now at end of input.'
    else
      yychai = yytranslate(yychar)
      if (yydbg) write (stderr, *) 'Next token is ', yychar, ' (', yytname(yychai), ')'
    endif

    yyn = yyn + yychai
    ! Jump to the default action
    ! (NB: see below for the reason why this can't be one put into a
    ! single statement).
    if (yyn.lt.0 .or. yyn.gt.YYLAST) then
      goto 90001
    else
      if (yycheck(yyn).ne.yychai) goto 90001
    endif
    yyn = yytable(yyn)

    ! YYN is what to do for this token type in this state.
    ! Negative => reduce, -YYN is rule number.
    ! Positive => shift, YYN is new state.
    !    New state is final state => don't bother to shift,
    !    just return success.
    ! 0, or YYFLAG => error.

    if (yyn.lt.0) then
      if (yyn.eq.YYFLAG) goto 90004
      ! Compute rule number and reduce
      yyn = -yyn
      goto 90008
    else if (yyn.eq.0) then
      goto 90004
    endif

    if (yyn.eq.YYFIN) then
      ! We're through: accept.
      yyparse = YYACPT
      return
    endif

    ! Shift the lookahead token.
    if (yydbg) write (stderr, *)                                     & 
      &           'Shifting token ', yychar, ' (', yytname(yychai), ')' 

    ! Discard the token being shifted unless it is EOF.
    if (yychar.ne.YYEOF) yychar = YYEMPT
    yyvsp = yyvsp + 1
    yyvsa(yyvsp) = yylval
    yylsp = yylsp + 1
    yylsa(yylsp) = yylloc
    !do i1 = 1, 4
    !  yylsa(i1,yylsp) = yylloc(i1)
    !enddo

    ! count tokens shifted since error; after three, turn off error status.
    if (yyerst.ne.0) yyerst = yyerst - 1

    yystat = yyn

    ! Push a new state
    goto 90007

    ! Do the default action for the current state.
90001 continue
    yyn = yydefact(yystat)
    ! Jump to the error routine
    if (yyn.eq.0) goto 90004

    ! Do a reduction.  YYN is the number of a rule to reduce with.
90008 continue
    yylen = yyr2(yyn)

    ! implement default value of the action
    yyval = yyvsa(yyvsp+1-yylen)
    if (yydbg) then
      write (stderr, *)                                             & 
        &    'Reducing via rule ', yyn, ' (line ', yyrline(yyn), ')'
      ! Print the symbols being reduced, and their result.
      i1 = yyprhs(yyn)
23000 if (yyrhs(i1).gt.0) then
        write (stderr, *) '    ', yytname(yyrhs(i1))
23001   i1= i1 + 1
        goto 23000
      endif

      write (stderr, *) ' -> ', yytname(yyr1(yyn))
    endif

    ! ========< Start of the actions copied from the grammar file >=========
select case (yyn)
  case (1)
! line 40 "xpathparser.y"
  
yyval = xpath()
  case (4)
! line 52 "xpathparser.y"


yyval = locationPath(RootNode)

  case (5)
! line 57 "xpathparser.y"


yyval = locationPath(yyvsa(yyvsp+(0)))

  case (7)
! line 67 "xpathparser.y"

yyval = addStep(currentLocation, yyvsa(yyvsp+(0)))
  case (8)
! line 70 "xpathparser.y"

yyval = addStep(yyvsa(yyvsp+(-2)), yyvsa(yyvsp+(0)))
  case (10)
! line 78 "xpathparser.y"


continue

  case (12)
! line 86 "xpathparser.y"


continue

  case (13)
! line 91 "xpathparser.y"


continue

  case (14)
! line 100 "xpathparser.y"


continue

  case (17)
! line 115 "xpathparser.y"


continue

  case (18)
! line 122 "xpathparser.y"


continue

  case (20)
! line 130 "xpathparser.y"


continue

  case (21)
! line 135 "xpathparser.y"


continue

  case (22)
! line 144 "xpathparser.y"


continue

  case (24)
! line 158 "xpathparser.y"


continue

  case (25)
! line 167 "xpathparser.y"


continue

  case (26)
! line 176 "xpathparser.y"


continue

  case (27)
! line 181 "xpathparser.y"


continue

  case (28)
! line 190 "xpathparser.y"


continue

  case (29)
! line 195 "xpathparser.y"


continue

  case (32)
! line 210 "xpathparser.y"


continue

  case (33)
! line 215 "xpathparser.y"


continue

  case (34)
! line 221 "xpathparser.y"


continue

  case (36)
! line 235 "xpathparser.y"


continue

  case (38)
! line 248 "xpathparser.y"


continue

  case (41)
! line 259 "xpathparser.y"


continue

  case (42)
! line 264 "xpathparser.y"


continue

  case (44)
! line 274 "xpathparser.y"


continue

  case (46)
! line 284 "xpathparser.y"


continue

  case (48)
! line 294 "xpathparser.y"


continue

  case (50)
! line 304 "xpathparser.y"


continue

  case (51)
! line 309 "xpathparser.y"


continue

  case (53)
! line 319 "xpathparser.y"


continue

  case (54)
! line 324 "xpathparser.y"


continue

  case (55)
! line 329 "xpathparser.y"


continue

  case (56)
! line 334 "xpathparser.y"


continue

  case (58)
! line 344 "xpathparser.y"


continue

  case (59)
! line 349 "xpathparser.y"


continue

  case (61)
! line 359 "xpathparser.y"


continue

  case (62)
! line 364 "xpathparser.y"


continue

  case (63)
! line 369 "xpathparser.y"


continue

  case (64)
! line 374 "xpathparser.y"


continue

  case (66)
! line 384 "xpathparser.y"


continue


end select


    ! =========< End of the actions copied from the grammar file >==========

    ! Pop the state and value stacks:
    yyssp = yyssp - yylen
    yyvsp = yyvsp - yylen
    yylsp = yylsp - yylen

    if (yydbg) then
      write (stderr, *) 'state stack now'
      do i1 = 1, yyssp
        write (stderr, *) '  ', yyssa(i1)
      enddo
    endif

    yyvsp = yyvsp + 1
    yyvsa(yyvsp) = yyval
    yylsp = yylsp + 1
    if (yylen == 0) then
      yylsa(yylsp)%first_line = yylloc%first_line
      yylsa(yylsp)%first_column = yylloc%first_column
      yylsa(yylsp)%last_line = yylsa(yylsp-1)%last_line
      yylsa(yylsp)%last_column = yylsa(yylsp-1)%last_column
    else
      yylsa(yylsp)%last_line = yylsa(yylsp-1+yylen)%last_line
      yylsa(yylsp)%last_column = yylsa(yylsp-1+yylen)%last_column
    endif

    ! Now "shift" the result of the reduction.
    ! Determine what state that goes to,
    ! based on the state we popped back to
    ! and the rule number reduced by.
    yyn = yyr1(yyn)
    yystat = yypgoto(yyn - YYNTBA) + yyssa(yyssp)
    ! (NB: don't even think of combining the nested IFs into a single
    ! condition.  FORTAN-77 is entitled to reorder them, therefore segment
    ! violations are possible -- VAX Fortran does so ...).
    if (yystat.ge.0 .and. yystat.le.YYLAST) then
      if (yycheck(yystat).eq.yyssa(yyssp)) then
        yystat = yytable(yystat)
      else
        yystat = yydefgoto(yyn - YYNTBA)
      endif
    else
      yystat = yydefgoto(yyn - YYNTBA)
    endif

    ! Push a new state
    goto 90007

    ! here on detecting error
90004 continue

    ! If not already recovering from an error.
    if (yyerst.eq.0) then
      yynerr = yynerr + 1
      call yyerro ('parse error:')
    endif

    ! The following algorithm fails because YYN + I might exceed the
    ! bounds of YYCHK.

    !cc c If not already recovering from an error, report this one verbosely.
    !cc       if (yyerst.eq.0) then
    !cc      yynerr = yynerr + 1
    !cc      yyn = yypact(yystat)
    !cc
    !cc c Count the possibilities
    !cc      if (yyn.gt.YYFLAG .and. yyn.lt.YYLAST) then
    !cc         i1 = 0
    !cc         i2 = 0
    !cc 23005           if (yytnam(i1).ne.' ') then
    !cc            if (yychk(yyn+i1).eq.i1) i2 = i2 + 1
    !cc            i1 = i1 + 1
    !cc            goto 23005
    !cc         endif
    !cc
    !cc             call yyerro ('parse error:')
    !cc
    !cc c If there are less than five possibilities, print them.
    !cc             if (i2.lt.5) then
    !cc                i1 = 0
    !cc                i2 = 0
    !cc 23008          if (yytnam(i1).ne.' ') then
    !cc                   if (yychk(yyn+i1).eq.i1) then
    !cc                      if (i2.eq.0) then
    !cc                         call yyerro ('expecting `', yytnam(i1), '''')
    !cc                      else
    !cc                         call yyerro ('       or `', yytnam(i1), '''')
    !cc                      endif
    !cc                      i2 = i2 + 1
    !cc                   endif
    !cc                   i1 = i1 + 1
    !cc                   goto 23008
    !cc                endif
    !cc             endif
    !cc          else
    !cc             call yyerro ('parse error:')
    !cc          endif
    !cc       endif

    ! here on error raised explicitly by an action
90005 continue

    ! if just tried and failed to reuse lookahead token after an error,
    ! discard it
    if (yyerst.eq.3) then

      ! return failure if at end of input
      if (yychar.eq.YYEOF) then
        yyparse = YYABRT
        return
      endif

      if(yydbg)                                                      &
        &        write (stderr, *) 'Discarding token ', yychar,            &
        &        ' (', yytname(yychai), ')'
      yychar = YYEMPT

    endif

    ! Else will try to reuse lookahead token
    ! after shifting the error token.
    ! Each real token shifted decrements this
    yyerst = 3
    ! Jump to the error handler
    goto 90003

    ! current state does not do anything special for the error token.
90002 continue

    ! pop the current state because it cannot handle the error token
90006 continue

    if (yyssp.eq.1) then
      yyparse = YYABRT
      return
    endif

    yyssp = yyssp - 1
    yystat = yyssa(yyssp)
    yyvsp = yyvsp - 1
    yylsp = yylsp - 1

    if (yydbg) then
      write (stderr, *) 'Error: state stack now'
      do i1 = 1, yyssp
        write (stderr, *) '  ', yyssa(yyssp)
      enddo
    endif

    ! Error handler
90003 continue

    yyn = yypact(yystat)
    ! Jump to the default error action
    if (yyn.eq.YYFLAG) goto 90002

    yyn = yyn + YYTERR
    ! Jump to the default error action
    ! (NB: see above for the reason why this can't be one put into a
    ! single statement).
    if (yyn.lt.0 .or. yyn.gt.YYLAST) then
      goto 90002
    else
      if (yycheck(yyn).ne.YYTERR) goto 90002
    endif

    ! Pop the stack iff YYN == 0 or YYN == YYFLAG
    yyn = yytable(yyn)
    if (yyn.lt.0) then
      if (yyn.eq.YYFLAG) goto 90006
      ! Compute rule number and reduce
      yyn = -yyn
      goto 90008
    else if (yyn.eq.0) then
      goto 90006
    endif

    if (yyn.eq.YYFIN) then
      ! We're through: accept
      yyparse = YYACPT
      return
    endif

    if (yydbg) write (stderr, *) 'Shifting error token'

    yyvsp = yyvsp + 1
    yyvsa(yyvsp) = yylval
    yylsp = yylsp + 1
    yylsa(yylsp) = yylloc

    yystat = yyn

    ! Push a new state
    goto 90007

  end function yyparse


  ! Special actions (these are macros in the C version, here
  ! we provide subroutines for readability).

  subroutine yyerok ()

    yyerst = 0
    yyerst = 0

  end subroutine yyerok


  subroutine yyclr ()

    yychar = YYEMPT

  end subroutine yyclr

  subroutine yylloc_assign(yylloc_out, yylloc_in)
    type(YYLTYPE), intent(out) :: yylloc_out
    type(YYLTYPE), intent(in)  :: yylloc_in

    yylloc_out%first_line   = yylloc_in%first_line
    yylloc_out%last_line    = yylloc_in%last_line
    yylloc_out%first_column = yylloc_in%first_column
    yylloc_out%last_column  = yylloc_in%last_column
  end subroutine yylloc_assign

! line 391 "xpathparser.y"


 subroutine yyerro(msg)
   character(len=*), intent(in) :: msg

   print*,msg
 end subroutine yyerro

 subroutine parse_init(string)
   character(len=*) :: string
   
   call init_tokenizer(string)

 end subroutine parse_init

 integer function yylex()

   type(token_type) :: t

   call clear_xpath_type(yylval)
   t = next_token()	   
   if (t%token == -1) then
     yylex = 0
   elseif (t%token == -10) then
     yylex = -1
   else
     yylval = t % value
     yylex = t % token
   endif
   yylloc%first_line = 0
   yylloc%last_line = 0
   yylloc%first_column = 0
   yylloc%last_column = 0

 end function yylex
end module parse_bison90
