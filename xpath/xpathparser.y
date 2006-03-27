/* This file based on one originally bearing this copyright notice:
Copyright (C) 1999 by Dr. Dieter Maurer <dieter@handshake.de>
D-66386 St. Ingbert, Eichendorffstr. 23, Germany

		All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
modified copies and that
both that copyright notice and this permission notice appear in
supporting documentation.
*/

/* Heavily modified for Fortran by Toby White <tow21@cam.ac.uk> */

%{

  use tokenizer
  use m_xpath_types, only: YYVALTYPE => xpath_type
  use m_xpath_types, only: currentLocation, rootNode

  implicit none

%}

%token WILDCARDNAME NODETYPE FUNCTIONNAME AXISNAME
%token AXISSUF
%token LITERAL NUMBER
%token VARIABLEREFERENCE
%token PARENT
%token AND OR
%token LE GE
%token NE
%token MULTIPLYOPERATOR DIV MOD QUO
%start TopExpr

%%

TopExpr: Expr {  
    $$ = xpath()
}
;

/* [1] */
LocationPath: RelativeLocationPath
| AbsoluteLocationPath
;


/* [2] */
AbsoluteLocationPath: '/' {
  
    $$ = locationPath(RootNode)

}
| '/' RelativeLocationPath {
  
    $$ = locationPath($2)

}
| AbbreviatedAbsoluteLocationPath
;


/* [3] */
RelativeLocationPath: Step {
    $$ = addStep(currentLocation, $1)
}
| RelativeLocationPath '/' Step {
    $$ = addStep($1, $3)
}
| AbbreviatedRelativeLocationPath
;


/* [4] */
Step: Basis predicates {
  
    continue

}
| AbbreviatedStep
;

predicates: /* empty */ {
  
    continue

}
| predicates Predicate {
  
    continue

}
;


/* [5] */
Basis: AXISNAME AXISSUF NodeTest {
  
    continue

}
| AbbreviatedBasis
;


/* [6] -- handled lexically */

/* [7]
   argument handled by semantic function
*/
NodeTest: WILDCARDNAME
| NODETYPE '(' arglist ')' {
  
    continue

}
;

arglist: /* empty */ {
  
    continue

}
| args
;

args: Expr {
  
    continue

}
| args ',' Expr {
  
    continue

}
;


/* [8] */
Predicate: '[' PredicateExpr ']' {
  
    continue

}
;


/* [9] */
PredicateExpr: Expr
;


/* [10] */
AbbreviatedAbsoluteLocationPath: '/' '/' RelativeLocationPath {
  
    continue

}
;


/* [11] */
AbbreviatedRelativeLocationPath: RelativeLocationPath '/' '/' Step {
  
    continue

}
;


/* [12] */
AbbreviatedStep: '.' {
  
    continue

}
| PARENT {
  
    continue

}
;


/* [13] */
AbbreviatedBasis: NodeTest {
  
    continue

}
| '@' NodeTest {
  
    continue

}
;


/* [14] */
Expr: OrExpr
;


/* [15] */
PrimaryExpr: VARIABLEREFERENCE
| '(' Expr ')' {
  
    continue

}
| LITERAL {
  
    continue

}

| NUMBER {
  
    continue

}

| FunctionCall
;


/* [16]
   Note: functions are responsible to check their argument numbers
         this is not done syntactically
*/
FunctionCall: FUNCTIONNAME '(' arglist ')' {
  
    continue

}
;


/* [17] -- done above*/


/* [18] */
UnionExpr:	PathExpr
| UnionExpr '|' PathExpr {
  
    continue

}
;


/* [19] */
PathExpr: LocationPath
| FilterExpr
| FilterExpr '/' RelativeLocationPath {
  
    continue

}
| FilterExpr '/' '/' RelativeLocationPath {
  
    continue

}
;


/* [20] */
FilterExpr: PrimaryExpr
| FilterExpr Predicate {
  
    continue

}
;


/* [21] */
OrExpr: AndExpr
| OrExpr OR AndExpr {
  
    continue

}
;


/* [22] */
AndExpr: EqualityExpr
| AndExpr AND EqualityExpr {
  
    continue

}
;


/* [23] */
EqualityExpr: RelationalExpr
| EqualityExpr '=' RelationalExpr {
  
    continue

}
| EqualityExpr NE RelationalExpr {
  
    continue

}
;


/* [24] */
RelationalExpr: AdditiveExpr
| RelationalExpr '<' AdditiveExpr {
  
    continue

}
| RelationalExpr '>' AdditiveExpr {
  
    continue

}
| RelationalExpr LE  AdditiveExpr {
  
    continue

}
| RelationalExpr GE  AdditiveExpr {
  
    continue

}
;


/* [25] */
AdditiveExpr:	MultiplicativeExpr
| AdditiveExpr '+' MultiplicativeExpr {
  
    continue

}
| AdditiveExpr '-' MultiplicativeExpr {
  
    continue

}
;


/* [26] */
MultiplicativeExpr: UnaryExpr
| MultiplicativeExpr MULTIPLYOPERATOR UnaryExpr {
  
    continue

}
| MultiplicativeExpr DIV UnaryExpr {
  
    continue

}
| MultiplicativeExpr MOD UnaryExpr {
  
    continue

}
| MultiplicativeExpr QUO UnaryExpr {
  
    continue

}
;


/* [27] */
UnaryExpr: UnionExpr
| '-' UnaryExpr {
  
    continue

}
;

%%

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
