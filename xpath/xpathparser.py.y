/* $Id: xpathparser.y,v 1.1 1999/08/03 21:05:57 dieter Exp dieter $ */
/*
Copyright (C) 1999 by Dr. Dieter Maurer <dieter@handshake.de>
D-66386 St. Ingbert, Eichendorffstr. 23, Germany

		All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
modified copies and that
both that copyright notice and this permission notice appear in
supporting documentation.

Dieter Maurer DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL Dieter Maurer
BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
*/

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
  /*py
    $$= yyenv.TopExpr($1,yyenv)
  */
}
;

/* [1] */
LocationPath: RelativeLocationPath
| AbsoluteLocationPath
;


/* [2] */
AbsoluteLocationPath: '/' {
  /*py
    $$= yyenv.LocationPath(1)
  */
}
| '/' RelativeLocationPath {
  /*py
    $$= yyenv.makeAbsolute($2)
  */
}
| AbbreviatedAbsoluteLocationPath
;


/* [3] */
RelativeLocationPath: Step {
  /*py
    $$= yyenv.addStep(yyenv.LocationPath(0),$1)
  */
}
| RelativeLocationPath '/' Step {
  /*py
    $$= yyenv.addStep($1,$3)
  */
}
| AbbreviatedRelativeLocationPath
;


/* [4] */
Step: Basis predicates {
  /*py
    $$= $1[0]($1[1],$2)
  */
}
| AbbreviatedStep
;

predicates: /* empty */ {
  /*py
    $$= []
  */
}
| predicates Predicate {
  /*py
    $$= $1; $$.append($2)
  */
}
;


/* [5] */
Basis: AXISNAME AXISSUF NodeTest {
  /*py
    $$= ($1,$3)
  */
}
| AbbreviatedBasis
;


/* [6] -- handled lexically */

/* [7]
   argument handled by semantic function
*/
NodeTest: WILDCARDNAME
| NODETYPE '(' arglist ')' {
  /*py
    $$= apply($1,$3)
  */
}
;

arglist: /* empty */ {
  /*py
    $$= []
  */
}
| args
;

args: Expr {
  /*py
    $$= [$1]
  */
}
| args ',' Expr {
  /*py
    $$= $1; $$.append($3)
  */
}
;


/* [8] */
Predicate: '[' PredicateExpr ']' {
  /*py
    $$= $2
  */
}
;


/* [9] */
PredicateExpr: Expr
;


/* [10] */
AbbreviatedAbsoluteLocationPath: '/' '/' RelativeLocationPath {
  /*py
    $$= yyenv.addDescendantsPath(yyenv.LocationPath(1),$3)
  */
}
;


/* [11] */
AbbreviatedRelativeLocationPath: RelativeLocationPath '/' '/' Step {
  /*py
    $$= yyenv.addDescendantsStep($1,$4)
  */
}
;


/* [12] */
AbbreviatedStep: '.' {
  /*py
    $$= yyenv.self(yyenv.node(),[])
  */
}
| PARENT {
  /*py
    $$= yyenv.parent(yyenv.node(),[])
  */
}
;


/* [13] */
AbbreviatedBasis: NodeTest {
  /*py
    $$= (yyenv.child,$1)
  */
}
| '@' NodeTest {
  /*py
    $$= (yyenv.attribute,$2)
  */
}
;


/* [14] */
Expr: OrExpr
;


/* [15] */
PrimaryExpr: VARIABLEREFERENCE
| '(' Expr ')' {
  /*py
    $$= $2
  */
}
| LITERAL {
  /*py
    $$= yyenv.String($1)
  */
}

| NUMBER {
  /*py
    $$= yyenv.Number($1)
  */
}

| FunctionCall
;


/* [16]
   Note: functions are responsible to check their argument numbers
         this is not done syntactically
*/
FunctionCall: FUNCTIONNAME '(' arglist ')' {
  /*py
    $$= yyenv.Function($1,$3)
  */
}
;


/* [17] -- done above*/


/* [18] */
UnionExpr:	PathExpr
| UnionExpr '|' PathExpr {
  /*py
    $$= yyenv.union($1,$3)
  */
}
;


/* [19] */
PathExpr: LocationPath
| FilterExpr
| FilterExpr '/' RelativeLocationPath {
  /*py
    $$= yyenv.addPathToExpr($1,$3)
  */
}
| FilterExpr '/' '/' RelativeLocationPath {
  /*py
    $$= yyenv.addDescendantsPathToExpr($1,$4)
  */
}
;


/* [20] */
FilterExpr: PrimaryExpr
| FilterExpr Predicate {
  /*py
    $$= yyenv.filter($1,$2)
  */
}
;


/* [21] */
OrExpr: AndExpr
| OrExpr OR AndExpr {
  /*py
    $$= $2($1,$3)
  */
}
;


/* [22] */
AndExpr: EqualityExpr
| AndExpr AND EqualityExpr {
  /*py
    $$= $2($1,$3)
  */
}
;


/* [23] */
EqualityExpr: RelationalExpr
| EqualityExpr '=' RelationalExpr {
  /*py
    $$= yyenv.Eq($1,$3)
  */
}
| EqualityExpr NE RelationalExpr {
  /*py
    $$= yyenv.Ne($1,$3)
  */
}
;


/* [24] */
RelationalExpr: AdditiveExpr
| RelationalExpr '<' AdditiveExpr {
  /*py
    $$= yyenv.Lt($1,$3)
  */
}
| RelationalExpr '>' AdditiveExpr {
  /*py
    $$= yyenv.Gt($1,$3)
  */
}
| RelationalExpr LE  AdditiveExpr {
  /*py
    $$= yyenv.Le($1,$3)
  */
}
| RelationalExpr GE  AdditiveExpr {
  /*py
    $$= yyenv.Ge($1,$3)
  */
}
;


/* [25] */
AdditiveExpr:	MultiplicativeExpr
| AdditiveExpr '+' MultiplicativeExpr {
  /*py
    $$= yyenv.add($1,$3)
  */
}
| AdditiveExpr '-' MultiplicativeExpr {
  /*py
    $$= yyenv.sub($1,$3)
  */
}
;


/* [26] */
MultiplicativeExpr: UnaryExpr
| MultiplicativeExpr MULTIPLYOPERATOR UnaryExpr {
  /*py
    $$= yyenv.mult($1,$3)
  */
}
| MultiplicativeExpr DIV UnaryExpr {
  /*py
    $$= $2($1,$3)
  */
}
| MultiplicativeExpr MOD UnaryExpr {
  /*py
    $$= $2($1,$3)
  */
}
| MultiplicativeExpr QUO UnaryExpr {
  /*py
    $$= $2($1,$3)
  */
}
;


/* [27] */
UnaryExpr: UnionExpr
| '-' UnaryExpr {
  /*py
    $$= yyenv.neg($2)
  */
}
;

