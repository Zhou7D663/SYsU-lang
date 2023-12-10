%{
#include <cctype>
#include <cstdio>
#include <string>
#define YYEOF 0
int yylex();

int main() {
  do {
  } while (yylex() != YYEOF);
}
std::string yyloc = "<stdin>";
int yyrow = 1, yycolumn = 1, yycolpre = 1, yystartofline = 1, yyleadingspace = 0;
char yyinfo[50] = "\0";
void getOtherInfo() {
   if(yyleadingspace && yystartofline) {
    std::sprintf(yyinfo, "[StartOfLine] [LeadingSpace]");
    yystartofline = 0;
    yyleadingspace = 0;
   }
   else if(yystartofline) {
    std::sprintf(yyinfo, "[StartOfLine]");
    yystartofline = 0;
   }
   else if(yyleadingspace) {
    std::sprintf(yyinfo, "[LeadingSpace]");
    yyleadingspace = 0;
   }
}
void myPrintf(const char * str) {
  getOtherInfo();
  std::fprintf(yyout, "%s '%s'\t %s\tLoc=<%s:%d:%d>\n", str, yytext, yyinfo, yyloc.c_str(),
               yyrow, yycolumn - yyleng);
  yyinfo[0] = '\0';
}
#define YY_USER_ACTION                                                         \
  do {                                                                         \
    yycolumn += yyleng;                                                        \
  } while (0);
%}
%option noyywrap
%%
#.* {
  std::string s(yytext);
  auto l = s.find("\""), r = s.rfind("\"");
  yyloc = s.substr(l + 1, r - l - 1);
  for (int i = 0; i < s.size(); ++i)
    if (std::isdigit(s[i])) {
      for (yyrow = 0; i < s.size() && std::isdigit(s[i]); ++i)
        yyrow = yyrow * 10 + s[i] - '0';
      --yyrow;
      break;
    }
}
\n {
  ++yyrow;
  yystartofline = 1;
  yyleadingspace = 0;
  yycolpre = yycolumn;
  yycolumn = 1;
}
[\r\f\v] {}
[\t ] {
  yyleadingspace = 1;
}
\/\/.* {}
\/\*.*\*\/ {}
\"(\\.|[^\"\\])*\" {
  myPrintf("string_literal");
  return ~YYEOF;
}
int {
  myPrintf("int");
  return ~YYEOF;
}
const {
  myPrintf("const");
  return ~YYEOF;
}
if {
  myPrintf("if");
  return ~YYEOF;
}
else {
  myPrintf("else");
  return ~YYEOF;
}
while {
  myPrintf("while");
  return ~YYEOF;
}
break {
  myPrintf("break");
  return ~YYEOF;
}
continue {
  myPrintf("continue");
  return ~YYEOF;
}
void {
  myPrintf("void");
  return ~YYEOF;
}
return {
  myPrintf("return");
  return ~YYEOF;
}
float {
  myPrintf("float");
  return ~YYEOF;
}
long {
  myPrintf("long");
  return ~YYEOF;
}
do {
  myPrintf("do");
  return ~YYEOF;
}
char {
  myPrintf("char");
  return ~YYEOF;
}

\( {
  myPrintf("l_paren");
  return ~YYEOF;
}
\) {
  myPrintf("r_paren");
  return ~YYEOF;
}
\{ {
  myPrintf("l_brace");
  return ~YYEOF;
}
\} {
  myPrintf("r_brace");
  return ~YYEOF;
}
\[ {
  myPrintf("l_square");
  return ~YYEOF;
}
\] {
  myPrintf("r_square");
  return ~YYEOF;
}
; {
  myPrintf("semi");
  return ~YYEOF;
}
(0x([0-9a-fA-F]+(\.[0-9a-fA-F]*)?|[0-9a-fA-F]*\.[0-9a-fA-F]+)([pP][-+]?[0-9]+)?)|(([0-9]+(\.[0-9]*)?|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?) {
  myPrintf("numeric_constant");
  return ~YYEOF;
}
, {
  myPrintf("comma");
  return ~YYEOF;
}
\+ {
  myPrintf("plus");
  return ~YYEOF;
}
- {
  myPrintf("minus");
  return ~YYEOF;
}
! {
  myPrintf("exclaim");
  return ~YYEOF;
}
\* {
  myPrintf("star");
  return ~YYEOF;
}
\/ {
  myPrintf("slash");
  return ~YYEOF;
}
% {
  myPrintf("percent");
  return ~YYEOF;
}
\< {
  myPrintf("less");
  return ~YYEOF;
}
> {
  myPrintf("greater");
  return ~YYEOF;
}
\<= {
  myPrintf("lessequal");
  return ~YYEOF;
}
>= {
  myPrintf("greaterequal");
  return ~YYEOF;
}
== {
  myPrintf("equalequal");
  return ~YYEOF;
}
!= {
  myPrintf("exclaimequal");
  return ~YYEOF;
}
= {
  myPrintf("equal");
  return ~YYEOF;
}
&& {
  myPrintf("ampamp");
  return ~YYEOF;
}
\|\| {
  myPrintf("pipepipe");
  return ~YYEOF;
}
[a-zA-Z_][a-zA-Z_0-9]* {
  myPrintf("identifier");
  return ~YYEOF;
}
<<EOF>> {
  std::fprintf(yyout, "eof ''\t\tLoc=<%s:%d:%d>\n", yyloc.c_str(), yyrow - 1,
               yycolpre - yyleng);
  return YYEOF;
}
\.\.\. {
  myPrintf("ellipsis");
  return ~YYEOF;
}
. {
  myPrintf("unknown");
  return ~YYEOF;
}
%%