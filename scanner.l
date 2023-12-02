%{
#include <stdio.h>
#include "y.tab.h"
extern int line;
%}

letter [A-Za-z]
digit [0-9]

identifier {letter}({letter}|{digit})*

%%

"/*"[^\n]*"*/"                                { /* ignore comments */ }
"/*"([^*]|(\*+([^*/]|[\n])))*\*+"/"           { ++line; printf("Error in line %d: Illegal comment\n", line);}

[ \t\r\a]+     { /* ignore whitespace */ }

and            { yylval.str = strdup(yytext); return AND; }
array          { yylval.str = strdup(yytext); return ARRAY; }
begin          { yylval.str = strdup(yytext); return BEGINN; }
bool           { yylval.str = strdup(yytext); return BOOL; }
call           { yylval.str = strdup(yytext); return CALL; }
case           { yylval.str = strdup(yytext); return CASE; }
char           { yylval.str = strdup(yytext); return CHAR; }
constant       { yylval.str = strdup(yytext); return CONSTANT; }
dim            { yylval.str = strdup(yytext); return DIM; }
do             { yylval.str = strdup(yytext); return DO; }
else           { yylval.str = strdup(yytext); return ELSE; }
end            { yylval.str = strdup(yytext); return END; }
false          { yylval.num = 0;              return FALSE; }
for            { yylval.str = strdup(yytext); return FOR; }
if             { yylval.str = strdup(yytext); return IF; }
input          { yylval.str = strdup(yytext); return INPUT; }
integer        { yylval.str = strdup(yytext); return INTEGER; }
not            { yylval.str = strdup(yytext); return NOT; }
of             { yylval.str = strdup(yytext); return OF; }
or             { yylval.str = strdup(yytext); return OR; }
output         { yylval.str = strdup(yytext); return OUTPUT; }
procedure      { yylval.str = strdup(yytext); return PROCEDURE; }
program        { yylval.str = strdup(yytext); return PROGRAM; }
read           { yylval.str = strdup(yytext); return READ; }
real           { yylval.str = strdup(yytext); return REAL; }
repeat         { yylval.str = strdup(yytext); return REPEAT; }
set            { yylval.str = strdup(yytext); return SET; }
stop           { yylval.str = strdup(yytext); return STOP; }
then           { yylval.str = strdup(yytext); return THEN; }
to             { yylval.str = strdup(yytext); return TO; }
true           { yylval.num = 1;              return TRUE; }
until          { yylval.str = strdup(yytext); return UNTIL; }
var            { yylval.str = strdup(yytext); return VAR; }
while          { yylval.str = strdup(yytext); return WHILE; }
write          { yylval.str = strdup(yytext); return WRITE; }

{identifier}   { yylval.str = strdup(yytext); return ID; }

[0-9]+         { yylval.num = atoi(yytext); return NUM; }

\'([^'\n]|\\.)+'    { yylval.str = strdup(yytext); return CHAR_CONSTANT; }
'[^']*[\n][^']*'    { ++line; printf("Error in line %d: Illegal character constant  %s\n", line, yytext);}

"+"            { yylval.str = strdup(yytext);return PLUS; }
"-"            { yylval.str = strdup(yytext);return MINUS; }
"*"            { yylval.str = strdup(yytext);return TIMES; }
"/"            { yylval.str = strdup(yytext);return DIVIDE; }
":="           { yylval.str = strdup(yytext);return ASSIGN; }
":"            { return COLON; }
";"            { return SEMICOLON; }
","            { return COMMA; }
"."            { return DOT; }
"("            { return LPAREN; }
")"            { return RPAREN; }
"["            { return LBRACK; }
"]"            { return RBRACK; }

"<="           { return LESSEQ; }
">="           { return GREATEREQ; }
"<"            { return LESS; }
">"            { return GREATER; }
"="            { return EQ; }

\n             { ++line;/* ignore newline */ }

.              { printf("Error in line %d: Illegal character: %s\n", line, yytext);}

%%

int yywrap() {
    return 1;
}