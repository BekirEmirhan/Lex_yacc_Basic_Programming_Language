%{
 #include <stdlib.h>
#include <stdio.h>
#include <string.h>
 void yyerror(char *);
int var =0;
 #include "y.tab.h"
// xy' || x'y 	(x=1, y=1 -> 0) (x=0, y=0 -> 0) (x=1, y=0 -> 1) (x=0, y=1 -> 1)
// [-+=/*\n] { return *yytext; } // we take operators and necessary chars.
struct varlist *symlook();
%}
%start begin
%%
[ \t] ; //we skip all whitespace chars
"program_basla"		return basla;
"program_bitir"		{printf("OK\n");return 0;}
"soyle"		return soyle;
"+"		{return plus;}
"-"		return minus;
"*"		return mul;
"/"		return divison;
"&&"		return AND;
"'&|"		return XNOR;
"||"		return OR;
">"		return gr;
"<"		return sm;
">="		return gre;
"<="		return sme;
"=="		return eq;
"!="		return neq;
"harf"		return ty_harf;
"tamsayi"	return ty_tams;
"rassayi"	return ty_rassayi;
";"		return endst;
":"		return declr;
"'"		return tirnak;
"kontrol"	return kosul;
"eger"		return dongu;
"devam_et"	return devam;
"("		return rightpar;
")"		return leftpar;
"{"		return openb;
"}"		return closeb;
"="		{return assign;}
"+="		return aplus;
"-="		return aminus;
"*="		return amult;
"/="		return adiv;
"&|"		return XOR;
"++"		return incr;
"--"		return decr;
[a-zA-Z]+[a-zA-Z0-9]*  {	// Lex read alphanumeric string
yylval.varlp=symlook(yytext);
 //In our union we have varlp which holds name of variable we assign alphanumaric name to it and duplicate with strdup function.
return VAR;	//we return VAR token
}

[0-9]*[.][0-9]+ {
	yylval.deger = atof(yytext);
	return RASSAYI;
}

[0-9]+ {	// Lex read if it is integer and return TAMSAYI token.
yylval.deger = atoi(yytext);
 return TAMSAYI;
 }
.	{return HARF;}
[\n] { return *yytext; } // we take operators and necessary chars.
%% 
int yywrap(void) {
 return 1;
}
