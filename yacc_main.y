%{
 #include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define VLIMIT	100000	//It is limit of alphanumeric variables
struct varlist{		//It holds list of all alphanumeric variable names
	char *isim;	//name of variable
	double value;	//value of variable
	int type;
}varlist[VLIMIT]; //It is symbol table of my compiler
//| VAR { $$ = $1->value;}
 //Funciton checks whether the variable has created or not
struct varlist *symlook();
 void yyerror(char *);	// It is the error checker
 int yylex(void);
int enteredif = 0;
int check= 0;
%}
%union {
double deger;
 struct varlist *varlp;
}
%token assign
%token plus minus mul divison
%token gr gre
%token sm sme
%token eq neq
%token declr
%token soyle
%token endst
%token ty_tams
%token ty_harf
%token ty_rassayi
%token <deger> TAMSAYI HARF RASSAYI // It is the token which consist integer values
%token <varlp> VAR	//It is the token which consist variables
%token tirnak
%token kosul
%token AND OR XOR XNOR
%token openb closeb
%token rightpar leftpar
%token dongu
%token devam
%token aminus aplus amult adiv
%token basla
%token incr decr
%right assign
%right "+=" "-=" "*=" "/="
%left "||"
%left "&|" "'&|"
%left "&&" 
%left "==" "!="
%left '>' '<' ">=" "<="
%left plus minus	// + and - is left associate
%left '*' '/'	// * and / is left associate and also have more precedence because I wrote it below of + and -
%right "++" "--"
%left '(' ')'
%type <deger> const;
%type <deger> expr;
%%

program: statement_list
	| 
;
statement_list: statement   '\n'
 | statement_list statement '\n'
;
//Statement can be an expression or an assignment(ın assignment we assign the number to our union deger field
statement: basla {if(check){yyerror("Syntax error");return 0;} check=1;}
 |ty_tams declr VAR assign expr endst { $3->value = $5; $3->type = 0;if(check==0){yyerror("Syntax error");return 0;}}	
 | ty_rassayi declr VAR assign expr endst	{ $3->value = $5; $3->type = 1;if(check==0){yyerror("Syntax error");return 0;}}
 | ty_harf declr VAR assign tirnak VAR tirnak endst	{$3->type = 2; int temp = $6->isim[0]; $3->value =temp;if(check==0){yyerror("Syntax error");return 0;}} 
 | ty_harf declr VAR assign tirnak HARF tirnak endst	{$3->type = 2; if(check==0){yyerror("Syntax error");return 0;}} 
 | kosul rightpar expr leftpar openb {enteredif ++; if(check==0){yyerror("Syntax error");return 0;}}
 | dongu rightpar expr  leftpar devam openb{enteredif++; if(check==0){yyerror("Syntax error");return 0;}}
 | expr	{if(check==0){yyerror("Syntax error");return 0;}}
 | closeb {enteredif--; if(enteredif<0) {yyerror("Syntax error"); return 0;} if(check==0){yyerror("Syntax error");return 0;}}
 | '\n'
 ;
	
expr: const AND expr { $$ = $1 && $3; }
 | const AND rightpar expr leftpar 	{ $$ = $1 && $4; }
 |rightpar expr leftpar AND expr	{ $$ = $2 && $5; }
 | const plus expr { $$ = $1 + $3; }
 | const plus rightpar expr leftpar 	{ $$ = $1 + $4; }
 |rightpar expr leftpar plus expr	{ $$ = $2 + $5; }
 | const minus expr { $$ = $1 - $3; }
 | const minus rightpar expr leftpar 	{ $$ = $1 - $4; }
 |rightpar expr leftpar minus expr	{ $$ = $2 - $5; }
 | const mul expr { $$ = $1 * $3; }
 | const mul rightpar expr leftpar 	{ $$ = $1 * $4; }
 |rightpar expr leftpar mul expr	{ $$ = $2 * $5; }
 | const divison expr { if($3 == 0) { printf("ERROR: Division by 0!\n"); return 0;} $$ = $1 / $3; }
 | const divison rightpar expr leftpar 	{ if($4 == 0) { printf("ERROR: Division by 0!\n"); return 0;} $$ = $1 / $4; }
 |rightpar expr leftpar divison expr	{ if($5 == 0) { printf("ERROR: Division by 0!\n"); return 0;} $$ = $2 / $5; }
 | const gr expr { $$ = $1 > $3; }
 | const gr rightpar expr leftpar 	{ $$ = $1 > $4; }
 |rightpar expr leftpar gr expr	{ $$ = $2 > $5; }
 | const gre expr { $$ = $1 >= $3; }
 | const gre rightpar expr leftpar 	{ $$ = $1 >= $4; }
 |rightpar expr leftpar gre expr	{ $$ = $2 >= $5; }
 | const sm expr { $$ = $1 < $3; }
 | const sm rightpar expr leftpar 	{ $$ = $1 < $4; }
 |rightpar expr leftpar sm expr	{ $$ = $2 < $5; }
 | const sme expr { $$ = $1 <= $3; }
 | const sme rightpar expr leftpar 	{ $$ = $1 <= $4; }
 |rightpar expr leftpar sme expr	{ $$ = $2 <= $5; }
 | const eq expr { $$ = $1 == $3; }
 | const eq rightpar expr leftpar 	{ $$ = $1 == $4; }
 |rightpar expr leftpar eq expr	{ $$ = $2 == $5; }
 | const neq expr { $$ = $1 != $3; }
 | const neq rightpar expr leftpar 	{ $$ = $1 != $4; }
 |rightpar expr leftpar neq expr	{ $$ = $2 != $5; }
 | const OR expr { $$ = $1 || $3; }
 | const OR rightpar expr leftpar 	{ $$ = $1 || $4; }
 |rightpar expr leftpar OR expr	{ $$ = $2 || $5; }
 | const XOR expr { $$ = ((!$1) && $3 ) || ($1 && (!$3)); }
 | const XOR rightpar expr leftpar 	{ $$ = ((!$1) && $4 ) || ($1 && (!$4)); }
 |rightpar expr leftpar XOR expr	{ $$ = ((!$2) && $5 ) || ($2 && (!$5)); }
 | const XNOR expr { $$ = ($1 && $3 ) || ((!$1) && (!$3)); }
 | const XNOR rightpar expr leftpar 	{ $$ = ($1 && $4 ) || ((!$1) && (!$4)); }
 |rightpar expr leftpar XNOR expr	{ $$ = ($2 && $5 ) || ((!$2) && (!$5)); }
 | VAR assign expr endst{ $1->value = $3; $$ = $1->value;}
 | VAR aplus expr endst{ $1->value += $3; $$ = $1->value;}
 | VAR aminus expr endst{ $1->value -= $3; $$ = $1->value;}
 | VAR amult expr endst{ $1->value *= $3; $$ = $1->value;}
 | VAR adiv expr endst{  if($3 == 0) { printf("ERROR: Division by 0!\n"); return 0;} $1->value /= $3; $$ = $1->value;}
 | VAR incr endst{$$ = $1->value++;}
 | VAR decr endst{$$ = $1->value--;}
 | minus expr {$$ = -$2;}
 | const
 ;
const: TAMSAYI	{}
 | RASSAYI
 | VAR { $$ = $1->value;}
 | soyle VAR endst { $$ = $2->value; 
			if($2->type==0) {int temp = $2->value;printf("%d\n",temp);}
			else if($2->type==2) {int temp = $2->value;printf("%c\n",temp);}
			else {printf("%lf\n",$2->value);}
			}
;
	

%%
void yyerror(char *s) {
 fprintf(stderr, "%s\n", s);
}
int main(void) {
 yyparse();
 return 0;
}
struct varlist * 
symlook(s)
char *s;
{
 char *p;
 struct varlist *sp;
 for(sp = varlist; sp < &varlist[VLIMIT]; sp++) {
 if(sp->isim && !strcmp(sp->isim, s))
 return sp;
 if(!sp->isim) {
 sp->isim = strdup(s);
 return sp;
 }
 /* otherwise continue to next */
 }
 //yyerror("Too many symbols"); 
 //exit(1); /* cannot continue */
}
