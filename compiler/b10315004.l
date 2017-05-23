%{
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "y.tab.h"
#define LIST     strcat(buf,yytext);
#define token(t) {LIST; return(t);}
#define tokenInteger(t,i) { LIST; return(t);}
#define tokenString(t,s) {return(t);}
#define MAX_LINE_LENG 256

int linenum = 1;
char buf[MAX_LINE_LENG];
char temp[MAX_LINE_LENG];
char *strupr(char *str);
%}
%x state
%x string_state
%%

"/*"    {BEGIN state;LIST;} 
<state>.*"*/".* {LIST;BEGIN 0;}
<state>\n   {LIST;printf("%d: %s", linenum++, buf);buf[0] = '\0';}
<state>\r   {}
<state>.*   {LIST;}

"bool"      token(BOOL);
"break"     token(BREAK);
"case"      token(CASE);
"const"     token(CONST);
"continue"  token(CONTINUE);
"default"   token(DEFAULT);
"else"      token(ELSE);
"false"     {token(FALSE); yylval.int_val = 0;}
"for"       token(FOR);
"func"      token(FUNC);
"go"        token(GO);
"if"        token(IF);
"import"    token(IMPORT);
"int"       token(INT);
"nil"       token(NIL);
"print"     token(PRINT);
"println"   token(PRINTLN);
"real"      token(REAL);
"return"    token(RETURN);
"string"    token(STRING);
"struct"    token(STRUCT);
"switch"    token(SWITCH);
"true"      {token(TRUE); yylval.int_val = 1;}
"type"      token(TYPE);
"var"       token(VAR);        
"void"      token(VOID);
"while"     token(WHILE);    
 
","     {token(',');}
":"     {token(':');}
";"     {token(';');}
"("     {token('(');}
")"     {token(')');}
"["     {token('[');}
"]"     {token(']');}
"{"     {token('{');}
"}"     {token('}');}

"+"      {token('+');}
"-"      {token('-');}
"*"      {token('*');}
"/"      {token('/');}
"%"      {token('%');}
"^"      {token('^');}
"="      {token('=');}
"<"      {token('<');}
"<="     {token(LE);}
">="     {token(BE);}
">"      {token('>');}
"=="     {token(EQ);}
"!="     {token(NEQ);}
"&"     {token('&');}
"|"     {token('|');}
"!"      {token('!');}
"+="      {token(PE);}
"-="      {token(ME);}
"*="      {token(TE);}
"/="      {token(DE);}

[a-zA-Z]([a-zA-Z]|[0-9]|_)*     { LIST;
                                yylval.string_val = malloc(sizeof(yytext)+1);
                                strcpy(yylval.string_val,yytext);
                                tokenString(ID,yytext);}

"//".* LIST;

("+"|"-"|"")([0-9]|[1-9][0-9]+)"."([0-9]*)(E|e)("+"|"-"|"")([0-9]*) {LIST; 
                                                                    yylval.string_val = malloc(sizeof(yytext)+1);
                                                                    strcpy(yylval.string_val,yytext);
                                                                    tokenString(REALCONSTANT,yytext);}

([1-9][0-9]*|0) {yylval.int_val = atoi(yytext); tokenInteger(INTEGER,atoi(yytext));}

"\"".*"\"" {
    char stringToken[MAX_LINE_LENG];
    LIST;
    int index = 0;
    for(int i = 1;i<strlen(yytext)-1;i++){
        if(yytext[i] == '\"'){
            stringToken[index] = '\"';
            i ++;
            index++;
        }
        else{
            stringToken[index] = yytext[i];
            index++;
        }
    }
    stringToken[index] = '\0';
    yylval.string_val = malloc(sizeof(stringToken)+1);
    strcpy(yylval.string_val,stringToken);
    tokenString(STRING_VAR,stringToken);
}

\n      {
        LIST;
        int num = 0;
        for(int i = 0;i<strlen(buf);i++){
            if(buf[i] == ' ' || buf[i] == '\t') num = i+1;
            else {
                break;
            }
        }
        printf("%d: %s", linenum++, buf+num);
        buf[0] = '\0';
        }

[ \t]*  {LIST;}

\r      ;

.       {
        LIST;
        }

%%


int yywrap(void){
    return -1;
}  

char *strupr(char *str){
    char *orign=str;
    for (; *str!='\0'; str++)
        *str = toupper(*str);
    return orign;
}