%{
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#define LIST     strcat(buf,yytext);
#define token(t) {LIST; printf("<%s>\n",t);}
#define tokenInteger(t,i) { LIST; printf("<%s:%d>\n",t,i);}
#define tokenString(t,s) {printf("<%s:%s>\n",t,s);}
#define MAX_LINE_LENG 256

int linenum = 1;
int num = 1;
char buf[MAX_LINE_LENG];
int insert(char* symbol);
void create();
int lookup(char* symbol);
void dump(void);
%}
%x state
%x string_state
%%

"/*"	{BEGIN state;LIST;} 
<state>.*"*/".*	{LIST;BEGIN 0;}
<state>\n	{LIST;printf("%d: %s", linenum++, buf);buf[0] = '\0';}
<state>\r  	{}
<state>.*	{LIST;}

"bool"|"break"|"case"|"const"|"continue"|"default"|"else"|"false"|"for"|"func"|"go"|"if"|"import"|"int"|"nil"|"print"|"println"|"real"|"return"|"string"|"struct"|"switch"|"true"|"type"|"var"|"void"|"while" {char temp[MAX_LINE_LENG]; strcpy(temp,yytext); strupr(temp); token(temp);}
 
","|":"|";"|"("|")"|"["|"]"|"{"|"}" token(yytext);

"+"|"-"|"*"|"/"|"ˆ"|"%"|"<"|"<="|">="|">"|"=="|"!="|"&"|"|"|"!"|"="|"+="|"-="|"*="|"/=" token(yytext);

[a-zA-Z]([a-zA-Z]|[0-9])*     {insert(yytext); LIST; tokenString("id",yytext);}

"//"[ a-z*A-Z*0-9*]* LIST;

("+"|"-"|"")([0-9]|[1-9][0-9]+)"."([0-9]*)(E|e)("+"|"-"|"")([0-9]*) {LIST; tokenString("real constant",yytext);}

([1-9][0-9]*|0)	tokenInteger("integer",atoi(yytext));

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
	tokenString("string",stringToken);
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

\r 		;

.       {
        LIST;
        printf("%d:%s\n", linenum+1, buf);
        printf("bad character:'%s'\n",yytext);
        exit(-1);
        }

%%

struct symbol
{
	int num;
	char id[MAX_LINE_LENG];
	struct symbol *next;
};

struct symbol *top,*bottom;

int insert(char* symbol){
	struct symbol *new_symbol;
	new_symbol = (struct symbol *) malloc(sizeof (struct symbol));
    new_symbol->num = num;
    strcat(new_symbol->id,symbol);
    new_symbol->next = NULL;

    if(num == 1){
    	create();
    }

    if(bottom != NULL){
    	if(lookup(symbol) == -1){
    		bottom->next = new_symbol;
    		bottom = new_symbol;
    		num++;
    	}
    }
    else{
    	top = new_symbol;
    	bottom = new_symbol;
    	num++;
    }

    return new_symbol->num;
}

void create(){
	struct symbol *top = NULL;
	struct symbol *bottom = NULL;
}

int lookup(char* symbol){
	struct symbol *temp = top;
	while(temp != NULL){
		if(strcmp(temp->id,symbol)==0) return temp->num;
       	temp=temp->next;
    }
	return -1;
}

void dump(void) {
     struct symbol *temp;
     temp=top;
     while(temp != NULL) {
           printf("index:%d, id_name:%s\n",temp->num,temp->id);
           temp=temp->next;
     }
}

int yywrap(void){
	printf("\nsymbol table :\n");
	dump();
	return -1;
}  