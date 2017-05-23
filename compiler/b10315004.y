%{
#include <stdlib.h>
#include <stdio.h>
#include "lex.yy.c"
#define Trace(t)  printf(t)

//function 宣告
int yyerror(char *s);
int insert_table();
int insert(char* symbol,int type,char* value);
int lookup(char* symbol);
void pop_table();
struct Type * getIDType(char* symbol);
void insertTypeStack(int type);
int checkLeftRight(int type1,int type2);
//struct 宣告
struct symbol
{
    int num;
    char id[MAX_LINE_LENG];
    int type;
    struct Type *newtype;
    char value[MAX_LINE_LENG];
    struct symbol *next;
};

struct symbol_table
{
    int num;
    struct symbol *symbol_top,*symbol_bottom;
    struct symbol_table *next;
    struct symbol_table *prev;
};
//專門放Type 是linked list
struct Type
{
    int type;
    struct Type *next;
};
int newinsert(char* symbol,int type,struct Type *newtype,char* value);
int newinsert_withoutLookUp(char* symbol,int type,struct Type *newtype,char* value);
int typeVal = 0;
//0 is Const ; 1 is String ; 2 is Integer ; 3 is Boolean ; 4 is Void ; 5 is Real; 6 is func
// var sum    int     =    10
//         [typeVal]    [Valtype]
int Valtype = 0;//0 is Const ; 1 is String ; 2 is Integer ; 3 is Boolean ; 4 is Void ; 5 is Real; 6 is func
//linked list預設
struct symbol_table *top_table = NULL,*bottom_table = NULL;
struct symbol_table *func_arg_table = NULL;
struct Type *typeHolder = NULL;
struct Type *ArgtypeHolder = NULL;
struct Type *typeStack = NULL;

union Value {
    int int_val;
    char *string_val;
};
%}

%token BOOL BREAK CASE CONST CONTINUE DEFAULT ELSE FOR FUNC
%token GO IF IMPORT INT NIL PRINT PRINTLN REAL RETURN STRING STRUCT
%token SWITCH TYPE VAR WHILE READ
%token  ID INTEGER REALCONSTANT STRING_VAR TRUE FALSE VOID
%token  LE BE EQ NEQ PE ME TE DE

%left '|'
%left '&'
%left '!'
%left LE BE EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/' '%'
%left '^'
%nonassoc UMINUS

%union {
  int int_val;
  char *string_val;
}
%type<string_val> ID
%type<int_val> INTEGER
%type<string_val> STRING_VAR  
%type<string_val> constant_exp        
%type<string_val> REALCONSTANT    
%type<int_val> TRUE            
%type<int_val> FALSE 
%type<int_val> type

%start program
%%
  program: definition 
          | program definition;

  definition: func 
              | var_const_de;

  var_const_de: const_de
                |var_de;

  const_de: CONST ID '=' constant_exp{
                                      typeHolder = (struct Type *) malloc(sizeof(struct Type));
                                      typeHolder->type = Valtype;
                                      typeHolder->next = NULL;
                                      struct Type *temp = (struct Type *) malloc(sizeof(struct Type));
                                      temp->type = 0;
                                      temp->next = NULL;
                                      typeHolder->next = temp;
                                      newinsert($2,Valtype,typeHolder,$4);
                                      typeHolder = NULL;
                                      /*insert($2,Valtype,"");*/};

  var_de: VAR ID type { 
                        typeHolder = (struct Type *) malloc(sizeof(struct Type));
                        typeHolder->type = $3;
                        typeHolder->next = NULL;
                        newinsert($2,$3,typeHolder,"");
                        typeHolder = NULL;
                        /*insert($2,typeVal,"");*/}
          | VAR ID type '=' exp {  
                                            typeHolder = (struct Type *) malloc(sizeof(struct Type));
                                            typeHolder->type = $3;
                                            typeHolder->next = NULL;
                                            if($3 != typeStack->type) yyerror("left and right type unmatched.\n"); 
                                            else newinsert($2,$3,typeHolder,"");
                                            //printf("%d\n",typeVal );
                                            typeHolder = NULL;
                                            /*insert($2,typeVal,"");*/}
          | VAR ID array_List type { 
                                      typeHolder = (struct Type *) malloc(sizeof(struct Type));
                                      typeHolder->type = $4;
                                      typeHolder->next = NULL;
                                      newinsert($2,$4,typeHolder,"");
                                      typeHolder = NULL;
                                      /*insert($2,typeVal,"");*/};

  array_List: '[' INTEGER_exp ']' | array_List '[' INTEGER_exp ']';

  INTEGER_exp: INTEGER '+' INTEGER
        | INTEGER '-' INTEGER
        | INTEGER '*' INTEGER
        | INTEGER '/' INTEGER
        | INTEGER '^' INTEGER
        | '-' INTEGER %prec UMINUS 
        | '(' INTEGER ')'
        | INTEGER 
        | ID { if(getIDType($1) != NULL)insertTypeStack(getIDType($1)->type);
                if(typeStack->type != 2) yyerror("ID not Integer"); }
        | func_invocation;

  constant_exp: STRING_VAR       {
                                  $$ = $1; 
                                  Valtype = 1;
                                  insertTypeStack(1);} 
                | INTEGER        {
                                  char str[50]; 
                                  sprintf(str, "%d", $1); 
                                  $$ = str; 
                                  Valtype = 2;
                                insertTypeStack(2);}
                | TRUE           {
                                  $$ = "TRUE"; 
                                  Valtype = 3;
                                insertTypeStack(3);} 
                | FALSE          {
                                  $$ = "FALSE"; 
                                  Valtype = 3;
                                insertTypeStack(3);}
                | REALCONSTANT   {
                                  $$ = $1; 
                                  Valtype = 5;
                                insertTypeStack(5);};


  func: FUNC type ID{  
                  //typeHolder 會先存function的Type              
                  typeHolder = (struct Type *) malloc(sizeof(struct Type));
                  typeHolder->type = 6;
                  typeHolder->next = NULL;
                  struct Type *temp = (struct Type *) malloc(sizeof(struct Type));
                  temp->type = $2;
                  temp->next = NULL;
                  typeHolder->next = temp;
                }

              func_arg{
                    struct Type *newtemp = typeHolder;  
                    /*while(newtemp->next != NULL){
                      printf("func : %d \n", newtemp->type);
                      newtemp = newtemp->next;
                    }
                    printf("func : %d \n", newtemp->type);*/
                    newinsert($3,6,typeHolder,"FUNCTION");
                    typeHolder = NULL;
                }
              compound;

  func_arg: '(' formal_arg ')'
            |'('  ')';

  formal_arg: ID type {
                        //ArgTypeHolder 暫存argument的type之後再放入symbol table
                        //printf("Trace : ID type\n");
                        ArgtypeHolder = (struct Type *) malloc(sizeof(struct Type));
                        ArgtypeHolder->type = $2;
                        //printf("%d\n",$2 );
                        ArgtypeHolder->next = NULL;
                        newinsert_withoutLookUp($1,$2,ArgtypeHolder,"parameter");
                        //printf("%d\n", bottom_table->symbol_bottom->type);
                        ArgtypeHolder = NULL;
                        if(typeHolder == NULL){
                          typeHolder = (struct Type *) malloc(sizeof(struct Type));
                          typeHolder->type = $2;
                          typeHolder->next = NULL;
                        }
                        else{
                          struct Type *temp = typeHolder;
                          while(temp->next != NULL){
                            temp = temp->next;
                          }
                          struct Type *newType = (struct Type *) malloc(sizeof(struct Type));
                          newType->type = $2;
                          newType->next = NULL;
                          temp->next = newType;
                        }
                      }

              | ID type ',' formal_arg{
                                        //printf("Trace : ID type ',' formal_arg\n");
                                        ArgtypeHolder = (struct Type *) malloc(sizeof(struct Type));
                                        ArgtypeHolder->type = $2;
                                        ArgtypeHolder->next = NULL;
                                        newinsert_withoutLookUp($1,$2,ArgtypeHolder,"parameter");
                                        ArgtypeHolder = NULL;
                                        if(typeHolder == NULL){
                                          typeHolder = (struct Type *) malloc(sizeof(struct Type));
                                          typeHolder->type = $2;
                                          typeHolder->next = NULL;
                                        }
                                        else{
                                          struct Type *temp = typeHolder;
                                          while(temp->next != NULL){
                                            temp = temp->next;
                                          }
                                          struct Type *newType = (struct Type *) malloc(sizeof(struct Type));
                                          newType->type = $2;
                                          newType->next = NULL;
                                          temp->next = newType;
                                        }
                                      };
  
  compound:'{'{
                insert_table(); 
                if(func_arg_table != NULL){
                  bottom_table->prev->next = func_arg_table; 
                  func_arg_table->prev = bottom_table->prev; 
                  bottom_table = func_arg_table;
                  func_arg_table = NULL;}
                } 
                s_list '}' {pop_table(); };
            | '{' '}'

  s_list: s_list_element  
          | s_list_element s_list;

  s_list_element: var_const_de 
                  | statement;

  statement: condition 
            | simple 
            | loop 
            | procedure;

  simple: ID '=' exp {  
                        if(getIDType($1) != NULL){
                          if(getIDType($1)->next != NULL){
                            if(getIDType($1)->next->type == 0) yyerror("Const can't be modified.\n");
                          }
                          else{
                            int IDType = getIDType($1)->type;
                            if(IDType != typeStack->type) {
                              yyerror("left and right type unmatched.\n");
                              //printf("%d %d\n",IDType,typeStack->type);
                            }
                          }
                        }
                        else printf("Undefined ID %s\n", $1);
                      }
          | ID '[' exp ']'
          | PRINT exp
          | PRINTLN exp
          | RETURN exp 
          | RETURN
          | READ exp;//need to fix

  exp: constant_exp
        | exp '+' exp {if(!checkLeftRight(typeStack->type,typeStack->next->type)) yyerror("left and right type unmatched.\n");}
        | exp '-' exp {if(!checkLeftRight(typeStack->type,typeStack->next->type)) yyerror("left and right type unmatched.\n");}
        | exp '*' exp {if(!checkLeftRight(typeStack->type,typeStack->next->type)) yyerror("left and right type unmatched.\n");}
        | exp '/' exp {if(!checkLeftRight(typeStack->type,typeStack->next->type)) yyerror("left and right type unmatched.\n");}
        | exp '^' exp {if(!checkLeftRight(typeStack->type,typeStack->next->type)) yyerror("left and right type unmatched.\n");}
        | '-' exp %prec UMINUS 
        | '(' exp ')' 
        | ID      {if(getIDType($1) != NULL)insertTypeStack(getIDType($1)->type);
                    else{
                      printf("Undefined ID %s\n", $1);
                    }}
        | func_invocation ;

  func_invocation: ID '(' unformal_arg ')' {
                                              struct Type *funcType = getIDType($1);
                                              int num = 0;
                                              int arg_type[MAX_LINE_LENG];
                                              while(funcType != NULL){
                                                if(num == 1) insertTypeStack(funcType->type);
                                                if(num>1) arg_type[num-2] = funcType->type;
                                                funcType = funcType->next;
                                                num++;
                                              }
                                              num = num - 3;
                                              struct Type *temp = typeStack;
                                              while(num >= 0){
                                              if(temp->type != arg_type[num]) printf("Function \"%s\" parameter type wrong \n",$1);
                                              num--;
                                              temp = temp->next;
                                           }};

  unformal_arg: exp 
                | exp ',' unformal_arg;

  type: STRING  {$$ = 1;typeVal = 1;
  insertTypeStack(1);}
        | INT   {$$ = 2; typeVal = 2;
        insertTypeStack(2);}
        | BOOL  {$$ = 3; typeVal = 3;
        insertTypeStack(3);}
        | VOID  {$$ = 4; typeVal = 4;
        insertTypeStack(4);}
        | REAL  {$$ = 5; typeVal = 5;
        insertTypeStack(5);};

  condition: IF '('  boolean_expr ')' com_state ELSE com_state
            | IF '('  boolean_expr ')' com_state;

  boolean_expr: exp logic_op exp {if(!checkLeftRight(typeStack->type,typeStack->next->type)) yyerror("left and right type unmatched.\n");}

  loop: FOR'(' boolean_expr')' com_state 
        | FOR'(' boolean_expr loop_statement_right')' com_state 
        | FOR'(' loop_statement_left boolean_expr')' com_state 
        | FOR '(' loop_statement_left boolean_expr loop_statement_right ')' com_state 
        | WHILE '(' boolean_expr ')' com_state; 

  com_state: compound 
            | statement;

  loop_statement_left: simple ';';
  
  loop_statement_right:';' simple ;

  logic_op: LE 
            | BE 
            | EQ 
            | NEQ 
            | PE 
            | ME 
            | TE 
            | DE 
            | '>' 
            | '<' 
            | '!' 
            | '&' 
            | '|';
   
  procedure: GO ID '(' unformal_arg ')';

%%

int yyerror(char *s)
{
 fprintf(stderr, "Error : %s\n", s);
 return 0;
}

int main(void)
{
 yyparse();
 pop_table();
 return 0;
}

//插入新的table
int insert_table(){
    if(top_table == NULL && bottom_table == NULL){
        struct symbol_table *new_symbol_table;
        new_symbol_table = (struct symbol_table *) malloc(sizeof(struct symbol_table));
        new_symbol_table->next = NULL;
        new_symbol_table->prev = NULL;
        new_symbol_table->num = 1;
        
        top_table = new_symbol_table;
        bottom_table = new_symbol_table;
    }

    else{
        struct symbol_table *new_symbol_table;
        new_symbol_table = (struct symbol_table *) malloc(sizeof(struct symbol_table));
        new_symbol_table->next = NULL;
        new_symbol_table->prev = bottom_table;
        new_symbol_table->num = 1;
        
        bottom_table = new_symbol_table;
    }
}
//舊的insert symbol目前沒有用到
int insert(char* symbol,int type,char* value){
    if(top_table == NULL && bottom_table == NULL){
        insert_table();
    }
    struct symbol *new_symbol;
    new_symbol = (struct symbol *) malloc(sizeof (struct symbol));
    new_symbol->num = bottom_table->num;
    strcat(new_symbol->id,symbol);
    new_symbol->type = type;
    strcat(new_symbol->value,value);
    new_symbol->next = NULL;

    if(bottom_table->num == 1){
         bottom_table->symbol_top = NULL;
         bottom_table->symbol_bottom = NULL;
    }

    if(bottom_table->symbol_bottom != NULL){
        if(lookup(symbol) == -1){
            bottom_table->symbol_bottom->next = new_symbol;
            bottom_table->symbol_bottom = new_symbol;
            bottom_table->num++;
        }
    }
    else{
        if(lookup(symbol) == -1){
            bottom_table->symbol_top = new_symbol;
            bottom_table->symbol_bottom = new_symbol;
            bottom_table->num++;
        }
    }

    return new_symbol->num;
}
//新的insert symbol
int newinsert(char* symbol,int type,struct Type *newtype,char* value){
    if(top_table == NULL && bottom_table == NULL){
        insert_table();
    }
    struct symbol *new_symbol;
    new_symbol = (struct symbol *) malloc(sizeof (struct symbol));
    new_symbol->num = bottom_table->num;
    strcat(new_symbol->id,symbol);
    new_symbol->type = type;
    strcat(new_symbol->value,value);
    new_symbol->next = NULL;
    new_symbol->newtype = newtype;

    if(bottom_table->num == 1){
         bottom_table->symbol_top = NULL;
         bottom_table->symbol_bottom = NULL;
    }

    if(bottom_table->symbol_bottom != NULL){
        if(lookup(symbol) == -1){
            bottom_table->symbol_bottom->next = new_symbol;
            bottom_table->symbol_bottom = new_symbol;
            bottom_table->num++;
        }
    }
    else{
        if(lookup(symbol) == -1){
            bottom_table->symbol_top = new_symbol;
            bottom_table->symbol_bottom = new_symbol;
            bottom_table->num++;
        }
    }

    return new_symbol->num;
}
//不管之前有沒有存在過ID都插入symbol table，用於func argument
int newinsert_withoutLookUp(char* symbol,int type,struct Type *newtype,char* value){
    if(func_arg_table == NULL) {
      func_arg_table = (struct symbol_table *) malloc(sizeof(struct symbol_table));
      func_arg_table->num = 1;}
    struct symbol *new_symbol;
    new_symbol = (struct symbol *) malloc(sizeof (struct symbol));
    new_symbol->num = func_arg_table->num;
    strcat(new_symbol->id,symbol);
    new_symbol->type = type;
    strcat(new_symbol->value,value);
    new_symbol->next = NULL;
    new_symbol->newtype = newtype;
    
    if(func_arg_table->num == 1){
         func_arg_table->symbol_top = NULL;
         func_arg_table->symbol_bottom = NULL;
    }

    if(func_arg_table->symbol_bottom != NULL){
      func_arg_table->symbol_bottom->next = new_symbol;
      func_arg_table->symbol_bottom = new_symbol;
      func_arg_table->num++;
    }
    else{
      func_arg_table->symbol_top = new_symbol;
      func_arg_table->symbol_bottom = new_symbol;
      func_arg_table->num++;
    }
    return new_symbol->num;
} 
//往下查看symboltable有沒有存在過的ID
int lookup(char* symbol){
    struct symbol_table *temp_table = top_table;
    while(temp_table != NULL){
        struct symbol *temp = temp_table->symbol_top;
        while(temp != NULL){
            if(strcmp(temp->id,symbol)==0) return temp->num;
            temp=temp->next;
        }
        temp_table = temp_table->next;
    }
    return -1;
}
//取得ID的Type 若沒有ID 回傳NULL
struct Type * getIDType(char* symbol){
    struct symbol_table *temp_table = top_table;
    struct Type *temp_newtype = NULL;
    while(temp_table != NULL){
        struct symbol *temp = temp_table->symbol_top;
        while(temp != NULL){
            if(strcmp(temp->id,symbol)==0) temp_newtype = temp->newtype;
            temp=temp->next;
        }
        temp_table = temp_table->next;
    }
    return temp_newtype;
}
//移除最下層Table
void pop_table(){
    struct symbol *temp = bottom_table->symbol_top;
    if(temp == NULL) bottom_table = bottom_table->prev;
    else{
      printf("\nsymbol table :\n");
      while(temp != NULL) {
            struct Type *typetemp = temp->newtype;
            printf("index:%d id_name=>%s id_type=>",temp->num,temp->id);
            while(typetemp != NULL){
              printf(" %d ",typetemp->type);
              typetemp = typetemp->next;
            }
            printf("id_value=>%s\n",temp->value);
            //printf("index:%d id_name=>%s id_type=>%d id_value=>%s\n",temp->num,temp->id,temp->type,temp->value);
            temp=temp->next;
      }
      printf("\n");
      bottom_table = bottom_table->prev;
    }
}
//將Type塞進TypeStack用於判斷exp 兩邊type是否相同
void insertTypeStack(int type){
  if(typeStack == NULL){
    typeStack = (struct Type *) malloc(sizeof(struct Type));
    typeStack->type = type;
    typeStack->next = NULL;
  }
  else
  {
    struct Type *tempStack = typeStack;
    struct Type *temp = (struct Type *) malloc(sizeof(struct Type));
    temp->type = type;
    temp->next = tempStack;
    typeStack = temp;
  }
}

int checkLeftRight(int type1,int type2){
  if(type1 == type2) return 1;
  else return 0;
}