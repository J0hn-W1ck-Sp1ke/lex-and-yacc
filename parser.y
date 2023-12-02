%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char filename[128];
FILE* yyin;
int yylex(void);
void yyerror(const char* s);
const char* no="NO";
int line=0;

// 定义符号表项的值
typedef union Value{
    int int_value;
    char char_value[64];
}Value;
typedef struct Symbol {
    char name[64];
    char type[64];
    // 变量值可以使用联合（union）来保存不同类型的值
    Value value;
    int is_constant;//常量标志
    struct Symbol* next; // 链表中的下一个变量
}Symbol;
// 符号表的头指针
Symbol* symbol_table = NULL;
// 记录临时变量的数量
int temp_variable_count = 0;
// 符号表函数声明
void add_variable(char* name, char* type);//标识符定义
void add_constant(char* type, Value*);//常量定义
void add_temp_variable(char* type);//临时变量
void set_integer(char* name, const int);//置值
void set_char(char* name, const char*);
void set_type(char* type);
Symbol* find_variable(char* name);//寻找变量
void print_variable();

void emit(const char* op, Symbol* left, Symbol* right, Symbol* result)
{
    printf("(%s, ", op);
    if(left==NULL) printf("-, ");
    else
    {
        if(left->is_constant) 
        {
            if(strcmp(left->type, "INTEGER")==0)
            {
                 printf("%d, ", left->value.int_value);
            }
            else if(strcmp(left->type, "BOOL")==0)  
            {
                if(left->value.int_value) printf("true, "); 
                else printf("false, ");
            }
            else printf("%s, ", left->value.char_value);

        }
        else printf("%s, ", left->name);
    }

    if(right==NULL)printf("-, ");
    else
    {
        if(right->is_constant) 
        {
            if(strcmp(right->type, "INTEGER")==0)
            {
                printf("%d, ", right->value.int_value);
            }
            else if(strcmp(right->type, "BOOL")==0)  
            {
                if(right->value.int_value) printf("true, "); 
                else printf("false, ");
            }
            else printf("%s, ", right->value.char_value);

        }
        else printf("%s, ", right->name);
    }
    

    if(result==NULL) printf("-)");
    else
    {
        if(result->is_constant) 
        {
            if(strcmp(result->type, "INTEGER")==0)
            {
                 printf("%d)", result->value.int_value);
            }
            else if(strcmp(result->type, "BOOL")==0)  
            {
                if(result->value.int_value) printf("true)"); 
                else printf("false)");
            }
            else printf("%s)", result->value.char_value);
        }
        else printf("%s)", result->name);
        }
    printf("\n");
}   

%}


%token ID
%token ARRAY BEGINN BOOL CALL CASE CHAR CONSTANT DIM DO ELSE END FALSE FOR IF INPUT INTEGER OF OUTPUT PROCEDURE PROGRAM READ REAL REPEAT SET STOP THEN TO TRUE UNTIL VAR WHILE WRITE
%token SEMICOLON COMMA COLON DOT LPAREN RPAREN LBRACK RBRACK
%token NUM CHAR_CONSTANT

/*定义优先级*/
%right ASSIGN
%left OR
%left AND
%left EQ
%left LESS LESSEQ GREATER GREATEREQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%union {
    int num;
    char* str;
    struct Symbol* sym;
}

%type <str> ASSIGN OR AND EQ LESS LESSEQ GREATER GREATEREQ PLUS MINUS TIMES DIVIDE NOT
%type <num> NUM TRUE FALSE
%type <sym> ari_exp term factor
%type <sym> bool_exp bool_term bool_factor bool_constant
%type <sym> char_exp id
%type <sym> statement statement_list  
%type <str> relational_op
%type <str> id_list variable_type
%type <str> INTEGER BOOL CHAR
%type <str> ID CHAR_CONSTANT


%start program

%%

program: PROGRAM ID SEMICOLON variable_declaration BEGINN statement_list END DOT {
    //<程序> → program <标识符> ; <变量说明> <复合语句>
    // 语义动作 
}|
    error{
        yyerror("Program error");
    }

variable_declaration: VAR variable_definition{
    //<变量说明> → var <变量定义>│ε
    //print_variable();//测试变量说明
    } | 
     /* 空产生式 */
    

variable_definition: id_list COLON variable_type SEMICOLON variable_definition {
        //<变量定义> → <标识符表> ：<类型> ；<变量定义> │ <标识符表> ：<类型> ；
    }|
    id_list COLON variable_type SEMICOLON{
    }|error{
        yyerror("Variable definition error");
    }
    
    

variable_type: INTEGER {
    //<类型> → integer│bool│char
    $$ = strdup("INTEGER");
    set_type($$);
} |
               BOOL {
                   $$ = strdup("BOOL");
                   set_type($$);
               } |
               CHAR {
                   $$ = strdup("CHAR");
                   set_type($$);
               }

id_list: ID COMMA id_list {
    //<标识符表> → <标识符> ，<标识符表>│<标识符>
    if(find_variable($1)==NULL)add_variable($1, no);
    else yyerror("Identifier redefine");
} |
         ID {
            if(find_variable($1)==NULL)add_variable($1, no);
            else yyerror("Identifier redefine");
         }



statement: id ASSIGN ari_exp  {
    //<语句> → <赋值句>│<if句>│<while句>│<repeat句>│<复合句>
    
    add_temp_variable($1->type);
    $$=symbol_table;
    if(strcmp($1->type, $3->type)==0) emit($2, $3, NULL, $1);
    else yyerror("Type error");
    }|
    id ASSIGN bool_exp {
            add_temp_variable($1->type);
            $$=symbol_table;
            if(strcmp($1->type, $3->type)==0) emit($2, $3, NULL, $1);
            else yyerror("Type error");
    }|
    id ASSIGN char_exp {
            add_temp_variable($1->type);
            $$=symbol_table;
            if(strcmp($1->type, $3->type)==0) emit($2, $3, NULL, $1);
            else yyerror("Type error");
            
    }|
              IF bool_term THEN statement {
                //<if句>→ <布尔表达式> then <语句>│if <布尔表达式> then <语句> else <语句>
                  
              } |
              IF bool_term THEN statement ELSE statement {
                  
              } |
              WHILE bool_term DO statement {
                //<while句> → while <布尔表达式> do <语句>
                  
              } |
              REPEAT statement UNTIL bool_term {
                //<repeat句> → repeat <语句> until <布尔表达式>
                
              } |
              BEGINN statement_list END {
                //<复合句> → begin <语句表> end
                
              }| error{
                yyerror("Statement error");
              }

statement_list: statement SEMICOLON statement_list {
    //<语句表> → <语句> ；<语句表>│<语句>

} |
                statement {
                    
                }




ari_exp: ari_exp PLUS term{
    //<算术表达式> → <算术表达式> + <项>│<算术表达式> - <项>│<项>
            add_temp_variable($1->type);
            $$=symbol_table;
            if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
            else yyerror("Type error");
    }|
    ari_exp MINUS term{
        add_temp_variable($1->type);
        $$=symbol_table;
        if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
        else yyerror("Type error");
    }|
    term{
        $$=$1;
    }

term: factor {
    //<项> → <项> * <因子>│<项> / <因子>│<因子>
    $$=$1;
} |
         term TIMES factor {
            add_temp_variable($1->type);
            $$=symbol_table;
            if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
            else yyerror("Type error");
         } |
         term DIVIDE factor {
            add_temp_variable($1->type);
            $$=symbol_table;
            if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
            else yyerror("Type error");
         }
        //因子即是算术量
factor: id {
    //<算术量> → <整数>│<标识符>│（ <算术表达式> ）
            $$=$1;
} |
           NUM {
            Value* constant_value = (Value*)malloc(sizeof(Value));
            
            constant_value->int_value=$1;
            
            add_constant("INTEGER", constant_value);
            $$=symbol_table;
            //printf("name: %s ; type: %s ; is_constant: %d\n", $$->name, $$->type, $$->is_constant);
           } |
           LPAREN ari_exp RPAREN {
            $$=$2;
           }

bool_exp: bool_exp OR bool_term{
    //<布尔表达式> → <布尔表达式> or <布尔项>│<布尔项>
        add_temp_variable($1->type);
        $$=symbol_table;
        emit($2, $1, $3, $$);
    }|
    bool_term{
        $$=$1;
    }

bool_term: bool_factor {
    //<布尔项> → <布尔项> and <布因子>│<布因子>
        $$=$1;
} |
             bool_term AND bool_factor {
                add_temp_variable($1->type);
                $$=symbol_table;
                emit($2, $1, $3, $$);
             }

bool_factor: NOT bool_factor {
    //<布因子> → <布尔量>│not <布因子>
    //<布尔量> → <布尔常量>│<标识符>│（ <布尔表达式> ）│ <标识符> <关系符> <标识符>│<算术表达式> <关系符> <算术表达式>
    add_temp_variable($2->type);
    $$=symbol_table;
    emit($1, $2, NULL, $$);
} |
               LPAREN bool_exp RPAREN {
                $$=$2;
               } |
               bool_constant{
                $$=$1;
               }|
               id{
                $$=$1;
               }|
               id relational_op id{
                add_temp_variable("BOOL");
                $$=symbol_table;
                if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
                else yyerror("Type error");
               } |
               id relational_op ari_exp{
                add_temp_variable("BOOL");
                $$=symbol_table;
                if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
                else yyerror("Type error");
               } |
               ari_exp relational_op ari_exp {
                add_temp_variable("BOOL");
                $$=symbol_table;
                if(strcmp($1->type, $3->type)==0) emit($2, $1, $3, $$);
                else yyerror("Type error");
               }

bool_constant: TRUE{
        Value* constant_value = (Value*)malloc(sizeof(Value));
        constant_value->int_value=$1;
        add_constant("BOOL", constant_value);
        $$=symbol_table;
    }|
    FALSE{
        Value* constant_value = (Value*)malloc(sizeof(Value));
        constant_value->int_value=$1;
        add_constant("BOOL", constant_value);
        $$=symbol_table;
    }

relational_op: LESS {
    //<关系符> → <│<>│<=│>=│>│=
    $$ = strdup("<");
} |
               LESSEQ {
                   $$ = strdup("<=");
               } |
               GREATER {
                   $$ = strdup(">");
               } |
               GREATEREQ {
                   $$ = strdup(">=");
               } |
               EQ {
                   $$ = strdup("=");
               }


char_exp: CHAR_CONSTANT{
    //<字符表达式> → <字符常数>│<标识符>
        Value* constant_value = (Value*)malloc(sizeof(Value));
        strcpy(constant_value->char_value, $1);
        add_constant("CHAR", constant_value);
        $$=symbol_table;
        //printf("char: %s\n", $$->value.char_value);
    }|
    id{
        $$=$1;
    };

id: ID{
    Symbol* id_variable=find_variable($1);
    if(id_variable!=NULL)//变量存在判断
    {
        $$=id_variable;
    }
    else
    {
        //printf("Error in line %d: identifier %s not defined\n", line, $1);
        yyerror("Identifier not defined");
    }
}

%%

int main() {
    scanf("%s", filename);
    yyin = fopen(filename, "r");
    if (!yyin) {
        fprintf(stderr, "Unable to open input file\n");
        return 1;
    }

    yyparse();

    //print_variable();
    //generate_intermediate_code();

    fclose(yyin);
    system("pause");
    return 0;
}

void yyerror(const char* s) {
    fprintf(stderr, "Error in line %d: %s\n", line, s);
}


void add_variable(char* name, char* type) {//返回创建好的variable
    // 检查变量是否已经存在
    if (find_variable(name) != NULL) {
        fprintf(stderr, "Error: Symbol %s already exists\n", name);
        exit(EXIT_FAILURE);
    }
    //printf("add_variable: %s\n", name);
    // 创建新的变量节点
    Symbol* new_variable = (Symbol*)malloc(sizeof(Symbol));
    new_variable->is_constant=0;
    strcpy(new_variable->name, name);
    strcpy(new_variable->type, type);
    new_variable->next = symbol_table;

    // 添加到符号表的头部
    symbol_table = new_variable;

}

void add_constant(char* type, Value* value)//常量定义
{
    // 创建新的变量节点
    Symbol* new_constant = (Symbol*)malloc(sizeof(Symbol));
    new_constant->is_constant=1;
    strcpy(new_constant->name, "constant");
    strcpy(new_constant->type, type);
    if(strcmp(type, "CHAR")==0) strcpy(new_constant->value.char_value, value->char_value);
    else new_constant->value.int_value=value->int_value;
    new_constant->next = symbol_table;

    // 添加到符号表的头部
    symbol_table = new_constant;
}

void add_temp_variable(char* type) 
{//临时变量定义
    // 创建临时变量的名称，形如 "T1", "T2", ...
    char temp_name[10];
    sprintf(temp_name, "T%d", ++temp_variable_count);

    // 添加临时变量到符号表
    add_variable(temp_name, type);
}

void add_integer(char* name, const int int_val)
{
    // 检查变量是否已经存在
    Symbol* variable = find_variable(name);
    if (variable != NULL) {
        fprintf(stderr, "Error: Symbol %s already exists\n", name);
        exit(EXIT_FAILURE);
    }
    variable->value.int_value = int_val;
}

void add_char(char* name, char* char_val)
{
    // 检查变量是否已经存在
    Symbol* variable = find_variable(name);
    if (variable != NULL) {
        fprintf(stderr, "Error: Symbol %s already exists\n", name);
        exit(EXIT_FAILURE);
    }
    strcpy(variable->value.char_value, char_val);
}

void set_type(char* type)
{
    Symbol* variable=symbol_table;
    while(variable!=NULL && strcmp(variable->type, "NO")==0)
    {
        strcpy(variable->type,type);
        variable=variable->next;
    }
}

Symbol* find_variable(char* name) {
    // 在符号表中查找变量，找到则返回变量指针，否则返回NULL
    Symbol* current = symbol_table;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

void print_variable()
{
    Symbol* current = symbol_table;
    while (current != NULL) {
        printf("name: %s ; type: %s ; is_constant: %d\n", current->name, current->type, current->is_constant);
        current = current->next;
    }
}