#include<stdio.h>
#include<stddef.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>

/* lexing */
char* TakeChar(char **str, char c){
	if (**str == c){
		return (*str)++;
	}

	return NULL;
}

char* TakePred(char **str, int (*fn)(int)){
	if(fn(**str)){
		return (*str)++;	
	}

	return NULL;
}

char* TakeWhiteSpace(char **str){
	char *start = *str;
	if(TakePred(str, isspace)){
		while(TakePred(str, isspace));
		return start;
	}

	*str = start;
	return NULL;
}

int isidentchar(int n){
	return n == '_' || isdigit(n) || isalpha(n);
}

char* TakeString(char **str, char *token){
	TakeWhiteSpace(str);
	char *start = *str;
	while(*token && TakeChar(str, *token)){
		token++;
	}

	if(*token || (isidentchar(token[-1]) && isidentchar(**str))){
		*str = start;
		return NULL;
	}

	return start;
}
/* ISO c89 19 */
char* TakeIdentifier_(char **str){
	char *start = *str;
	if(TakeChar(str, '_') || TakePred(str, isalpha)){
		while(TakeChar(str, '_') || TakePred(str, isalpha) || TakePred(str, isdigit));
		return start;
	}

	*str = start;
	return NULL;
}

/* ISO c89 26 */
char* TakeFloatingConstant_(char **str){
	char *start = *str;
	int parts = 0;

	while(TakePred(str, isdigit))
		parts |= 1;

	if(TakeChar(str, '.'))
		parts |= 2;

	while(TakePred(str, isdigit))
		parts |= 4;

	if(TakeChar(str, 'e') || TakeChar(str, 'E'))
		parts |= 8;

	TakeChar(str, '+') || TakeChar(str, '-');

	while(TakePred(str, isdigit))
		parts |= 16;

	TakeChar(str, 'f') || TakeChar(str, 'F') || TakeChar(str, 'l') || TakeChar(str, 'L');

	if(parts == 0x1f || parts == 0x1e || parts == 0x19 || parts == 0x7 || parts == 0x6)
		return start;

	*str = start;
	return NULL;
}

/* ISO c89 26 */
char* TakeIntegerConstant_(char **str){
	char *start = *str;

	if(*start == '0'){
		if(toupper(start[1]) == 'X' && isxdigit(start[2])){
			/* hex */
			(*str) += 3;
			while(TakePred(str, isxdigit));
		}else{
			/* octal */	
			while(**str >= '0' && **str <= '7')
				(*str)++;

		}

	}else{
		/* decimal */
		while(TakePred(str, isdigit));
	}

	if(*str == start){
		return NULL;	
	}

	/* suffix */
	if(TakeChar(str, 'u') || TakeChar(str, 'U')){
		TakeChar(str, 'l') || TakeChar(str, 'L');
	}else{
		TakeChar(str, 'l') || TakeChar(str, 'L');
		TakeChar(str, 'u') || TakeChar(str, 'U');
	}

	return start;
}

char* TakeEscapeSequence_(char **str){
	char *start = *str;
	if(TakeChar(str, '\\')){
		if(TakeChar(str, 'x') && TakePred(str, isxdigit)){
			/* hex escape sequence */
			while(TakePred(str, isxdigit));
			return start;
		}else if(**str >= '0' && **str <= '7'){
			/* octal escape sequence*/
			if(**str >= '0' || **str <= '7')
				(*str) ++;
			if(**str >= '0' || **str <= '7')
				(*str) ++;
			if(**str >= '0' || **str <= '7')
				(*str) ++;
			return start;
		}else if(TakeChar(str, '\'') || TakeChar(str, '"') || TakeChar(str, '?') || 
			TakeChar(str, '\\') || TakeChar(str, 'a') || TakeChar(str, 'b') || 
			TakeChar(str, 'f') || TakeChar(str, 'n') || TakeChar(str, 'r') || 
			TakeChar(str, 't') || TakeChar(str, 'v')){
			/* simple escape sequence */
			return start;
		}
	}

	*str = start;
	return NULL;
}

/* ISO c89 28 */
char* TakeCharacterConstant_(char **str){
	char *start = *str;
	TakeChar(str, 'L');	
	if(TakeChar(str, '\'')){
		if(TakeEscapeSequence_(str)){
			if(TakeChar(str, '\''))
				return start;
		}else if(**str != '\'' && **str != '\n'){
			(*str)++;
			if(TakeChar(str, '\''))
				return start;
		}
	}

	*str = start;
	return NULL;
}

/* ISO c89 30 */
char* TakeStringLiteral_(char **str){
	char* start = *str;
	TakeChar(str, 'L');
	if(TakeChar(str, '\"')){
		while(1){
			if(TakeChar(str, '"')){
				return start;
			}

			if(**str != '\\' && **str != '\n'){
				(*str)++;
			}else if(!TakeEscapeSequence_(str)){
				break;
			}
		}
	}

	*str = start;
	return NULL;
}








enum NodeType{
	SYMBOL,
	FUNC_INIT,
	FUNC_APP,

	TYPEDEF_NAME,
	INT_CONST,
	FLOAT_CONST,
	STRING_CONST,
	CHAR_CONST,
	IDENTIFIER,
	ENUM_CONST,

	ENUM,
	STRUCT,
	UNION,
	BITFIELD,

	AUTO,
	REGISTER,
	TYPEDEF,
	EXTERN,
	STATIC,

	PARAMETER,
	VAR_ARG,

	VOLATILE,
	CONST,

	TYPENAME, 
	INT,
	UINT,
	FLOAT,
	DOUBLE,
	VOID,
	CHAR,
	POINTER,
	FUNCTION,
	ARRAY,

	DECLARATOR,
	DECLARATION,

	ASSIGN,
	MUL_ASSIGN,
	DIV_ASSIGN,
	MOD_ASSIGN,
	ADD_ASSIGN,
	SUB_ASSIGN,
	LSHIFT_ASSIGN,
	RSHIFT_ASSIGN,
	BITAND_ASSIGN,
	BITOR_ASSIGN,
	XOR_ASSIGN,
	

	COMMA,
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	LT,
	GT,
	LTE,
	GTE,
	EQ,
	NEQ,
	DEREF,
	AND,
	BITAND,
	BITOR,
	XOR,
	OR,
	LSHIFT,
	RSHIFT,
	TERNARY,
	UPLUS,
	UMINUS,
	CAST,
	INCRAMENT,
	DECRAMENT,
	PREINCRAMENT,
	PREDECRAMENT,
	CALL,
	ADDRESS,
	BITNOT,
	NOT,
	SIZEOF,
	INDEX,
	MEMBER,
	DEREF_MEMBER,

	DEFAULT,
	CASE,
	GOTO,
	BREAK,
	CONTINUE,
	RETURN,
	DOWHILE,
	WHILE,
	FOR,
	SWITCH,
	LABEL,
	BLOCK,

	LIST,
	NONE
};

char *node_type_strings[] = {
"SYMBOL", "FUNC_INIT", "FUNC_APP", "TYPEDEF_NAME","INT_CONST","FLOAT_CONST","STRING_CONST","CHAR_CONST","IDENTIFIER","ENUM_CONST","ENUM","STRUCT","UNION","BITFIELD","AUTO","REGISTER","TYPEDEF","EXTERN","STATIC","PARAMETER","VAR_ARG","VOLATILE","CONST","TYPENAME","INT","UINT","FLOAT","DOUBLE","VOID","CHAR","POINTER","FUNCTION","ARRAY","DECLARATOR","DECLARATION","ASSIGN","MUL_ASSIGN","DIV_ASSIGN","MOD_ASSIGN","ADD_ASSIGN","SUB_ASSIGN","LSHIFT_ASSIGN","RSHIFT_ASSIGN","BITAND_ASSIGN","BITOR_ASSIGN","XOR_ASSIGN","COMMA","ADD","SUB","MUL","DIV","MOD","LT","GT","LTE","GTE","EQ","NEQ","DEREF","AND","BITAND","BITOR","XOR","OR","LSHIFT","RSHIFT","TERNARY","UPLUS","UMINUS","CAST","INCRAMENT","DECRAMENT","PREINCRAMENT","PREDECRAMENT","CALL","ADDRESS","BITNOT","NOT","SIZEOF","INDEX","MEMBER","DEREF_MEMBER","DEFAULT","CASE","GOTO","BREAK","CONTINUE","RETURN","DOWHILE","WHILE","FOR","SWITCH","LABEL","BLOCK","LIST","NONE"
};

struct Node{
	enum NodeType type;
	void *item;
	size_t nodes;
	struct Node **node;
};

struct Symbol{
	char *start, *end;
	struct Node *node;
};


struct Node *Node(enum NodeType type, size_t size, struct Node** node){
	size_t i;

	struct Node *nd = malloc(sizeof(struct Node));
	nd->type = type;
	nd->nodes = size;
	nd->item = NULL;
	nd->node = malloc(sizeof(struct Node*) * size);
	for(i = 0; i < size; i++){
		nd->node[i] = node[i];
	}

	return nd;
}

void NodeDealloc(struct Node* node){
	size_t i;
	for(i = 0; i < node->nodes; i++)
		if(node->node[i])
			NodeDealloc(node->node[i]);
	if(node->item != NULL)
		free(node->item);
	free(node->node);
	free(node);
}

void FreeNodes(struct Node **nd){
	while(*nd){
		NodeDealloc(*nd);
		*nd = NULL;
		nd++;
	}
}

void print_node(struct Node* node){
	size_t i;
	if(node == NULL){
		printf("\"NULL\"");
		return;
	}

	printf("{type: \"%s\", nodes: [", node_type_strings[node->type]);

	for(i = 0; i < node->nodes; i++){
		print_node(node->node[i]);
		if(i+1 != node->nodes){
			printf(",");
		}
	}

	printf("]");

	if(node->type == SYMBOL){
		struct Symbol *sym = node->item;
		printf(", name: \"%.*s\"", sym->end - sym->start, sym->start);
	}

	printf("}");
}














struct Symbol *table_lookup(struct Symbol *table, struct Symbol sym){
	size_t i;
	while(table->start){
		if(sym.start - sym.end == table->start - table->end){
			if(strncmp(table->start, sym.start, sym.end - sym.start) == 0)
				return table;
		}

		table++;
	}

	return table;
}

struct Node* table_insert(struct Symbol *table, struct Symbol sym, struct Node *node){
	struct Symbol *pos = table_lookup(table, sym);
	if(pos->start != NULL){
		fprintf(stderr, "%.*s Redefined\n", sym.end-sym.start, sym.start);
		return NULL;
	}

	sym.node = node;
	*pos = sym;
	return node;
}

struct Symbol sym(char *str){
	struct Symbol sym;
	sym.start = str;
	sym.end = str+strlen(str);
	return sym;
}

struct Symbol get_sym(char *(*fn)(char **), char *str){
	TakeWhiteSpace(&str);

	struct Symbol sym;
	sym.start = fn(&str);
	sym.end = str;

	return sym;
}















struct Node* Symbol(struct Symbol sym){
	struct Node *node = Node(SYMBOL, 0, NULL);
	node->item = malloc(sizeof(struct Symbol));
	*((struct Symbol*)node->item) = sym;
	return node;
}

struct Node *Struct(struct Symbol sym){
	struct Node *node = Node(STRUCT, 0, NULL);
	node->item = malloc(sizeof(struct Symbol));
	*((struct Symbol*)node->item) = sym;
	return node;
}

struct Node *Type(struct Node *specifier, struct Node *declarator){
	//TODO
	
	return NULL;
}

struct Node *List(struct Node *a, struct Node *b){
	struct Node *node[] = {a, b};
	return Node(LIST, 2, node);
}

struct Node *FuncInit(struct Node *type, struct Node *arguments, struct Node *body){
	struct Node *node[] = {type, arguments, body};
	return Node(FUNC_INIT, 3, node);
}

struct Node *TypeInit(struct Node *type, struct Node *node){
	//TODO
}

struct Node *FuncApp(struct Node *function, struct Node *arguments){
	struct Node *node[] = {function, arguments};
	return Node(FUNC_APP, 2, node);
}




/* Parsing */

#include"parser.c"


int main(int argc, char **argv){
	char *str = "int **(*)(int, int)";

	char *cur = str;
	struct Node *ast = NULL;
	if(TakeTypeName(&cur, &ast)){
		print_node(ast);
	}
}

