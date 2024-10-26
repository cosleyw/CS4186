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





enum TypeSpecifier{
	VOID = 1 << 8,
	CHAR = 1 << 9,
	SHORT = 1 << 10,
	INT = 1 << 11,
	LONG = 1 << 12,
	FLOAT = 1 << 13,
	DOUBLE = 1 << 14,
	SIGNED = 1 << 15,
	UNSIGNED = 1 << 16,

	CONST = 1 << 17,
	VOLATILE = 1 << 18,

	EXTERN = 1 << 19,
	STATIC = 1 << 20,
	AUTO = 1 << 21,
	REGISTER = 1 << 22,
	TYPEDEF = 1 << 23
};

enum NodeType{
	CONTINUE,
	BREAK,
	RETURN,
	GOTO,
	FOR,
	WHILE,
	DOWHILE,
	
	SWITCH,
	TERNARY,
	DEFAULT,
	CASE,
	
	LABEL,
	BLOCK,

	DECLARATOR,
	TYPEDEF_NAME,

	DEFSYM,
	DEFTYPE,
	TYPEREF,
	ENUM,
	UNION,
	STRUCT,
	BITFIELD,
	ARRAY,
	FUNCTION,
	VAR_ARG,

	CHAR_CONST,
	STRING_CONST,
	ENUM_CONST,
	FLOAT_CONST,
	INT_CONST,

	POINTER,

	LIST,
	FUNCAPP,
	FUNCDEF,
	SYMBOL
};

char *node_type_strings[] = {
	"CONTINUE","BREAK","RETURN","GOTO","FOR","WHILE","DOWHILE","SWITCH","TERNARY","DEFAULT","CASE","LABEL","BLOCK","DECLARATOR","TYPEDEF_NAME","DEFSYM","DEFTYPE","TYPEREF","ENUM","UNION","STRUCT","BITFIELD","ARRAY","FUNCTION","VAR_ARG","CHAR_CONST","STRING_CONST","ENUM_CONST","FLOAT_CONST","INT_CONST","POINTER","LIST","FUNCAPP","FUNCDEF","SYMBOL"
};

struct Node{
	enum NodeType type;
	void *item;
	size_t count, nodes;
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
	nd->count = 0;
	nd->node = malloc(sizeof(struct Node*) * size);
	for(i = 0; i < size; i++){
		nd->node[i] = node[i];
	}

	return nd;
}

void NodeResetMark(struct Node *node){
	size_t i;
	if(node->count != 0){
		node->count = 0;	
		for(i = 0; i < node->nodes; i++)
			if(node->node[i])
				NodeResetMark(node->node[i]);
	}
}

void NodeRemoveCycles(struct Node* node){
	size_t i;
	node->count = 1;
	for(i = 0; i < node->nodes; i++){
		if(node->node[i]){
			if(node->node[i]->count)
				node->node[i] = NULL;
			else
				NodeRemoveCycles(node->node[i]);
		}
	}
}

void NodeDealloc_(struct Node* node){
	size_t i;
	for(i = 0; i < node->nodes; i++)
		if(node->node[i])
			NodeDealloc_(node->node[i]);
	if(node->item != NULL)
		free(node->item);
	free(node->node);
	free(node);
}

void NodeDealloc(struct Node* node){
	NodeResetMark(node);
	NodeRemoveCycles(node);
	NodeDealloc_(node);
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

	if(node->type == LIST){
		struct Node *cur = node;

		int start = 1;
		printf("[");
		while(cur != NULL){
			if(!start)
				printf(",");
			start = 0;
			print_node(cur->node[0]);
			cur = cur->node[1];	
		}

		printf("]");
		return;	
	}

	if(node->type >= sizeof(node_type_strings)/sizeof(node_type_strings[0]))
		printf("{type: %d", node->type);
	else
		printf("{type: \"%s\"", node_type_strings[node->type]);


	if(node->type == SYMBOL){
		struct Symbol *sym = node->item;
		printf(", name: \"%.*s\"}", sym->end - sym->start, sym->start);
		return;
	}

	printf(", nodes: [");

	for(i = 0; i < node->nodes; i++){
		print_node(node->node[i]);
		if(i+1 != node->nodes){
			printf(",");
		}
	}

	printf("]}");
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








struct Node *SetType(struct Node *node, enum NodeType type){
	node->type = type;
	return node;
}
struct Node *SetItem(struct Node *node, void *item){
	node->item = item;
	return node;
}
struct Node *SetNode(struct Node *node, size_t ind, struct Node *nd){
	node->node[ind] = nd;
	return node;
}




struct Node *List(struct Node *a, struct Node *b){
	struct Node *node[] = {a, b};
	return Node(LIST, 2, node);
}

struct Node *Concat(struct Node *a, struct Node *b){
	struct Node *cur = a;
	if(b == NULL)
		return a;
	while(cur->node[1] != NULL)
		cur = cur->node[1];
	cur->node[1] = b;
	return a;
}
struct Node *Flatten(struct Node *a){
	struct Node *ret;
	if(a == NULL)
		return NULL;

	if(a->node[0]->type == LIST)
		ret = Concat(a->node[0], Flatten(a->node[1]));
	else
		ret = List(a->node[0], Flatten(a->node[1]));

	a->node[0] = NULL;
	a->node[1] = NULL;
	NodeDealloc(a);
	return ret;
}

struct Node *SymDef(struct Node *sym, struct Node *type, struct Node *node){
	struct Node *nd[] = {sym, type, node};
	return Node(DEFSYM, 3, nd);
}

struct Node *TypeDef(struct Node *sym, struct Node *node){
	struct Node *nd[] = {sym, node};
	return Node(DEFTYPE, 2, nd);
}

struct Node *FuncApp(struct Node *function, struct Node *arguments){
	struct Node *node[] = {function, arguments};
	return Node(FUNCAPP, 2, node);
}





struct Node* Symbol(struct Symbol sym){
	struct Node *node = Node(SYMBOL, 0, NULL);
	node->item = malloc(sizeof(struct Symbol));
	*((struct Symbol*)node->item) = sym;
	return node;
}




struct Node *Specifier(struct Node *specifier){
	enum TypeSpecifier type = 0;
	struct Node *base = NULL;
	struct Node *cur = specifier;

	if(specifier == NULL || specifier->type != LIST){
		printf("Type: Expected specifier list.");
		return Symbol(sym("int"));
	}


	while(cur != NULL){
		struct Node *node = cur->node[0];
		switch(node->type){
			case SYMBOL: case TYPEREF: case TYPEDEF: case STRUCT: case UNION: case ENUM:
				if(base != NULL){
					printf("Unexpected type specifier: ");
					print_node(node);
				}
				base = node;
				break;
			default:
				type |= node->type;
		}

		cur = cur->node[1];
	}

	if(!(!base ^ !(type & 0xff00))){
		printf("Unexpected type specifier");
		printf("\n\n");
	}

	if(base == NULL){
		type = type & ~SIGNED;
		switch(type & 0xff00){ //TODO implement more types and use qualifiers/storage class
			case VOID:
				base = Symbol(sym("void"));
				break;
			case INT:
			case SHORT:
			case INT | SHORT:
			case LONG:
			case INT | LONG:
			case SHORT | LONG:
			case INT | SHORT | LONG:
				base = Symbol(sym("int"));
				break;
			case INT | UNSIGNED:
			case SHORT | UNSIGNED:
			case INT | SHORT | UNSIGNED:
			case LONG | UNSIGNED:
			case INT | LONG | UNSIGNED:
			case SHORT | LONG | UNSIGNED:
			case INT | SHORT | LONG | UNSIGNED:
				base = Symbol(sym("unsigned int"));
				break;
			case FLOAT:
			case DOUBLE:
				base = Symbol(sym("double"));
				break;
			default:
				printf("unkown type specifier");
				base = Symbol(sym("int"));
		}
	}else{
		struct Node *new_base = Node(base->type, base->nodes, base->node);
		memset(base->node, sizeof(struct Node*) * base->nodes, 0);
		base = new_base;
	}	

	NodeDealloc(specifier);
	return base;
}

struct Node *Type(struct Node *base, struct Node *declarator){
	if(declarator == NULL)
		return base;

	switch(declarator->type){
		case LIST: {
			struct Node *cur = declarator;
			while(cur != NULL){
				base = Type(base, cur->node[0]);
				cur->node[0] = NULL;
				cur = cur->node[1];
			}

			NodeDealloc(declarator);
			return base;
		} case POINTER:{
			struct Node *cur = declarator;
			while(cur != NULL){
				struct Node *node = cur->node[0];
				cur->node[0] = base;
				base = cur;
				cur = node;
			}

			return base;
		}case FUNCTION: case ARRAY: {
			struct Node *cur = declarator;
			struct Node *node = cur->node[0];

			cur->node[0] = base;
			base = cur;
			return Type(base, node);
		}case SYMBOL: {
			return SymDef(declarator, base, NULL);
		}case DECLARATOR: {
			base = Type(base, declarator->node[0]);
			base->node[2] = declarator->node[1];
			
			declarator->node[0] = declarator->node[1] = NULL;
			NodeDealloc(declarator);
			return base;
		}
		default:
			printf("unexpected: ");
			print_node(declarator);
			printf("\n\n");
	}

	return NULL;
}

struct Node *TypeList(struct Node *base, struct Node *declarator){
	struct Node* cur = declarator;
	if(declarator->type != LIST){
		printf("Expected list\n");
		return NULL;
	}

	while(cur != NULL){
		cur->node[0] = Type(base, cur->node[0]);
		cur = cur->node[1];
	}

	return declarator;
}

struct Node *Block(struct Node *block){
	return block;
}






/* Parsing */

#include"parser.c"







/* IO/other */

char *ReadFile(char *path){
	FILE *fp = fopen(path, "rb");
	if(fseek(fp, 0, SEEK_END)){
		return NULL;
	}

	size_t end = ftell(fp);

	rewind(fp);
	char *buf = malloc(end + 1);
	if(buf == NULL){
		fclose(fp);
		return NULL;
	}

	if(fread(buf, 1, end, fp) != end){
		fclose(fp);
		free(buf);
		return NULL;
	}

	fclose(fp);
	buf[end] = 0;
	return buf;
}




int main(int argc, char **argv){
	char *str = ReadFile(argv[1]);

	char *cur = str;
	struct Node *ast = NULL;
	if(TakeTranslationUnit(&cur, &ast)){
		TakeWhiteSpace(&cur);
		if(*cur == 0){
			printf("success:\n");
			print_node(ast);
		}
	}

	NodeDealloc(ast);
	free(str);
}

