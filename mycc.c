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
	char *start = *str;

	TakeWhiteSpace(str);
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
	TYPEINIT,
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
	SYMBOL,

	MEMORY,
	REGISTER
};

char *node_type_strings[] = {
	"CONTINUE","BREAK","RETURN","GOTO","FOR","WHILE","DOWHILE","SWITCH","TERNARY","DEFAULT","CASE","LABEL","BLOCK","DECLARATOR","TYPEDEF_NAME","DEFSYM","DEFTYPE","TYPEREF","TYPEINIT","ENUM","UNION","STRUCT","BITFIELD","ARRAY","FUNCTION","VAR_ARG","CHAR_CONST","STRING_CONST","ENUM_CONST","FLOAT_CONST","INT_CONST","POINTER","LIST","FUNCAPP","FUNCDEF","SYMBOL","MEMORY","REGISTER"
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

void NodeSetMark(struct Node *node, int mark){
	size_t i;
	if(node->count != mark){
		node->count = mark;
		for(i = 0; i < node->nodes; i++)
			if(node->node[i])
				NodeSetMark(node->node[i], mark);
	}
}

void NodeRemoveCycles(struct Node* node){
	size_t i;
	node->count = 1;
	for(i = 0; i < node->nodes; i++){
		if(node->node[i]){
			if(node->node[i]->count == 1)
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
	if(node == NULL)
		return;
	NodeSetMark(node, -1);
	NodeSetMark(node, 0);
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

void print_symbol(struct Symbol *sym){
	if(sym == NULL){
		printf("NULL");
		return;
	}

	printf("%.*s", sym->end - sym->start, sym->start);
}


void print_node(struct Node* node){
	size_t i;
	if(node == NULL){
		printf("`NULL`");
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
		printf("{type: `%s`", node_type_strings[node->type]);


	if(node->type == SYMBOL || node->type == TYPEREF){
		printf(", name: `");
		print_symbol(node->item);
		printf("`");
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











struct Node *SetType(struct Node *node, enum NodeType type){
	node->type = type;
	return node;
}
struct Node *SetItem(struct Node *node, void *item){
	node->item = item;
	return node;
}
struct Node *SetNode(struct Node *node, size_t i, struct Node *item){
	node->node[i] = item;
	return node;
}



struct Node *List(struct Node *a, struct Node *b){
	struct Node *node[2];
	node[0] = a;
	node[1] = b;
	return Node(LIST, 2, node);
}
size_t ListLen(struct Node *a){
	if(a == NULL)
		return 0;
	return 1 + ListLen(a->node[1]);
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

	if(a->node[0]->type == LIST){
		ret = Concat(a->node[0], Flatten(a->node[1]));
		a->node[0] = a->node[1] = NULL;
		NodeDealloc(a);
	}else{
		a->node[1] = Flatten(a->node[1]);
		ret = a;
	}

	return ret;
}

struct Node *SetDefVal(struct Node *node, struct Node *nd){
	node->node[1] = nd;
	return node;
}

struct Node *FuncApp(struct Node *function, struct Node *arguments){
	struct Node *node[2];
       	node[0] = function;
	node[1] = arguments;
	return Node(FUNCAPP, 2, node);
}




struct Node* Symbol(struct Symbol sym){
	struct Node *nd[3] = {NULL, NULL, NULL};
	struct Node *node = Node(SYMBOL, 3, nd);
	node->item = malloc(sizeof(struct Symbol));
	*((struct Symbol*)node->item) = sym;
	return node;
}

struct Symbol sym(char *str){
	struct Symbol sym;
	sym.start = str;
	sym.end = str+strlen(str);
	return sym;
}

struct Symbol get_sym(char *(*fn)(char **), char *str){
	struct Symbol sym;

	TakeWhiteSpace(&str);
	sym.start = fn(&str);
	sym.end = str;

	return sym;
}


struct Node *Struct(struct Node *decl){
	return Node(STRUCT, 1, &decl);
}

struct Node *Union(struct Node *decl){
	return Node(UNION, 1, &decl);
}

struct Node *Enum(struct Node *decl){
	return Node(ENUM, 1, &decl);
}

struct Node *TypeInit(struct Node *type, struct Node *val){
	struct Node *node[2];
       	node[0] = type;
	node[1] = val;
	return Node(TYPEINIT, 2, node);
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
					break;
				}

				cur->node[0] = NULL;
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
		switch(type & 0xff00){ /*TODO implement more types and use qualifiers/storage class*/
			case VOID:
				base = Symbol(sym("void"));
				break;
			case INT:
			case SHORT:
			case INT | SHORT:
				base = Symbol(sym("int"));
				break;
			case LONG:
			case INT | LONG:
			case SHORT | LONG:
			case INT | SHORT | LONG:
				base = Symbol(sym("long"));
				break;
			case INT | UNSIGNED:
			case SHORT | UNSIGNED:
			case INT | SHORT | UNSIGNED:
				base = Symbol(sym("unsigned int"));
				break;
			case LONG | UNSIGNED:
			case INT | LONG | UNSIGNED:
			case SHORT | LONG | UNSIGNED:
			case INT | SHORT | LONG | UNSIGNED:
				base = Symbol(sym("unsigned long"));
				break;
			case FLOAT:
				base = Symbol(sym("float"));
				break;
			case DOUBLE:
				base = Symbol(sym("double"));
				break;
			case CHAR | UNSIGNED:
				base = Symbol(sym("unsigned char"));
				break;
			case CHAR:
			case CHAR | SIGNED:
			case CHAR | UNSIGNED | SIGNED:
				base = Symbol(sym("char"));
				break;
			default:
				printf("unkown type specifier");
				base = Symbol(sym("int"));
		}
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
			declarator->node[0] = base;
			return declarator;
		}case DECLARATOR: {
			base = Type(base, declarator->node[0]);
			base->node[1] = declarator->node[1];
			
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
	block->type = BLOCK;
	return block;
}





/* Parsing */

#include"lexer.c"
#include"parser.c"







/* analysis/typechecking */

/* TODO verify assumptions */
void verify(struct Node* node){
}

enum Builtin{
	MY_VOID,
	MY_INT,
	MY_LONG,
	MY_UINT,
	MY_ULONG,
	MY_FLOAT,
	MY_DOUBLE,
	MY_UCHAR,
	MY_CHAR,

	MY_ADD,
	MY_SUB,
	MY_MUL,
	MY_DIV,
	MY_TERNARY,
	MY_WHILE,
	MY_ASSIGN
};


int SymCompare(struct Symbol s1, struct Symbol s2){
	if(s1.start - s1.end == s2.start - s2.end){
		if(strncmp(s1.start, s2.start, s2.end - s2.start) == 0)
			return 1;
	}
	return 0;
}

struct Node* SymLookup(struct Node *env, enum NodeType type, struct Symbol sym){
	struct Node *nd;
	if(env == NULL)
		return NULL;

	nd = env->node[0];
	if(nd->type == type && SymCompare(*(struct Symbol*)(nd->item), sym))
		return nd;
	return SymLookup(env->node[1], type, sym);
}

int graph_eq(struct Node* n1, struct Node* n2){
	size_t i = 0;

	if(n1 == NULL || n2 == NULL)
		return 0;

	if(n1->count != n2->count)
		return 0;
	if(n1->count && n2->count)
		return 1;

	n1->count = 1;
	n2->count = 1;

	if(n1->nodes == n2->nodes && n1->type == n2->type){
		for(i = 0; i < n2->nodes; i++)
			if(!graph_eq(n1->node[i], n2->node[i]))
				return 0;
		return 1;
	}

	return 0;
	
}

struct Node *TypeOf(struct Node *nd){
	struct Node *cur;
	if(nd == NULL)
		return NULL;

	switch(nd->type){
		case SYMBOL:
			return nd->node[0];
		case FUNCAPP:
			cur = TypeOf(nd->node[0]);
			if(cur == NULL)
				return NULL;
			return cur->node[0];
		case TYPEINIT:
			return nd->node[0];
	}
}

void eval(struct Node **ref, void *res, struct Node *nd){
	switch(nd->type){
		case TYPEINIT:
			if(nd->node[0] == ref[MY_INT]){
				struct Symbol *sym = nd->node[1]->item;
				*(int*)res = strtol(sym->start, NULL, 0);
			}
		case FUNCAPP:
		case SYMBOL:
			/* TODO */
			break;
	}
}

size_t SizeOf(struct Node **ref, struct Node *nd){
	struct Node *cur;
	int len;

	switch(nd->type){
		case FUNCTION:
		case POINTER:
			return 8; /* function pointer */
		case ENUM:
			return 4; /* integer */
		case ARRAY:
			if(TypeOf(nd->node[1]) != ref[MY_INT])
				printf("unexpected type");

			eval(ref, &len, nd->node[1]);
			return SizeOf(ref, nd->node[0]) * len;
		case STRUCT:
		case UNION:
			/* TODO */
		case SYMBOL:
			if(nd == ref[MY_INT])
				return 4;
			/* TODO */
		default:
			return SizeOf(ref, TypeOf(nd));	
	}
}

int type_check(struct Node *nd){
	switch(nd->type){
		case SYMBOL:
			return type_check(nd->node[1]) && graph_eq(TypeOf(nd->node[0]), TypeOf(nd->node[1]));
		case FUNCAPP: {
			struct Node *args = TypeOf(nd->node[0]), *cur = nd->node[1];

			if(args == NULL) /* symbols with no type */
				return -1;

			if(args->type != FUNCTION)
				return 0;

			args = args->node[1];
			if(ListLen(args) != ListLen(cur))
				return 0;

			while(args){
				if(!graph_eq(args->node[0], TypeOf(cur->node[0])))
					return 0;

				args = args->node[1];
				cur = cur->node[1];
			}

			return 1;
		} case TYPEINIT:
			/* TODO arrays? */
			return 1;
			

	}
}








struct Node *graphify(struct Node **env, struct Node *ast){
	struct Node *node = NULL;
	if(ast == NULL || ast->count == 1)
		return ast;

	ast->count = 1;
	switch(ast->type){
		case FUNCTION:{
			size_t n = 0;
			/*TODO parameters*/

			node = ast->node[1];
			while(node){
				struct Node *cur = node->node[0];
				*env = List(cur, *env);

				cur->node[2] = Node(MEMORY, 0, NULL);
				cur->node[2]->item = malloc(sizeof(int));
				((int*)cur->node[1]->item)[0] = n++;

				node = node->node[1];
			}
			ast->node[0] = graphify(env, ast->node[0]);

			break;
		}case LIST: 
		case BLOCK: 
		case FUNCAPP:
		case ARRAY:
			ast->node[0] = graphify(env, ast->node[0]);
			ast->node[1] = graphify(env, ast->node[1]);
			break;
		case RETURN:
		case POINTER:
			ast->node[0] = graphify(env, ast->node[0]);
			break;
		case TYPEINIT:
			ast->node[0] = graphify(env, ast->node[0]);
			if(ast->node[0]->type == FUNCTION){
				/*TODO local variales*/
				ast->node[1] = graphify(env, ast->node[1]->node[1]);
			}
			break;
		case SYMBOL:
		case TYPEREF:
			node = SymLookup(*env, ast->type, *(struct Symbol*)(ast->item));

			if(node == NULL){
				*env = List(ast, *env);
				ast->node[0] = graphify(env, ast->node[0]);
				ast->node[1] = graphify(env, ast->node[1]);
				return ast;
			}

			if(ast->node[0] != NULL){
				printf("redefinition of ");
				print_node(node);
				printf(".\n");
			}

			NodeDealloc(ast);
			ast = node;
			break;
		default: break;
	}

	return ast;
}



void rev_map(struct Node *node, struct Node **v, void (*fn)(struct Node **, struct Node *)){
	if(node == NULL)
		return;
	rev_map(node->node[1], v, fn);
	fn(v, node->node[0]);
}

void map(struct Node *node, struct Node **v, void (*fn)(struct Node **, struct Node *)){
	if(node == NULL)
		return;
	fn(v, node->node[0]);
	map(node->node[1], v, fn);
}






void gen_def(struct Node **ref, struct Node *node);

void gen_typeinit(struct Node **ref, struct Node *node){
	struct Node *type = node->node[0]; 
	struct Node *init = node->node[1];

	if(ref[MY_INT] == type){
		printf("MOV eax, "); print_symbol(init->item); printf("\n");
	}
}


void gen_expr(struct Node **ref, struct Node *node);
void gen_block(struct Node **ref, struct Node *node);

void gen_ref(struct Node **ref, struct Node *node){
	if(node->node[1] && node->node[1]->type == MEMORY){
		int ind = ((int*)node->node[1]->item)[0];	

		printf("rbp + 8*(2 + %d)", ind); 
		return;
	}

	print_symbol(node->item);
}

void gen_param(struct Node **ref, struct Node *node){
	gen_expr(ref, node);
	printf("PUSH rax\n");
}

void gen_expr(struct Node **ref, struct Node *node){
	static size_t label_id = 0; /*TODO this is dumb (turn into parameter context)*/

	switch(node->type){
		case FUNCAPP: {
			if(ref[MY_TERNARY] == node->node[0]){
				struct Node *cur = node->node[1];

				gen_expr(ref, cur->node[0]);
				cur = cur->node[1];

				printf("test eax, eax\n");
				printf("jne .else%d\n", label_id);
				gen_expr(ref, cur->node[0]);
				cur = cur->node[1];

				if(cur->node[0] != NULL){
					printf("jmp .end%d\n", label_id);
					printf(".else%d:\n", label_id);
					gen_expr(ref, cur->node[0]);
					printf(".end%d:\n", label_id);
					label_id++;
					break;
				}

				printf(".else%d:\n", label_id);
				label_id++;

				break;
			}

			if(ref[MY_WHILE] == node->node[0]){
				struct Node *cur = node->node[1];

				printf(".start%d:\n", label_id);
				gen_expr(ref, cur->node[0]);
				cur = cur->node[1];
				printf("test eax, eax\n");
				printf("jne .end%d\n", label_id);
				gen_expr(ref, cur->node[0]);
				printf("jmp .start%d\n", label_id);
				printf(".end%d:\n", label_id);

				label_id++;
				break;
			}

			if(ref[MY_ASSIGN] == node->node[0]){
				struct Node *sym = node->node[1]->node[0];
				struct Node *expr = node->node[1]->node[1]->node[0];

				gen_expr(ref, expr);
				printf("MOV ["); gen_ref(ref, sym); printf("], eax\n");
				break;
			}



			rev_map(node->node[1], ref, gen_param);
			printf("CALL "); print_symbol(node->node[0]->item); printf("\n");
			printf("add rsp, %d\n", ListLen(node->node[1])*8);
			break;
		} case TYPEINIT:
			gen_typeinit(ref, node);
			break;
		case SYMBOL:
			printf("MOV eax, ["); gen_ref(ref, node); printf("]\n");
			break;
		case RETURN:
			gen_expr(ref, node->node[0]);
			printf("mov rsp, rbp\npop rbp\nret\n");
			break;
		case BLOCK:
			gen_block(ref, node);
			break;
		default:
			printf("unexpected expression: %d\n\n", node->type);
			print_node(node);
			printf("\n");
	}
}

void gen_block(struct Node **ref, struct Node *node){
	struct Node *defs = node->node[0];
	struct Node *body = node->node[1];

	map(defs, ref, gen_def);
	map(body, ref, gen_expr);
}

void gen_function(struct Node **ref, struct Node *node){
	struct Node *type = node->node[0];
	struct Node *init = node->node[1];

	print_symbol(node->item); printf(":\n");
	printf("push rbp\nmov rbp, rsp\n");

	gen_block(ref, init);

	printf("mov rsp, rbp\npop rbp\nret\n");
}

void gen_def(struct Node **ref, struct Node *node){
	struct Node *type = node->node[0];
	struct Node *init = node->node[1];

	size_t size;

	switch(type->type){
		case FUNCTION:
			printf("jmp _F");
			print_symbol(node->item);
			printf("_end\n");
			gen_function(ref, node);
			printf("_F");
			print_symbol(node->item);
			printf("_end:\n");
			break;
		default:
		case ARRAY:
		case POINTER:
		case STRUCT:
		case UNION:
		case ENUM:
			size = SizeOf(ref, node);
			printf("jmp "); print_symbol(node->item); printf("+%d\n", size);
			print_symbol(node->item); printf(": "); printf("times %d db 0\n", size);
			if(init != NULL){
				gen_expr(ref, init);
				printf("mov [");
				print_symbol(node->item);
				printf("], eax\n");
			}
	}
}

void code_gen(struct Node *node, struct Node *env){
	size_t env_len = ListLen(env);
	struct Node **ref = malloc(sizeof(struct Node*) * env_len);
	struct Node *cur = env;
	size_t i = 0;

	cur = env;
	while(cur){
		ref[env_len - ++i] = cur->node[0];
		cur = cur->node[1];
	}


	cur = node;
	while(cur){
		/*print_symbol(cur->node[0]->item);
		printf("\n");*/
		gen_def(ref, cur->node[0]);
		cur = cur->node[1];

	}

	free(ref);
}




/* IO/other */

char *ReadFile(char *path){
	FILE *fp;
	size_t end;
	char *buf;

	if(path == NULL){
		return NULL;
	}

	fp = fopen(path, "rb");

	if(fp == NULL || fseek(fp, 0, SEEK_END)){
		return NULL;
	}

	end = ftell(fp);

	rewind(fp);
	buf = malloc(end + 1);
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
	struct Node *ast;
	char *cur;

	if(str == NULL){
		printf("usage: mycc file\n");
		return 0;
	}

	cur = str;
	ast = NULL;
	if(TakeTranslationUnit(&cur, &ast)){
		TakeWhiteSpace(&cur);
		if(*cur == 0){
			struct Node *cur, *env = NULL;

			env = List(Symbol(sym("void")), env);
			env = List(Symbol(sym("int")), env);
			env = List(Symbol(sym("long")), env);
			env = List(Symbol(sym("unsigned int")), env);
			env = List(Symbol(sym("unsigned long")), env);
			env = List(Symbol(sym("float")), env);
			env = List(Symbol(sym("double")), env);
			env = List(Symbol(sym("unsigned char")), env);
			env = List(Symbol(sym("char")), env);

			env = List(Symbol(sym("_Add")), env);
			env = List(Symbol(sym("_Sub")), env);
			env = List(Symbol(sym("_Mul")), env);
			env = List(Symbol(sym("_Div")), env);
			env = List(Symbol(sym("_Ternary")), env);
			env = List(Symbol(sym("_While")), env);
			env = List(Symbol(sym("_Assign")), env);

			ast = Flatten(ast);

			/*
			printf("\n\n");
			print_node(ast);
			printf("\n\n");
			*/

			graphify(&env, ast);
			NodeSetMark(ast, -1);
			NodeSetMark(env, -1);
			NodeSetMark(ast, 0);
			NodeSetMark(env, 0);

			printf("%%include\"my_crt.S\"\n");
			printf("GLOBAL _start\n");
			printf("_start:\n");
			code_gen(ast, env);
			printf("MOV eax, 1\n");
			printf("PUSH rax\n");
			printf("CALL main\n");
			printf("MOV edi, eax\n");
			printf("CALL exit\n");

			/*dealloc env*/
			ast = List(ast, env);
			NodeDealloc(ast);
		}
	}

	free(str);
}

