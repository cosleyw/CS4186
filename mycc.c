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

char* TakeString(char **str, char *token){
	char *start = *str;
	while(*token && TakeChar(str, *token)){
		token++;
	}

	if(*token){
		*str = start;
		return NULL;
	}

	return start;
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

/* ISO c89 19 */
char* TakeIdentifier(char **str){
	char *start = *str;
	if(TakeChar(str, '_') || TakePred(str, isalpha)){
		while(TakeChar(str, '_') || TakePred(str, isalpha) || TakePred(str, isdigit));
		return start;
	}

	*str = start;
	return NULL;
}

/* ISO c89 26 */
char* TakeFloatingConstant(char **str){
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

	/*
		valid floating point constants have

		d1 . d2 e1 d3
		. d2 e1 d3
		d1 e1 d3
		d1 . d2
		. d2
	*/

	if(parts == 0x1f || parts == 0x1e || parts == 0x19 || parts == 0x7 || parts == 0x6)
		return start;

	*str = start;
	return NULL;
}

/* ISO c89 26 */
char* TakeIntegerConstant(char **str){
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

char* TakeEscapeSequence(char **str){
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
char* TakeCharacterConstant(char **str){
	char *start = *str;
	TakeChar(str, 'L');	
	if(TakeChar(str, '\'')){
		if(TakeEscapeSequence(str)){
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
char* TakeStringLiteral(char **str){
	char* start = *str;
	TakeChar(str, 'L');
	if(TakeChar(str, '\"')){
		while(1){
			if(TakeChar(str, '"')){
				return start;
			}

			if(**str != '\\' && **str != '\n'){
				(*str)++;
			}else if(!TakeEscapeSequence(str)){
				break;
			}
		}
	}

	*str = start;
	return NULL;
}


void TestX(char **passing, char **failing, char *name, char* (*fn)(char **str)){
	char **arr = passing;
	while(*arr){
		char *my_ptr = *arr;
		char *test = fn(&my_ptr);

		if(test){
			if(*my_ptr != 0){
				printf("%s: did not lex entire string: \"%s\", got \"%.*s\"\n", name, *arr, my_ptr - *arr, *arr);
			}

			if(test != *arr){
				printf("%s: Returned Pointer Is Incorrect\n", name);
			}
		}else{
			printf("%s: failed to parse: %s\n", name, *arr);
		}
		
		arr++;
	}

	arr = failing;
	while(*arr){
		char *my_ptr = *arr;
		char *test = fn(&my_ptr);

		if(test){
			if(*my_ptr != 0){
				printf("%s: Invalid Partial Parse: %s\n", name, *arr);
			}else{
				printf("%s: Invalid Parse: %s\n", name, *arr);
			}
		}

		arr++;
	}
}


void LexerTests(){
	printf("running lexer tests\n");

	{
		char* Identifiers[] = {
			"dAve",
			"_dave",
			"C8arl",
			NULL
		};

		char*  InvalidIdentifiers[] = {
			"4ndy",
			"(",
			NULL
		};

		TestX(Identifiers, InvalidIdentifiers, "TakeIdentifier", TakeIdentifier);
	}

	{
		char* FloatingConstants[] = {
			"1e6f",
			"1e+6",
			".1E-6",
			"1.1e-6L",
			NULL
		};

		char* InvalidFloatingConstants[] = {
			"100",
			"0x17",
			"C8arl",
			"1.",
			NULL
		};

		TestX(FloatingConstants, InvalidFloatingConstants, "TakeFloatingConstant", TakeFloatingConstant);
	}

	{
		char* IntegerConstants[] = {
			"0",
			"0x100",
			"256",
			"077",
			NULL,
		};

		char* InvalidIntegerConstants[] = {
			"Hi",
			".1E-6",
			"+",
			"-",
			NULL
		};

		TestX(IntegerConstants, InvalidIntegerConstants, "TakeIntegerConstant", TakeIntegerConstant);
	}

	{
		char* CharacterConstants[] = {
			"'H'",
			"L'H'",
			"'\\077'",
			"'\\xffff'",
			"'\\b'",
			NULL
		};

		char* InvalidCharacterConstants[] = {
			"''",
			"\"Hi\"",
			"Hi",
			"7",
			NULL
		};

		TestX(CharacterConstants, InvalidCharacterConstants, "TakeCharacterConstant", TakeCharacterConstant);
	}

	{
		char* StringLiterals[] = {
			"L\"hi\"",
			"\"\"",
			"\"\\\"\"",
			"\"\\n\"",
			NULL
		};

		char* InvalidStringLiterals[] = {
			"Hi",
			"'\\077'",
			"7",
			NULL
		};

		TestX(StringLiterals, InvalidStringLiterals, "TakeStringLiteral", TakeStringLiteral);
	}
}



enum TypeQualifier{
	STATIC = 1<<0,
	CONST = 1<<2,
	REGISTER = 1<<3,
	VOLATILE = 1<<4
};

enum DerivedType{
	UNION,
	STRUCT,
	POINTER,
	FUNCTION
};

struct Type{
	int id;
	char *st, *ed;
	enum TypeQualifier qualifiers;
	enum DerivedType type;

	size_t sub_types;
	struct Type **sub_type;
};

enum NodeType{
	TYPE_INIT,
	VAR_REF,
	VAR_ASSIGN,
	FUNC_INIT, /* lambdas anyone? */
	FUNC_APP, /* operators are functions fight me */
	NONE
};

struct Node{
	enum NodeType type;
	void *item;
	size_t nodes;
	struct Node **node;
};

/* TODO more node constructors */

struct Symbol{
	int id;
	char *st, *ed;	
	struct Type *type;
	struct Node *value; /* initial value probably not needed... */
};

struct Scope{
	size_t symbols;
	struct Symbol *symbol;
	struct Scope *sub_scope;
};

/* ISO c89 13 */

/* TODO make dynamic */
/* TODO make non-global */
size_t Types = 0;
struct Type TypeTable[1024];

struct Type *TypeAlloc(char *st, char *ed, enum TypeQualifier qualifier, enum DerivedType derived, size_t sub_types, struct Type **sub_type){
	struct Type *type = malloc(sizeof(struct Type));
	size_t i;

	type->id = -1;
	type->st = st;
	type->ed = ed;
	type->qualifiers = qualifier;
	type->type = derived;
	type->sub_types = sub_types;
	type->sub_type = malloc(sub_types * sizeof(struct Type));

	for(i = 0; i < sub_types; i++){
		type->sub_type[i] = sub_type[i];
	}
}

struct Type *TypePointer(enum TypeQualifier qualifier, struct Type* type){
	return TypeAlloc(NULL, NULL, qualifier, POINTER, 1, &type);
}

struct Type *TypeUnion(enum TypeQualifier qualifier, size_t sub_types, struct Type** type){
	return TypeAlloc(NULL, NULL, qualifier, UNION, sub_types, type);
}

struct Type *TypeStruct(enum TypeQualifier qualifier, size_t sub_types, struct Type** type){
	return TypeAlloc(NULL, NULL, qualifier, STRUCT, sub_types, type);
}

struct Type *TypeFunction(enum TypeQualifier qualifier, struct Type *return_type, size_t args, struct Type **arguments){
	struct Type *things[2];
	things[0] = return_type;
	things[1] = TypeStruct(0, args, arguments);
	return TypeAlloc(NULL, NULL, qualifier, FUNCTION, 2, things);
}

void TypeDealloc(struct Type *type){
	size_t i;
	for(i = 0; i < type->sub_types; i++)
		TypeDealloc(type->sub_type[i]);
	free(type->sub_type);
	free(type);
}

struct Type *TypeDef(struct Type *type){
	TypeTable[Types] = *type;
	TypeTable[Types].id = Types;
	Types++;
	return type;
}

struct Type *TypeLookup(char *st, char *ed){
	size_t i;
	for(i = 0; i < Types; i++){
		struct Type *type = TypeTable + i;
		if(ed - st == type->ed - type->st){
			if(strncmp(type->st, st, type->ed - type->st) == 0){
				return type;
			}
		}
	}

	return NULL;
}


/* TODO make dynamic */
/* TODO make non-global */
size_t Symbols = 0;
struct Symbol SymbolTable[1024];

struct Scope *ScopeAlloc(struct Scope *sub_scope, size_t symbols, struct Symbol *symbol){
	size_t i;
	struct Scope *scope = malloc(sizeof(struct Scope));
	scope->symbol = malloc(symbols * sizeof(struct Symbol));
	for(i = 0; i < symbols; i++){
		scope->symbol[i] = symbol[i];
	}

	return scope;
}

struct Symbol *SymLookup(struct Scope *scope, char *st, char *ed){
	size_t i;
	for(i = 0; i < scope->symbols; i++){
		struct Symbol *sym = scope->symbol + i;
		if(ed - st == sym->ed - sym->st){
			if(strncmp(sym->st, st, sym->ed - sym->st) == 0){
				return sym;
			}
		}
	}

	if(scope->sub_scope)
		return SymLookup(scope->sub_scope, st, ed);

	return NULL;
}

struct Symbol *SymDef(char *st, char *ed, struct Type* type, struct Node* value){
	struct Symbol *sym = SymbolTable + Symbols;

	if(Symbols == 1024){
		fprintf(stderr, "Ran out of space in Symbol Table.\r\n");	
		return NULL;
	}

	sym->id = Symbols;
	sym->st = st;
	sym->ed = ed;
	sym->type = type;
	sym->value = value;

	Symbols++;
	return sym;
}


struct Node *NodeAlloc(enum NodeType type, size_t size, struct Node** node){
	size_t i;

	struct Node *nd = malloc(sizeof(struct Node));
	nd->type = type;
	nd->nodes = size;
	nd->node = malloc(sizeof(struct Node*) * size);
	for(i = 0; i < size; i++){
		nd->node[i] = node[i];
	}

	return nd;
}

void NodeDealloc(struct Node* node){
	size_t i;
	for(i = 0; i < node->nodes; i++)
		NodeDealloc(node->node[i]);
	free(node->node);
	free(node);
}

struct Node *NodeVarRef(struct Scope *scope, char *st, char *ed){
	struct Node *node = NodeAlloc(VAR_REF, 0, NULL);
	node->item = SymLookup(scope, st, ed);
	return node;
}

/* TODO remove? */
struct Node *NodeVarAssign(struct Scope *scope, char *st, char *ed, struct Node* value){
	struct Node *node = NodeAlloc(VAR_ASSIGN, 1, &value);
	node->item = SymLookup(scope, st, ed);
	return node;
}

struct Node *NodeTypeInit(struct Type *type, size_t values, struct Node** value){
	struct Node *node = NodeAlloc(TYPE_INIT, values, value);
	node->item = type;
	return node;
}

struct Node *NodeFuncInit(struct Type *type, size_t arguments, struct Node **argument, size_t statements, struct Node **statement){
	struct Node *sub_node[2];
	struct Node *node;
	sub_node[0] = NodeAlloc(NONE, arguments, argument);
	sub_node[1] = NodeAlloc(NONE, statements, statement);

	node = NodeAlloc(FUNC_INIT, 1, sub_node);
	node->item = type;

	return node;
}

struct Node *NodeFuncApp(struct Node *func, size_t arguments, struct Node **argument){
	return NodeAlloc(FUNC_APP, arguments, argument);
}


/* Parsing */


char *TakeTranslationUnit(char **, struct Node **);
char *TakeExternalDeclaration(char **, struct Node **);
char *TakeDeclaration(char **, struct Node **);
char *TakeDeclarationSpecifiers(char **, struct Node **);
char *TakeStorageClassSpecifier(char **, struct Node **);
char *TakeTypeSpecifier(char **, struct Node **);
char *TakeEnumSpecifier(char **, struct Node **);
char *TakeEnumeratorList(char **, struct Node **);
char *TakeEnumerator(char **, struct Node **);
char *TakeConditionalExpression(char **, struct Node **);
char *TakeLogicalOrExpression(char **, struct Node **);
char *TakeLogicalAndExpression(char **, struct Node **);
char *TakeInclusiveOrExpression(char **, struct Node **);
char *TakeExclusiveOrExpression(char **, struct Node **);
char *TakeAndExpression(char **, struct Node **);
char *TakeEqualityExpression(char **, struct Node **);
char *TakeRelationalExpression(char **, struct Node **);
char *TakeShiftExpression(char **, struct Node **);
char *TakeAdditiveExpression(char **, struct Node **);
char *TakeMultiplicativeExpression(char **, struct Node **);
char *TakeCastExpression(char **, struct Node **);
char *TakeTypeName(char **, struct Node **);
char *TakeSpecifierQualifierList(char **, struct Node **);
char *TakeTypeQualifier(char **, struct Node **);
char *TakeAbstractDeclarator(char **, struct Node **);
char *TakePointer(char **, struct Node **);
char *TakeTypeQualifierList(char **, struct Node **);
char *TakeTypeQualifierList_Rec(char **, struct Node **);
char *TakeDirectAbstractDeclarator(char **, struct Node **);
char *TakeDirectAbstractDeclarator_Rec(char **, struct Node **);
char *TakeParameterTypeList(char **, struct Node **);
char *TakeParameterList(char **, struct Node **);
char *TakeParameterDeclaration(char **, struct Node **);
char *TakeDeclarator(char **, struct Node **);
char *TakeDirectDeclarator(char **, struct Node **);
char *TakeDirectDeclarator_Rec(char **, struct Node **);
char *TakeIdentifierList(char **, struct Node **);
char *TakeIdentifierList_Rec(char **, struct Node **);
char *TakeParameterList_Rec(char **, struct Node **);
char *TakeUnaryExpression(char **, struct Node **);
char *TakeUnaryOperator(char **, struct Node **);
char *TakePostfixExpression(char **, struct Node **);
char *TakePrimaryExpression(char **, struct Node **);
char *TakeExpression(char **, struct Node **);
char *TakeAssignmentExpression(char **, struct Node **);
char *TakeAssignmentOperator(char **, struct Node **);
char *TakeExpression_Rec(char **, struct Node **);
char *TakePostfixExpression_Rec(char **, struct Node **);
char *TakeArgumentExpressionList(char **, struct Node **);
char *TakeMultiplicativeExpression_Rec(char **, struct Node **);
char *TakeAdditiveExpression_Rec(char **, struct Node **);
char *TakeShiftExpression_Rec(char **, struct Node **);
char *TakeRelationalExpression_Rec(char **, struct Node **);
char *TakeEnumeratorList_Rec(char **, struct Node **);
char *TakeStructOrUnionSpecifier(char **, struct Node **);
char *TakeStructOrUnion(char **, struct Node **);
char *TakeStructDeclarationList(char **, struct Node **);
char *TakeStructDeclaration(char **, struct Node **);
char *TakeStructDeclaratorList(char **, struct Node **);
char *TakeStructDeclarator(char **, struct Node **);
char *TakeStructDeclaratorList_Rec(char **, struct Node **);
char *TakeInitDeclaratorList(char **, struct Node **);
char *TakeInitDeclarator(char **, struct Node **);
char *TakeInitializer(char **, struct Node **);
char *TakeInitializerList(char **, struct Node **);
char *TakeInitializerList_Rec(char **, struct Node **);
char *TakeFunctionDefinition(char **, struct Node **);
char *TakeDeclarationList(char **, struct Node **);
char *TakeDeclarationList_Rec(char **, struct Node **);
char *TakeCompoundStatement(char **, struct Node **);
char *TakeStatementList(char **, struct Node **);
char *TakeStatement(char **, struct Node **);
char *TakeJumpStatement(char **, struct Node **);
char *TakeIterationStatement(char **, struct Node **);
char *TakeSelectionStatement(char **, struct Node **);
char *TakeExpressionStatement(char **, struct Node **);
char *TakeLabeledStatement(char **, struct Node **);
char *TakeStatementList_Rec(char **, struct Node **);
char *TakeTranslationUnit(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeExternalDeclaration(str, ast) && TakeTranslationUnit(str, ast))
        return start_;
    *str = start;
    if (TakeExternalDeclaration(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeExternalDeclaration(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeDeclaration(str, ast)) return start_;
    *str = start;
    if (TakeFunctionDefinition(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeDeclaration(char **str, struct Node **ast) {
    char *start_ = *str;
    if (1 && TakeDeclarationSpecifiers(str, ast)) {
        char *start = *str;
        if (1 && TakeInitDeclaratorList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ";")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, ";")) return start_;
        *str = start;
    }
    return NULL;
}
char *TakeDeclarationSpecifiers(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeStorageClassSpecifier(str, ast) &&
        TakeDeclarationSpecifiers(str, ast))
        return start_;
    *str = start;
    if (1 && TakeTypeSpecifier(str, ast) && TakeDeclarationSpecifiers(str, ast))
        return start_;
    *str = start;
    if (1 && TakeTypeQualifier(str, ast) && TakeDeclarationSpecifiers(str, ast))
        return start_;
    *str = start;
    if (TakeTypeQualifier(str, ast)) return start_;
    *str = start;
    if (TakeTypeSpecifier(str, ast)) return start_;
    *str = start;
    if (TakeStorageClassSpecifier(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeStorageClassSpecifier(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeWhiteSpace(str), TakeString(str, "register")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "auto")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "static")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "extern")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "typedef")) return start_;
    *str = start;
    return NULL;
}
char *TakeTypeSpecifier(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeWhiteSpace(str), TakeString(str, "TypedefName")) return start_;
    *str = start;
    if (TakeEnumSpecifier(str, ast)) return start_;
    *str = start;
    if (TakeStructOrUnionSpecifier(str, ast)) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "unsigned")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "signed")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "double")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "float")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "long")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "int")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "short")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "char")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "void")) return start_;
    *str = start;
    return NULL;
}
char *TakeEnumSpecifier(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "enum"))) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
            (TakeWhiteSpace(str), TakeString(str, "{")) &&
            TakeEnumeratorList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "}")))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "{")) &&
            TakeEnumeratorList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "}")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "Identifier")) return start_;
        *str = start;
    }
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "enum")) return start_;
    *str = start;
    return NULL;
}
char *TakeEnumeratorList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeEnumerator(str, ast) && TakeEnumeratorList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeEnumerator(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeEnumerator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
        (TakeWhiteSpace(str), TakeString(str, "=")) &&
        TakeConditionalExpression(str, ast))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "Identifier")) return start_;
    *str = start;
    return NULL;
}
char *TakeConditionalExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeLogicalOrExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "?")) &&
        TakeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ":")) &&
        TakeConditionalExpression(str, ast))
        return start_;
    *str = start;
    if (TakeLogicalOrExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeLogicalOrExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeLogicalAndExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "||")) &&
        TakeLogicalOrExpression(str, ast))
        return start_;
    *str = start;
    if (TakeLogicalAndExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeLogicalAndExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeInclusiveOrExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "&&")) &&
        TakeLogicalAndExpression(str, ast))
        return start_;
    *str = start;
    if (TakeInclusiveOrExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeInclusiveOrExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeExclusiveOrExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "|")) &&
        TakeInclusiveOrExpression(str, ast))
        return start_;
    *str = start;
    if (TakeExclusiveOrExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeExclusiveOrExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeAndExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "^")) &&
        TakeExclusiveOrExpression(str, ast))
        return start_;
    *str = start;
    if (TakeAndExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeAndExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeEqualityExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "&")) &&
        TakeAndExpression(str, ast))
        return start_;
    *str = start;
    if (TakeEqualityExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeEqualityExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeRelationalExpression(str, ast)) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "==")) &&
            TakeEqualityExpression(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "!=")) &&
            TakeEqualityExpression(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (TakeRelationalExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeRelationalExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeShiftExpression(str, ast) &&
        TakeRelationalExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeShiftExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeShiftExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeAdditiveExpression(str, ast) &&
        TakeShiftExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeAdditiveExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeAdditiveExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeMultiplicativeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "+")) &&
        TakeAdditiveExpression(str, ast) &&
        TakeAdditiveExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && TakeMultiplicativeExpression(str, ast) &&
        TakeAdditiveExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && TakeMultiplicativeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "+")) &&
        TakeAdditiveExpression(str, ast))
        return start_;
    *str = start;
    if (TakeMultiplicativeExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeMultiplicativeExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeCastExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "*")) &&
        TakeMultiplicativeExpression(str, ast) &&
        TakeMultiplicativeExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && TakeCastExpression(str, ast) &&
        TakeMultiplicativeExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && TakeCastExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "*")) &&
        TakeMultiplicativeExpression(str, ast))
        return start_;
    *str = start;
    if (TakeCastExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeCastExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeTypeName(str, ast) && (TakeWhiteSpace(str), TakeString(str, ")")) &&
        TakeCastExpression(str, ast))
        return start_;
    *str = start;
    if (TakeUnaryExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeTypeName(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeSpecifierQualifierList(str, ast) &&
        TakeAbstractDeclarator(str, ast))
        return start_;
    *str = start;
    if (TakeSpecifierQualifierList(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeSpecifierQualifierList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeTypeSpecifier(str, ast) &&
        TakeSpecifierQualifierList(str, ast))
        return start_;
    *str = start;
    if (1 && TakeTypeQualifier(str, ast) &&
        TakeSpecifierQualifierList(str, ast))
        return start_;
    *str = start;
    if (TakeTypeQualifier(str, ast)) return start_;
    *str = start;
    if (TakeTypeSpecifier(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeTypeQualifier(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeWhiteSpace(str), TakeString(str, "volatile")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "const")) return start_;
    *str = start;
    return NULL;
}
char *TakeAbstractDeclarator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakePointer(str, ast) && TakeDirectAbstractDeclarator(str, ast))
        return start_;
    *str = start;
    if (TakeDirectAbstractDeclarator(str, ast)) return start_;
    *str = start;
    if (TakePointer(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakePointer(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "*"))) {
        char *start = *str;
        if (1 && TakeTypeQualifierList(str, ast) && TakePointer(str, ast))
            return start_;
        *str = start;
        if (TakePointer(str, ast)) return start_;
        *str = start;
        if (TakeTypeQualifierList(str, ast)) return start_;
        *str = start;
    }
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "*")) return start_;
    *str = start;
    return NULL;
}
char *TakeTypeQualifierList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeTypeQualifier(str, ast) && TakeTypeQualifierList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeTypeQualifier(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeTypeQualifierList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeTypeQualifier(str, ast) && TakeTypeQualifierList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeTypeQualifier(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeDirectAbstractDeclarator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
        char *start = *str;
        if (1 && TakeAbstractDeclarator(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")) &&
            TakeDirectAbstractDeclarator_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && TakeParameterTypeList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")) &&
            TakeDirectAbstractDeclarator_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
            TakeDirectAbstractDeclarator_Rec(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "["))) {
        char *start = *str;
        if (1 && TakeConditionalExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "]")) &&
            TakeDirectAbstractDeclarator_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "]")) &&
            TakeDirectAbstractDeclarator_Rec(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
        char *start = *str;
        if (1 && TakeAbstractDeclarator(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (1 && TakeParameterTypeList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, ")")) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "["))) {
        char *start = *str;
        if (1 && TakeConditionalExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "]")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "]")) return start_;
        *str = start;
    }
    *str = start;
    return NULL;
}
char *TakeDirectAbstractDeclarator_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
            char *start = *str;
            if (1 && TakeParameterTypeList(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakeDirectAbstractDeclarator_Rec(str, ast))
                return start_;
            *str = start;
            if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakeDirectAbstractDeclarator_Rec(str, ast))
                return start_;
            *str = start;
        }
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "["))) {
            char *start = *str;
            if (1 && TakeConditionalExpression(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, "]")) &&
                TakeDirectAbstractDeclarator_Rec(str, ast))
                return start_;
            *str = start;
            if (1 && (TakeWhiteSpace(str), TakeString(str, "]")) &&
                TakeDirectAbstractDeclarator_Rec(str, ast))
                return start_;
            *str = start;
        }
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
        char *start = *str;
        if (1 && TakeParameterTypeList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, ")")) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "["))) {
        char *start = *str;
        if (1 && TakeConditionalExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "]")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "]")) return start_;
        *str = start;
    }
    *str = start;
    return NULL;
}
char *TakeParameterTypeList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeParameterList(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ",")) &&
        (TakeWhiteSpace(str), TakeString(str, "...")))
        return start_;
    *str = start;
    if (TakeParameterList(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeParameterList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeParameterDeclaration(str, ast) &&
        TakeParameterList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeParameterDeclaration(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeParameterDeclaration(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeDeclarationSpecifiers(str, ast)) {
        char *start = *str;
        if (TakeAbstractDeclarator(str, ast)) return start_;
        *str = start;
        if (TakeDeclarator(str, ast)) return start_;
        *str = start;
    }
    *str = start;
    if (TakeDeclarationSpecifiers(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeDeclarator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakePointer(str, ast) && TakeDirectDeclarator(str, ast))
        return start_;
    *str = start;
    if (TakeDirectDeclarator(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeDirectDeclarator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeDeclaration(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ")")) &&
        TakeDirectDeclarator_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
        TakeDirectDeclarator_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeDeclaration(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ")")))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "Identifier")) return start_;
    *str = start;
    return NULL;
}
char *TakeDirectDeclarator_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
            char *start = *str;
            if (1 && TakeParameterTypeList(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakeDirectDeclarator_Rec(str, ast))
                return start_;
            *str = start;
            if (1 && TakeIdentifierList(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakeDirectDeclarator_Rec(str, ast))
                return start_;
            *str = start;
            if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakeDirectDeclarator_Rec(str, ast))
                return start_;
            *str = start;
        }
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "["))) {
            char *start = *str;
            if (1 && TakeConditionalExpression(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, "]")) &&
                TakeDirectDeclarator_Rec(str, ast))
                return start_;
            *str = start;
            if (1 && (TakeWhiteSpace(str), TakeString(str, "]")) &&
                TakeDirectDeclarator_Rec(str, ast))
                return start_;
            *str = start;
        }
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
        char *start = *str;
        if (1 && TakeParameterTypeList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (1 && TakeIdentifierList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, ")")) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "["))) {
        char *start = *str;
        if (1 && TakeConditionalExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "]")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "]")) return start_;
        *str = start;
    }
    *str = start;
    return NULL;
}
char *TakeIdentifierList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
        TakeIdentifierList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "Identifier")) return start_;
    *str = start;
    return NULL;
}
char *TakeIdentifierList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
        TakeIdentifierList_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        (TakeWhiteSpace(str), TakeString(str, "Identifier")))
        return start_;
    *str = start;
    return NULL;
}
char *TakeParameterList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeParameterDeclaration(str, ast) && TakeParameterList_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeParameterDeclaration(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeUnaryExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "sizeof"))) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "(")) &&
            TakeTypeName(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (TakeUnaryExpression(str, ast)) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "++")) &&
        TakeUnaryExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "--")) &&
        TakeUnaryExpression(str, ast))
        return start_;
    *str = start;
    if (1 && TakeUnaryOperator(str, ast) && TakeUnaryExpression(str, ast))
        return start_;
    *str = start;
    if (TakePostfixExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeUnaryOperator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeWhiteSpace(str), TakeString(str, "!")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "~")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "-")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "+")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "*")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "&")) return start_;
    *str = start;
    return NULL;
}
char *TakePostfixExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakePrimaryExpression(str, ast) &&
        TakePostfixExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (TakePrimaryExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakePrimaryExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeExpression(str, ast) && (TakeWhiteSpace(str), TakeString(str, ")")))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "StringLiteral")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "CharacterConstant"))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "FloatingConstant")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "IntegerConstant")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "Identifier")) return start_;
    *str = start;
    return NULL;
}
char *TakeExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeAssignmentExpression(str, ast) && TakeExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeAssignmentExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeAssignmentExpression(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeUnaryExpression(str, ast) &&
        TakeAssignmentOperator(str, ast) && TakeAssignmentExpression(str, ast))
        return start_;
    *str = start;
    if (TakeConditionalExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeAssignmentOperator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeWhiteSpace(str), TakeString(str, "|=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "^=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "&=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, ">>=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "<<=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "-=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "+=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "%=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "/=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "*=")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "=")) return start_;
    *str = start;
    return NULL;
}
char *TakeExpression_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeAssignmentExpression(str, ast) && TakeExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeAssignmentExpression(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakePostfixExpression_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
            char *start = *str;
            if (1 && TakeArgumentExpressionList(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakePostfixExpression_Rec(str, ast))
                return start_;
            *str = start;
            if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                TakePostfixExpression_Rec(str, ast))
                return start_;
            *str = start;
        }
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "[")) &&
            TakeExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "]")) &&
            TakePostfixExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ".")) &&
            (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
            TakePostfixExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "->")) &&
            (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
            TakePostfixExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "--")) &&
            TakePostfixExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "++")) &&
            TakePostfixExpression_Rec(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "("))) {
        char *start = *str;
        if (1 && TakeArgumentExpressionList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ")")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, ")")) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "[")) &&
        TakeExpression(str, ast) && (TakeWhiteSpace(str), TakeString(str, "]")))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ".")) &&
        (TakeWhiteSpace(str), TakeString(str, "Identifier")))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "->")) &&
        (TakeWhiteSpace(str), TakeString(str, "Identifier")))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "--")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "++")) return start_;
    *str = start;
    return NULL;
}
char *TakeArgumentExpressionList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeAssignmentExpression(str, ast))
        return start_;
    *str = start;
    if (TakeAssignmentExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeMultiplicativeExpression_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "/")) &&
            TakeCastExpression(str, ast) &&
            TakeMultiplicativeExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "%")) &&
            TakeCastExpression(str, ast) &&
            TakeMultiplicativeExpression_Rec(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "/")) &&
        TakeCastExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "%")) &&
        TakeCastExpression(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeAdditiveExpression_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "-")) &&
        TakeMultiplicativeExpression(str, ast) &&
        TakeAdditiveExpression_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "-")) &&
        TakeMultiplicativeExpression(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeShiftExpression_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ">>")) &&
            TakeAdditiveExpression(str, ast) &&
            TakeShiftExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "<<")) &&
            TakeAdditiveExpression(str, ast) &&
            TakeShiftExpression_Rec(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ">>")) &&
        TakeAdditiveExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "<<")) &&
        TakeAdditiveExpression(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeRelationalExpression_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "<")) &&
            TakeShiftExpression(str, ast) &&
            TakeRelationalExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ">")) &&
            TakeShiftExpression(str, ast) &&
            TakeRelationalExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "<=")) &&
            TakeShiftExpression(str, ast) &&
            TakeRelationalExpression_Rec(str, ast))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ">=")) &&
            TakeShiftExpression(str, ast) &&
            TakeRelationalExpression_Rec(str, ast))
            return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "<")) &&
        TakeShiftExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ">")) &&
        TakeShiftExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "<=")) &&
        TakeShiftExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ">=")) &&
        TakeShiftExpression(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeEnumeratorList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeEnumerator(str, ast) && TakeEnumeratorList_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeEnumerator(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeStructOrUnionSpecifier(char **str, struct Node **ast) {
    char *start_ = *str;
    if (1 && TakeStructOrUnion(str, ast)) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
            (TakeWhiteSpace(str), TakeString(str, "{")) &&
            TakeStructDeclarationList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "}")))
            return start_;
        *str = start;
        if (1 && (TakeWhiteSpace(str), TakeString(str, "{")) &&
            TakeStructDeclarationList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "}")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "Identifier")) return start_;
        *str = start;
    }
    return NULL;
}
char *TakeStructOrUnion(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeWhiteSpace(str), TakeString(str, "union")) return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, "struct")) return start_;
    *str = start;
    return NULL;
}
char *TakeStructDeclarationList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeStructDeclaration(str, ast) &&
        TakeStructDeclarationList(str, ast))
        return start_;
    *str = start;
    if (TakeStructDeclaration(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeStructDeclaration(char **str, struct Node **ast) {
    char *start_ = *str;
    if (1 && TakeSpecifierQualifierList(str, ast) &&
        TakeStructDeclaratorList(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ";")))
        return start_;
    return NULL;
}
char *TakeStructDeclaratorList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeStructDeclarator(str, ast) &&
        TakeStructDeclaratorList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeStructDeclarator(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeStructDeclarator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeDeclarator(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ":")) &&
        TakeConditionalExpression(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ":")) &&
        TakeConditionalExpression(str, ast))
        return start_;
    *str = start;
    if (TakeDeclarator(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeStructDeclaratorList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeStructDeclarator(str, ast) &&
        TakeStructDeclaratorList_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeStructDeclarator(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeInitDeclaratorList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeInitDeclarator(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeInitDeclaratorList(str, ast))
        return start_;
    *str = start;
    if (TakeInitDeclarator(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeInitDeclarator(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeDeclarator(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "=")) &&
        TakeInitializer(str, ast))
        return start_;
    *str = start;
    if (TakeDeclarator(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeInitializer(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "{")) &&
        TakeInitializerList(str, ast)) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
            (TakeWhiteSpace(str), TakeString(str, "}")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "}")) return start_;
        *str = start;
    }
    *str = start;
    if (TakeAssignmentExpression(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeInitializerList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeInitializer(str, ast) && TakeInitializerList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeInitializer(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeInitializerList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeInitializer(str, ast) && TakeInitializerList_Rec(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, ",")) &&
        TakeInitializer(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeFunctionDefinition(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeDeclarator(str, ast)) {
        char *start = *str;
        if (1 && TakeDeclarationList(str, ast) &&
            TakeCompoundStatement(str, ast))
            return start_;
        *str = start;
        if (TakeCompoundStatement(str, ast)) return start_;
        *str = start;
    }
    *str = start;
    if (1 && TakeDeclarationSpecifiers(str, ast) && TakeDeclarator(str, ast)) {
        char *start = *str;
        if (1 && TakeDeclarationList(str, ast) &&
            TakeCompoundStatement(str, ast))
            return start_;
        *str = start;
        if (TakeCompoundStatement(str, ast)) return start_;
        *str = start;
    }
    *str = start;
    return NULL;
}
char *TakeDeclarationList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeDeclaration(str, ast) && TakeDeclarationList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeDeclaration(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeDeclarationList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeDeclaration(str, ast) && TakeDeclarationList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeDeclaration(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeCompoundStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "{"))) {
        char *start = *str;
        if (1 && TakeDeclarationList(str, ast)) {
            char *start = *str;
            if (1 && TakeStatementList(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, "}")))
                return start_;
            *str = start;
            if (TakeWhiteSpace(str), TakeString(str, "}")) return start_;
            *str = start;
        }
        *str = start;
        if (1 && TakeStatementList(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "}")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, "}")) return start_;
        *str = start;
    }
    return NULL;
}
char *TakeStatementList(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeStatement(str, ast) && TakeStatementList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeStatement(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (TakeJumpStatement(str, ast)) return start_;
    *str = start;
    if (TakeIterationStatement(str, ast)) return start_;
    *str = start;
    if (TakeSelectionStatement(str, ast)) return start_;
    *str = start;
    if (TakeExpressionStatement(str, ast)) return start_;
    *str = start;
    if (TakeCompoundStatement(str, ast)) return start_;
    *str = start;
    if (TakeLabeledStatement(str, ast)) return start_;
    *str = start;
    return NULL;
}
char *TakeJumpStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "return"))) {
        char *start = *str;
        if (1 && TakeExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ";")))
            return start_;
        *str = start;
        if (TakeWhiteSpace(str), TakeString(str, ";")) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "goto")) &&
        (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
        (TakeWhiteSpace(str), TakeString(str, ";")))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "continue")) &&
        (TakeWhiteSpace(str), TakeString(str, ";")))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "break")) &&
        (TakeWhiteSpace(str), TakeString(str, ";")))
        return start_;
    *str = start;
    return NULL;
}
char *TakeIterationStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "for")) &&
        (TakeWhiteSpace(str), TakeString(str, "("))) {
        char *start = *str;
        if (1 && (TakeWhiteSpace(str), TakeString(str, ";"))) {
            char *start = *str;
            if (1 && (TakeWhiteSpace(str), TakeString(str, ";"))) {
                char *start = *str;
                if (1 && TakeExpression(str, ast) &&
                    (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
                if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
            }
            *str = start;
            if (1 && TakeExpression(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, ";"))) {
                char *start = *str;
                if (1 && TakeExpression(str, ast) &&
                    (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
                if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
            }
            *str = start;
        }
        *str = start;
        if (1 && TakeExpression(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, ";"))) {
            char *start = *str;
            if (1 && (TakeWhiteSpace(str), TakeString(str, ";"))) {
                char *start = *str;
                if (1 && TakeExpression(str, ast) &&
                    (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
                if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
            }
            *str = start;
            if (1 && TakeExpression(str, ast) &&
                (TakeWhiteSpace(str), TakeString(str, ";"))) {
                char *start = *str;
                if (1 && TakeExpression(str, ast) &&
                    (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
                if (1 && (TakeWhiteSpace(str), TakeString(str, ")")) &&
                    TakeStatement(str, ast))
                    return start_;
                *str = start;
            }
            *str = start;
        }
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "while")) &&
        (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ")")) && TakeStatement(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "do")) &&
        TakeStatement(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, "while")) &&
        (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ")")) &&
        (TakeWhiteSpace(str), TakeString(str, ";")))
        return start_;
    *str = start;
    return NULL;
}
char *TakeSelectionStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "if")) &&
        (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ")"))) {
        char *start = *str;
        if (1 && TakeStatement(str, ast) &&
            (TakeWhiteSpace(str), TakeString(str, "else")) &&
            TakeStatement(str, ast))
            return start_;
        *str = start;
        if (TakeStatement(str, ast)) return start_;
        *str = start;
    }
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "switch")) &&
        (TakeWhiteSpace(str), TakeString(str, "(")) &&
        TakeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ")")) && TakeStatement(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeExpressionStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ";")))
        return start_;
    *str = start;
    if (TakeWhiteSpace(str), TakeString(str, ";")) return start_;
    *str = start;
    return NULL;
}
char *TakeLabeledStatement(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "Identifier")) &&
        (TakeWhiteSpace(str), TakeString(str, ":")) && TakeStatement(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "case")) &&
        TakeConditionalExpression(str, ast) &&
        (TakeWhiteSpace(str), TakeString(str, ":")) && TakeStatement(str, ast))
        return start_;
    *str = start;
    if (1 && (TakeWhiteSpace(str), TakeString(str, "default")) &&
        (TakeWhiteSpace(str), TakeString(str, ":")) && TakeStatement(str, ast))
        return start_;
    *str = start;
    return NULL;
}
char *TakeStatementList_Rec(char **str, struct Node **ast) {
    char *start_ = *str;
    char *start = *str;
    if (1 && TakeStatement(str, ast) && TakeStatementList_Rec(str, ast))
        return start_;
    *str = start;
    if (TakeStatement(str, ast)) return start_;
    *str = start;
    return NULL;
}





int main(int argc, char **argv){
	LexerTests();

	char *str = "int Identifier; int Identifier() {}";

	char *cur = str;
	if(TakeTranslationUnit(&cur, NULL)){
		printf("yay!\n");	
	}else{
		printf("hmm\n");	
	}


	if(argc != 2){
		printf("usage: mycc [file]\r\n");
		return 0;
	}

}

