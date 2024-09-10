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

/* Parsing */

/*
ISO c89 39
PrimaryExpression -> Identifier
PrimaryExpression -> IntegerConstant
PrimaryExpression -> FloatingConstant
PrimaryExpression -> CharacterConstant
PrimaryExpression -> StringLiteral
PrimaryExpression -> ( Expression )

ISO c89 39
PostfixExpression -> PrimaryExpression
PostfixExpression -> PostfixExpression [ Expression ]
PostfixExpression -> PostfixExpression ( ArgumentExpressionList )
PostfixExpression -> PostfixExpression ( )
PostfixExpression -> PostfixExpression . Identifier
PostfixExpression -> PostfixExpression "->" Identifier
PostfixExpression -> PostfixExpression ++
PostfixExpression -> PostfixExpression --

ArgumentExpressionList -> AssignmentExpression
ArgumentExpressionList -> , AssignmentExpression

ISO c89 43
UnaryExpression -> PostfixExpression
UnaryExpression -> ++ UnaryExpression
UnaryExpression -> -- UnaryExpression
UnaryExpression -> UnaryOperator UnaryExpression
UnaryExpression -> "sizeof" UnaryExpression
UnaryExpression -> "sizeof" ( TypeName )

UnaryOperator -> &
UnaryOperator -> *
UnaryOperator -> +
UnaryOperator -> -
UnaryOperator -> ~
UnaryOperator -> !

ISO c89 45
CastExpression -> UnaryExpression
CastExpression -> ( TypeName ) CastExpression

ISO c89 46
MultiplicativeExpression -> CastExpression
MultiplicativeExpression -> MultiplicativeExpression * CastExpression
MultiplicativeExpression -> MultiplicativeExpression / CastExpression
MultiplicativeExpression -> MultiplicativeExpression % CastExpression

ISO c89 46
AdditiveExpression -> MultiplicativeExpression
AdditiveExpression -> AdditiveExpression + MultiplicativeExpression
AdditiveExpression -> AdditiveExpression - MultiplicativeExpression

ISO c89 48
ShiftExpression -> AdditiveExpression
ShiftExpression -> ShiftExpression >> AdditiveExpression
ShiftExpression -> ShiftExpression << AdditiveExpression

ISO c89 48
RelationalExpression -> ShiftExpression
RelationalExpression -> RelationalExpression < ShiftExpression
RelationalExpression -> RelationalExpression > ShiftExpression
RelationalExpression -> RelationalExpression <= ShiftExpression
RelationalExpression -> RelationalExpression >= ShiftExpression

ISO c89 49
EqualityExpression -> RelationalExpression
EqualityExpression -> EqualityExpression == RelationalExpression
EqualityExpression -> EqualityExpression != RelationalExpression

ISO c89 50
AndExpression -> EqualityExpression
AndExpression -> AndExpression & EqualityExpression

ExclusiveOrExpression -> AndExpression
ExclusiveOrExpression -> ExclusiveOrExpression ^ AndExpression

InclusiveOrExpression -> ExclusiveOrExpression
InclusiveOrExpression -> InclusiveOrExpression | ExclusiveOrExpression

ISO c89 51
LogicalAndExpression -> InclusiveOrExpression
LogicalAndExpression -> LogicalAndExpression && InclusiveOrExpression

LogicalOrExpression -> LogicalAndExpression
LogicalOrExpression -> LogicalOrExpression || LogicalAndExpression

ConditionalExpression -> LogicalOrExpression
ConditionalExpression -> LogicalOrExpression ? Expression : ConditionalExpression

ISO c89 53
AssignmentExpression -> ConditionalExpression
AssignmentExpression -> UnaryExpression AssignmentOperator AssignmentExpression

AssignmentOperator -> =
AssignmentOperator -> *=
AssignmentOperator -> /=
AssignmentOperator -> %=
AssignmentOperator -> +=
AssignmentOperator -> -=
AssignmentOperator -> <<=
AssignmentOperator -> >>=
AssignmentOperator -> &=
AssignmentOperator -> ^=
AssignmentOperator -> |=

ISO c89 54
Expression -> AssignmentExpression
Expression -> Expression , AssignmentExpression
*/





















/*
ISO c89 57
Declaration -> DeclarationSpecifiers ;
Declaration -> DeclarationSpecifiers InitDeclaratorList ;

DeclarationSpecifiers -> StorageClassSpecifier
DeclarationSpecifiers -> StorageClassSpecifier DeclarationSpecifiers
DeclarationSpecifiers -> TypeSpecifier
DeclarationSpecifiers -> TypeSpecifier DeclarationSpecifiers
DeclarationSpecifiers -> TypeQualifier
DeclarationSpecifiers -> TypeQualifier DeclarationSpecifiers

InitDeclaratorList -> InitDeclarator
InitDeclaratorList -> InitDeclaratorList , InitDeclarator

InitDeclarator -> Declarator
InitDeclarator -> Declarator = Initializer

ISO c89 58
StorageClassSpecifier -> typedef
StorageClassSpecifier -> extern
StorageClassSpecifier -> static
StorageClassSpecifier -> auto
StorageClassSpecifier -> register

TypeSpecifier -> void
TypeSpecifier -> char
TypeSpecifier -> short
TypeSpecifier -> int
TypeSpecifier -> long
TypeSpecifier -> float
TypeSpecifier -> double
TypeSpecifier -> signed
TypeSpecifier -> unsigned
TypeSpecifier -> StructOrUnionSpecifier
TypeSpecifier -> EnumSpecifier
TypeSpecifier -> TypedefName

ISO c89 59
StructOrUnionSpecifier -> StructOrUnion Identifier { StructDeclarationList }
StructOrUnionSpecifier -> StructOrUnion { StructDeclarationList }
StructOrUnionSpecifier -> StructOrUnion Identifier

StructOrUnion -> struct
StructOrUnion -> union

StructDeclarationList -> StructDeclaration
StructDeclarationList -> StructDeclarationList StructDeclaration

StructDeclaration -> SpecifierQualifierList StructDeclaratorList ;

SpecifierQualifierList -> TypeSpecifier SpecifierQualifierList
SpecifierQualifierList -> TypeSpecifier
SpecifierQualifierList -> TypeQualifier SpecifierQualifierList
SpecifierQualifierList -> TypeQualifier

StructDeclaratorList -> StructDeclarator
StructDeclaratorList -> StructDeclaratorList , StructDeclarator

StructDeclarator -> Declarator
StructDeclarator -> Declarator : ConditionalExpression
StructDeclarator -> : ConditionalExpression

ISO c89 61
EnumSpecifier -> enum Identifier { EnumeratorList }
EnumSpecifier -> enum { EnumeratorList }
EnumSpecifier -> enum Identifier
EnumSpecifier -> enum

EnumeratorList -> Enumerator
EnumeratorList -> EnumeratorList , Enumerator

Enumerator -> Identifier
Enumerator -> Identifier = ConditionalExpression

ISO c89 64
TypeQualifier -> const
TypeQualifier -> volatile

ISO c89 65
Declarator -> Pointer DirectDeclarator
Declarator -> DirectDeclarator

DirectDeclarator -> Identifier
DirectDeclarator -> ( Declaration )
DirectDeclarator -> DirectDeclarator [ ConditionalExpression ]
DirectDeclarator -> DirectDeclarator [ ]
DirectDeclarator -> DirectDeclarator ( ParameterTypeList )
DirectDeclarator -> DirectDeclarator ( IdentifierList )
DirectDeclarator -> DirectDeclarator ( )

Pointer -> *
Pointer -> * TypeQualifierList
Pointer -> * TypeQualifierList Pointer
Pointer -> * Pointer

TypeQualifierList -> TypeQualifier
TypeQualifierList -> TypeQualifierList TypeQualifier

ParameterTypeList -> ParameterList
ParameterTypeList -> ParameterList , ...

ParameterList -> ParameterDeclaration
ParameterList -> ParameterList , ParameterDeclaration

ParameterDeclaration -> DeclarationSpecifiers Declarator
ParameterDeclaration -> DeclarationSpecifiers AbstractDeclarator
ParameterDeclaration -> DeclarationSpecifiers

IdentifierList -> Identifier
IdentifierList -> IdentifierList , Identifier

ISO c89 69
TypeName -> SpecifierQualifierList AbstractDeclarator
TypeName -> SpecifierQualifierList

AbstractDeclarator -> Pointer
AbstractDeclarator -> Pointer AbstractDeclarator
AbstractDeclarator -> AbstractDeclarator

DirectAbstractDeclarator -> ( AbstractDeclarator )
DirectAbstractDeclarator -> DirectAbstractDeclarator [ ConditionalExpression ]
DirectAbstractDeclarator -> [ ConditionalExpression ]
DirectAbstractDeclarator -> DirectAbstractDeclarator [ ]
DirectAbstractDeclarator -> [ ]
DirectAbstractDeclarator -> DirectAbstractDeclarator ( ParameterTypeList )
DirectAbstractDeclarator -> ( ParameterTypeList )
DirectAbstractDeclarator -> DirectAbstractDeclarator ( )
DirectAbstractDeclarator -> ( )

ISO c89 71
Initializer -> AssignmentExpression
Initializer -> { InitializerList }
Initializer -> { InitializerList, }

InitializerList -> Initializer
InitializerList -> InitializerList , Initializer

*/




























/*
ISO c89 75
Statement -> LabeledStatement
Statement -> CompoundStatement
Statement -> ExpressionStatement
Statement -> SelectionStatement
Statement -> IterationStatement
Statement -> JumpStatement

LabeledStatement -> Identifer : Statement
LabeledStatement -> case ConditionalExpression : Statement
LabeledStatement -> default : Statement

CompoundStatement -> { DeclarationList StatementList }
CompoundStatement -> { StatementList }
CompoundStatement -> { DeclarationList }
CompoundStatement -> { }

DeclarationList -> Declaration
DeclarationList -> DeclarationList Declaration

StatementList -> Statement
StatementList -> StatementList Statement

ISO c89 76
ExpressionStatement -> Expression ;
ExpressionStatement -> ;

SelectionStatement -> if ( Expression ) Statement
SelectionStatement -> if ( Expression ) Statement else Statement
SelectionStatement -> switch ( Expression ) Statement

ISO c89 78
IterationStatement -> while ( Expression ) Statement
IterationStatement -> do Statement while ( Expression ) ;
IterationStatement -> for ( Expression ; Expression ; Expression ) Statement
IterationStatement -> for ( ; Expression ; Expression ) Statement
IterationStatement -> for ( Expression ; ; Expression ) Statement
IterationStatement -> for ( ; ; Expression ) Statement
IterationStatement -> for ( Expression ; Expression ; ) Statement
IterationStatement -> for ( ; Expression ; ) Statement
IterationStatement -> for ( Expression ; ; ) Statement
IterationStatement -> for ( ; ; ) Statement

ISO c89 79
JumpStatement -> goto Identifer ;
JumpStatement -> continue ;
JumpStatement -> break ;
JumpStatement -> return Expression ;
JumpStatement -> return ;


ISO c89 81
TranslationUnit -> ExternalDeclaration
TranslationUnit -> TranslationUnit ExternalDeclaration

ExternalDeclaration -> FunctionDefinition
ExternalDeclaration -> Declaration

FunctionDefinition -> DeclarationSpecifiers Declarator DeclarationList CompoundStatement
FunctionDefinition -> Declarator DeclarationList CompoundStatement
FunctionDefinition -> DeclarationSpecifiers Declarator CompoundStatement
FunctionDefinition -> Declarator CompoundStatement
*/

int main(int argc, char **argv){
	LexerTests();

	if(argc != 2){
		printf("usage: mycc [file]\r\n");
		return 0;
	}

}

