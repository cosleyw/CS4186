char *TakeTranslationUnit(char **, struct Node **);
char *TakeExternalDeclaration(char **, struct Node **);
char *TakeFunctionDefinition(char **, struct Node **);
char *TakeDeclarationSpecifiers(char **, struct Node **);
char *TakeStorageClassSpecifier(char **, struct Node **);
char *TakeTypeSpecifier(char **, struct Node **);
char *TakeStructSpecifier(char **, struct Node **);
char *TakeIdentifier(char **, struct Node **);
char *TakeStructDeclarationList(char **, struct Node **);
char *TakeStructDeclaration(char **, struct Node **);
char *TakeSpecifierQualifierList(char **, struct Node **);
char *TakeTypeQualifier(char **, struct Node **);
char *TakeStructDeclaratorList(char **, struct Node **);
char *TakeStructDeclarator(char **, struct Node **);
char *TakeDeclarator(char **, struct Node **);
char *TakePointer(char **, struct Node **);
char *TakeTypeQualifierList(char **, struct Node **);
char *TakeDirectDeclarator(char **, struct Node **);
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
char *TakeAbstractDeclarator(char **, struct Node **);
char *TakeDirectAbstractDeclarator(char **, struct Node **);
char *TakeParameterTypeList(char **, struct Node **);
char *TakeParameterList(char **, struct Node **);
char *TakeParameterDeclaration(char **, struct Node **);
char *TakeUnaryExpression(char **, struct Node **);
char *TakePostfixExpression(char **, struct Node **);
char *TakePrimaryExpression(char **, struct Node **);
char *TakeExpression(char **, struct Node **);
char *TakeAssignmentExpression(char **, struct Node **);
char *TakeIntegerConstant(char **, struct Node **);
char *TakeFloatingConstant(char **, struct Node **);
char *TakeCharacterConstant(char **, struct Node **);
char *TakeStringLiteral(char **, struct Node **);
char *TakeArgumentExpressionList(char **, struct Node **);
char *TakeIdentifierList(char **, struct Node **);
char *TakeUnionSpecifier(char **, struct Node **);
char *TakeEnumSpecifier(char **, struct Node **);
char *TakeEnumeratorList(char **, struct Node **);
char *TakeEnumerator(char **, struct Node **);
char *TakeTypedefName(char **, struct Node **);
char *TakeDeclarationList(char **, struct Node **);
char *TakeDeclaration(char **, struct Node **);
char *TakeInitDeclaratorList(char **, struct Node **);
char *TakeInitDeclarator(char **, struct Node **);
char *TakeInitializer(char **, struct Node **);
char *TakeInitializerList(char **, struct Node **);
char *TakeCompoundStatement(char **, struct Node **);
char *TakeStatementList(char **, struct Node **);
char *TakeStatement(char **, struct Node **);
char *TakeLabeledStatement(char **, struct Node **);
char *TakeExpressionStatement(char **, struct Node **);
char *TakeSelectionStatement(char **, struct Node **);
char *TakeIterationStatement(char **, struct Node **);
char *TakeJumpStatement(char **, struct Node **);
char *TakeTranslationUnit(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeExternalDeclaration(str, node + 0)) {
    char *start = *str;
    if (TakeTranslationUnit(str, node + 1)) {
      *ast = Concat(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeExternalDeclaration(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeFunctionDefinition(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeDeclaration(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeFunctionDefinition(char **str, struct Node **ast) {
  struct Node *node[5];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (5));
  char *start = *str;
  if (TakeDeclarationSpecifiers(str, node + 0) &&
      TakeDeclarator(str, node + 1)) {
    char *start = *str;
    if (TakeDeclarationList(str, node + 2) &&
        TakeCompoundStatement(str, node + 3)) {
      *ast = SetNode(Type(Specifier(node[0]), node[1]), 2,
                     List(node[2], List(node[3], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (TakeCompoundStatement(str, node + 2)) {
      *ast = SetNode(Type(Specifier(node[0]), node[1]), 2, node[2]);
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeDeclarator(str, node + 0)) {
    char *start = *str;
    if (TakeDeclarationList(str, node + 1) &&
        TakeCompoundStatement(str, node + 2)) {
      *ast = SetNode(Type(Specifier(NULL), node[0]), 2,
                     List(node[1], List(node[2], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeCompoundStatement(str, node + 1)) {
      *ast = SetNode(Type(Specifier(NULL), node[0]), 2, node[1]);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeDeclarationSpecifiers(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeStorageClassSpecifier(str, node + 0)) {
    char *start = *str;
    if (TakeDeclarationSpecifiers(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeTypeSpecifier(str, node + 0)) {
    char *start = *str;
    if (TakeDeclarationSpecifiers(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeTypeQualifier(str, node + 0)) {
    char *start = *str;
    if (TakeDeclarationSpecifiers(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStorageClassSpecifier(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  char *start = *str;
  if (TakeString(str, "typedef")) {
    *ast = Node(TYPEDEF, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "extern")) {
    *ast = Node(EXTERN, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "static")) {
    *ast = Node(STATIC, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "auto")) {
    *ast = Node(AUTO, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "register")) {
    *ast = Node(REGISTER, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeTypeSpecifier(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeString(str, "void")) {
    *ast = Node(VOID, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "char")) {
    *ast = Node(CHAR, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "short")) {
    *ast = Node(SHORT, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "int")) {
    *ast = Node(INT, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "long")) {
    *ast = Node(LONG, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "float")) {
    *ast = Node(FLOAT, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "double")) {
    *ast = Node(DOUBLE, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "signed")) {
    *ast = Node(SIGNED, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "unsigned")) {
    *ast = Node(UNSIGNED, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeStructSpecifier(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeUnionSpecifier(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeEnumSpecifier(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeTypedefName(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStructSpecifier(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeString(str, "struct")) {
    char *start = *str;
    if (TakeIdentifier(str, node + 0)) {
      char *start = *str;
      if (TakeString(str, "{") && TakeStructDeclarationList(str, node + 1) &&
          TakeString(str, "}")) {
        *ast = TypeDef(node[0], Node(STRUCT, 1, node + 1));
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = SetType(node[0], TYPEREF);
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, "{") && TakeStructDeclarationList(str, node + 0) &&
        TakeString(str, "}")) {
      *ast = Node(STRUCT, 1, node);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeIdentifier(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeString(str, "identifier")) {
    *ast = Symbol(sym("TODO"));
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStructDeclarationList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeStructDeclaration(str, node + 0)) {
    char *start = *str;
    if (TakeStructDeclarationList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStructDeclaration(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeSpecifierQualifierList(str, node + 0) &&
      TakeStructDeclaratorList(str, node + 1) && TakeString(str, ";")) {
    *ast = TypeList(Specifier(node[0]), node[1]);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeSpecifierQualifierList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeTypeSpecifier(str, node + 0)) {
    char *start = *str;
    if (TakeSpecifierQualifierList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeTypeQualifier(str, node + 0)) {
    char *start = *str;
    if (TakeSpecifierQualifierList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeTypeQualifier(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  char *start = *str;
  if (TakeString(str, "const")) {
    *ast = Node(CONST, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "volatile")) {
    *ast = Node(VOLATILE, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStructDeclaratorList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeStructDeclarator(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeStructDeclaratorList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStructDeclarator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeDeclarator(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ":") && TakeConditionalExpression(str, node + 1)) {
      *ast = Node(BITFIELD, 2, node);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, ":") && TakeConditionalExpression(str, node + 0)) {
    *ast = Node(BITFIELD, 1, node);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeDeclarator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakePointer(str, node + 0) && TakeDirectDeclarator(str, node + 1)) {
    *ast = List(node[0], List(node[1], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeDirectDeclarator(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakePointer_Rec(char **str, struct Node **ast, struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  if (TakeString(str, "*")) {
    char *start = *str;
    if (TakeTypeQualifierList(str, node + 1)) {
      char *start = *str;
      if (TakePointer_Rec(str, ast,
                          node[0] = SetType(List(node[0], node[1]), POINTER))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakePointer_Rec(str, ast,
                        node[0] = SetType(List(node[0], NULL), POINTER))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakePointer(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeString(str, "*")) {
    char *start = *str;
    if (TakeTypeQualifierList(str, node + 0)) {
      char *start = *str;
      if (TakePointer_Rec(str, ast, SetType(List(NULL, node[0]), POINTER))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = SetType(List(NULL, node[0]), POINTER);
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakePointer_Rec(str, ast, SetType(List(NULL, NULL), POINTER))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 0);
    if (1) {
      *ast = SetType(List(NULL, NULL), POINTER);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeTypeQualifierList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeTypeQualifier(str, node + 0)) {
    char *start = *str;
    if (TakeTypeQualifierList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeDirectDeclarator_Rec(char **str, struct Node **ast,
                               struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  char *start = *str;
  if (TakeString(str, "[")) {
    char *start = *str;
    if (TakeConditionalExpression(str, node + 1) && TakeString(str, "]")) {
      char *start = *str;
      if (TakeDirectDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], node[1]), ARRAY))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "]")) {
      char *start = *str;
      if (TakeDirectDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], NULL), ARRAY))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "(")) {
    char *start = *str;
    if (TakeParameterTypeList(str, node + 1) && TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], node[1]), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeIdentifierList(str, node + 1) && TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], node[1]), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], NULL), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeDirectDeclarator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeString(str, "(") && TakeDeclarator(str, node + 0) &&
      TakeString(str, ")")) {
    char *start = *str;
    if (TakeDirectDeclarator_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeIdentifier(str, node + 0)) {
    char *start = *str;
    if (TakeDirectDeclarator_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeConditionalExpression(char **str, struct Node **ast) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  if (TakeLogicalOrExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "?") && TakeExpression(str, node + 1) &&
        TakeString(str, ":") && TakeConditionalExpression(str, node + 2)) {
      *ast = FuncApp(Symbol(sym("_Ternary")),
                     List(node[0], List(node[1], List(node[2], NULL))));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeLogicalOrExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeLogicalAndExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "||") && TakeLogicalOrExpression(str, node + 1)) {
      *ast = FuncApp(Symbol(sym("_Or")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeLogicalAndExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeInclusiveOrExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "&&") && TakeLogicalAndExpression(str, node + 1)) {
      *ast =
          FuncApp(Symbol(sym("_BitAnd")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeInclusiveOrExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeExclusiveOrExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "|") && TakeInclusiveOrExpression(str, node + 1)) {
      *ast = FuncApp(Symbol(sym("_BitOr")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeExclusiveOrExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeAndExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "^") && TakeExclusiveOrExpression(str, node + 1)) {
      *ast = FuncApp(Symbol(sym("_Xor")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeAndExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeEqualityExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "&") && TakeAndExpression(str, node + 1)) {
      *ast =
          FuncApp(Symbol(sym("_BitAnd")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeEqualityExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeRelationalExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "==") && TakeEqualityExpression(str, node + 1)) {
      *ast = FuncApp(Symbol(sym("_EQ")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "!=") && TakeEqualityExpression(str, node + 1)) {
      *ast = FuncApp(Symbol(sym("_NEQ")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeRelationalExpression_Rec(char **str, struct Node **ast,
                                   struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  char *start = *str;
  if (TakeString(str, "<") && TakeShiftExpression(str, node + 1)) {
    char *start = *str;
    if (TakeRelationalExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_LT")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, ">") && TakeShiftExpression(str, node + 1)) {
    char *start = *str;
    if (TakeRelationalExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_GT")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "<=") && TakeShiftExpression(str, node + 1)) {
    char *start = *str;
    if (TakeRelationalExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_LTE")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, ">=") && TakeShiftExpression(str, node + 1)) {
    char *start = *str;
    if (TakeRelationalExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_GTE")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeRelationalExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeShiftExpression(str, node + 0)) {
    char *start = *str;
    if (TakeRelationalExpression_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeShiftExpression_Rec(char **str, struct Node **ast,
                              struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  char *start = *str;
  if (TakeString(str, ">>") && TakeAdditiveExpression(str, node + 1)) {
    char *start = *str;
    if (TakeShiftExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_RShift")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "<<") && TakeAdditiveExpression(str, node + 1)) {
    char *start = *str;
    if (TakeShiftExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_LShift")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeShiftExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeAdditiveExpression(str, node + 0)) {
    char *start = *str;
    if (TakeShiftExpression_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeAdditiveExpression_Rec(char **str, struct Node **ast,
                                 struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  if (TakeString(str, "-") && TakeMultiplicativeExpression(str, node + 1)) {
    char *start = *str;
    if (TakeAdditiveExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_Sub")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeAdditiveExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeMultiplicativeExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "+") && TakeAdditiveExpression(str, node + 1)) {
      char *start = *str;
      if (TakeAdditiveExpression_Rec(
              str, ast,
              FuncApp(Symbol(sym("_Add")),
                      List(node[0], List(node[1], NULL))))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = FuncApp(Symbol(sym("_Add")), List(node[0], List(node[1], NULL)));
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeAdditiveExpression_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeMultiplicativeExpression_Rec(char **str, struct Node **ast,
                                       struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  char *start = *str;
  if (TakeString(str, "/") && TakeCastExpression(str, node + 1)) {
    char *start = *str;
    if (TakeMultiplicativeExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_Div")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "%") && TakeCastExpression(str, node + 1)) {
    char *start = *str;
    if (TakeMultiplicativeExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_Mod")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeMultiplicativeExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeCastExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "*") && TakeMultiplicativeExpression(str, node + 1)) {
      char *start = *str;
      if (TakeMultiplicativeExpression_Rec(
              str, ast,
              FuncApp(Symbol(sym("_Mul")),
                      List(node[0], List(node[1], NULL))))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = FuncApp(Symbol(sym("_Mul")), List(node[0], List(node[1], NULL)));
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeMultiplicativeExpression_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeCastExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeString(str, "(") && TakeTypeName(str, node + 0) &&
      TakeString(str, ")") && TakeCastExpression(str, node + 1)) {
    *ast = FuncApp(Symbol(sym("_Cast")), List(node[0], List(node[1], NULL)));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeUnaryExpression(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeTypeName(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeSpecifierQualifierList(str, node + 0)) {
    char *start = *str;
    if (TakeAbstractDeclarator(str, node + 1)) {
      *ast = Type(Specifier(node[0]), node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = Type(Specifier(node[0]), NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeAbstractDeclarator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakePointer(str, node + 0)) {
    char *start = *str;
    if (TakeDirectAbstractDeclarator(str, node + 1)) {
      *ast = List(node[0], List(node[1], NULL));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeDirectAbstractDeclarator(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeDirectAbstractDeclarator_Rec(char **str, struct Node **ast,
                                       struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  char *start = *str;
  if (TakeString(str, "[")) {
    char *start = *str;
    if (TakeConditionalExpression(str, node + 1) && TakeString(str, "]")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], node[1]), ARRAY))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "]")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], NULL), ARRAY))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "(")) {
    char *start = *str;
    if (TakeParameterTypeList(str, node + 1) && TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], node[1]), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, node[0] = SetType(List(node[0], NULL), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeDirectAbstractDeclarator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeString(str, "(")) {
    char *start = *str;
    if (TakeAbstractDeclarator(str, node + 0) && TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(str, ast, node[0])) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeParameterTypeList(str, node + 0) && TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, SetType(List(NULL, node[0]), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = SetType(List(NULL, node[0]), FUNCTION);
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, ")")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, SetType(List(NULL, NULL), FUNCTION))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 0);
      if (1) {
        *ast = SetType(List(NULL, NULL), FUNCTION);
        return start_;
      }
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "[")) {
    char *start = *str;
    if (TakeConditionalExpression(str, node + 0) && TakeString(str, "]")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(
              str, ast, SetType(List(NULL, node[0]), ARRAY))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = SetType(List(NULL, node[0]), ARRAY);
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, "]")) {
      char *start = *str;
      if (TakeDirectAbstractDeclarator_Rec(str, ast,
                                           SetType(List(NULL, NULL), ARRAY))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 0);
      if (1) {
        *ast = SetType(List(NULL, NULL), ARRAY);
        return start_;
      }
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeParameterTypeList(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  if (TakeParameterList(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeString(str, "...")) {
      *ast = Node(VAR_ARG, 1, node);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeParameterList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeParameterDeclaration(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeParameterList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeParameterDeclaration(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeDeclarationSpecifiers(str, node + 0)) {
    char *start = *str;
    if (TakeDeclarator(str, node + 1)) {
      *ast = Type(Specifier(node[0]), node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeAbstractDeclarator(str, node + 1)) {
      *ast = Type(Specifier(node[0]), node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = Type(Specifier(node[0]), NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeUnaryExpression(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeString(str, "++") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_PreIncrament")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "--") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_PreDecrament")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "&") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_Address")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "*") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_Deref")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "+") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_UPlus")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "-") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_UMinus")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "~") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_BitNot")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "!") && TakeUnaryExpression(str, node + 0)) {
    *ast = FuncApp(Symbol(sym("_Not")), List(node[0], NULL));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "sizeof")) {
    char *start = *str;
    if (TakeString(str, "(") && TakeTypeName(str, node + 0) &&
        TakeString(str, ")")) {
      *ast = FuncApp(Symbol(sym("_Sizeof")), List(node[0], NULL));
      return start_;
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeUnaryExpression(str, node + 0)) {
      *ast = FuncApp(Symbol(sym("_Sizeof")), List(node[0], NULL));
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakePostfixExpression(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakePostfixExpression_Rec(char **str, struct Node **ast,
                                struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  char *start = *str;
  if (TakeString(str, "[") && TakeExpression(str, node + 1) &&
      TakeString(str, "]")) {
    char *start = *str;
    if (TakePostfixExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_Index")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "(")) {
    char *start = *str;
    if (TakeArgumentExpressionList(str, node + 1) && TakeString(str, ")")) {
      char *start = *str;
      if (TakePostfixExpression_Rec(str, ast,
                                    node[0] = FuncApp(node[0], node[1]))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 2);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, ")")) {
      char *start = *str;
      if (TakePostfixExpression_Rec(str, ast,
                                    node[0] = FuncApp(node[0], NULL))) {
        ;
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = node[0];
        return start_;
      }
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, ".") && TakeIdentifier(str, node + 1)) {
    char *start = *str;
    if (TakePostfixExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_Member")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "->") && TakeIdentifier(str, node + 1)) {
    char *start = *str;
    if (TakePostfixExpression_Rec(
            str, ast,
            node[0] = FuncApp(Symbol(sym("_DerefMember")),
                              List(node[0], List(node[1], NULL))))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "++")) {
    char *start = *str;
    if (TakePostfixExpression_Rec(str, ast,
                                  node[0] = FuncApp(Symbol(sym("_Incrament")),
                                                    List(node[0], NULL)))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 1);
  if (TakeString(str, "--")) {
    char *start = *str;
    if (TakePostfixExpression_Rec(str, ast,
                                  node[0] = FuncApp(Symbol(sym("_Decrament")),
                                                    List(node[0], NULL)))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakePostfixExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakePrimaryExpression(str, node + 0)) {
    char *start = *str;
    if (TakePostfixExpression_Rec(str, ast, node[0])) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakePrimaryExpression(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeString(str, "(") && TakeExpression(str, node + 0) &&
      TakeString(str, ")")) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeIdentifier(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeIntegerConstant(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeFloatingConstant(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeCharacterConstant(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeStringLiteral(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeAssignmentExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeExpression(str, node + 1)) {
      *ast = FuncApp(Symbol(sym("_Comma")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeAssignmentExpression(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeUnaryExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "=") && TakeAssignmentExpression(str, node + 1)) {
      *ast =
          FuncApp(Symbol(sym("_Assign")), List(node[0], List(node[1], NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "*=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_Mul")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "/=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_Div")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "%=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_Mod")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "+=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_Add")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "-=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_Sub")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "<<=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_LShift")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, ">>=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_RShift")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "&=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_BitAnd")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "^=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_Xor")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "|=") && TakeAssignmentExpression(str, node + 1)) {
      *ast = FuncApp(
          Symbol(sym("_Assign")),
          List(node[0], List(FuncApp(Symbol(sym("_BitOr")),
                                     List(node[0], List(node[1], NULL))),
                             NULL)));
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeConditionalExpression(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeIntegerConstant(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeString(str, "integer_constant")) {
    *ast = Node(INT_CONST, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeFloatingConstant(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeString(str, "floating_constant")) {
    *ast = Node(FLOAT_CONST, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeCharacterConstant(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeString(str, "character_constant")) {
    *ast = Node(CHAR_CONST, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStringLiteral(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeString(str, "string_literal")) {
    *ast = Node(STRING_CONST, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeArgumentExpressionList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeAssignmentExpression(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeArgumentExpressionList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeIdentifierList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeIdentifier(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeIdentifierList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeUnionSpecifier(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeString(str, "union")) {
    char *start = *str;
    if (TakeIdentifier(str, node + 0)) {
      char *start = *str;
      if (TakeString(str, "{") && TakeStructDeclarationList(str, node + 1) &&
          TakeString(str, "}")) {
        *ast = TypeDef(node[0], Node(UNION, 1, node + 1));
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = SetType(node[0], TYPEREF);
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, "{") && TakeStructDeclarationList(str, node + 0) &&
        TakeString(str, "}")) {
      *ast = Node(UNION, 1, node);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeEnumSpecifier(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeString(str, "enum")) {
    char *start = *str;
    if (TakeIdentifier(str, node + 0)) {
      char *start = *str;
      if (TakeString(str, "{") && TakeEnumeratorList(str, node + 1) &&
          TakeString(str, "}")) {
        *ast = TypeDef(node[0], Node(ENUM, 1, node + 1));
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (1) {
        *ast = SetType(node[0], TYPEREF);
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, "{") && TakeEnumeratorList(str, node + 0) &&
        TakeString(str, "}")) {
      *ast = Node(ENUM, 1, node);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeEnumeratorList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeEnumerator(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeEnumeratorList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeEnumerator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeIdentifier(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "=") && TakeConditionalExpression(str, node + 1)) {
      *ast = Node(ENUM_CONST, 2, node);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = Node(ENUM_CONST, 2, node);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeTypedefName(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeString(str, "typedef_name")) {
    *ast = Node(TYPEDEF_NAME, 0, NULL);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeDeclarationList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeDeclaration(str, node + 0)) {
    char *start = *str;
    if (TakeDeclarationList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeDeclaration(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeDeclarationSpecifiers(str, node + 0)) {
    char *start = *str;
    if (TakeInitDeclaratorList(str, node + 1) && TakeString(str, ";")) {
      *ast = TypeList(Specifier(node[0]), node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, ";")) {
      *ast = Type(Specifier(node[0]), NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeInitDeclaratorList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeInitDeclarator(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeInitDeclaratorList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeInitDeclarator(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeDeclarator(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, "=") && TakeInitializer(str, node + 1)) {
      *ast = Node(DECLARATOR, 2, node);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeInitializer(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeString(str, "{") && TakeInitializerList(str, node + 0)) {
    char *start = *str;
    if (TakeString(str, ",") && TakeString(str, "}")) {
      *ast = node[0];
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (TakeString(str, "}")) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeAssignmentExpression(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeInitializerList_Rec(char **str, struct Node **ast,
                              struct Node *prev) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  node[0] = prev;
  if (TakeString(str, ",") && TakeInitializer(str, node + 1)) {
    char *start = *str;
    if (TakeInitializerList_Rec(str, ast, node[0] = List(node[0], node[1]))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = node[0];
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node + 1);
  *ast = node[0];
  return *str;
}
char *TakeInitializerList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeInitializer(str, node + 0)) {
    char *start = *str;
    if (TakeInitializerList_Rec(str, ast, List(node[0], NULL))) {
      ;
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeCompoundStatement(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeString(str, "{")) {
    char *start = *str;
    if (TakeDeclarationList(str, node + 0)) {
      char *start = *str;
      if (TakeStatementList(str, node + 1) && TakeString(str, "}")) {
        *ast = Block(Concat(Flatten(node[0]), node[1]));
        return start_;
      }
      *str = start;
      FreeNodes(node + 1);
      if (TakeString(str, "}")) {
        *ast = Block(Flatten(node[0]));
        return start_;
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeStatementList(str, node + 0) && TakeString(str, "}")) {
      *ast = Block(node[0]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, "}")) {
      *ast = Block(NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStatementList(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  if (TakeStatement(str, node + 0)) {
    char *start = *str;
    if (TakeStatementList(str, node + 1)) {
      *ast = List(node[0], node[1]);
      return start_;
    }
    *str = start;
    FreeNodes(node + 1);
    if (1) {
      *ast = List(node[0], NULL);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeStatement(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeLabeledStatement(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeCompoundStatement(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeExpressionStatement(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeSelectionStatement(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeIterationStatement(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeJumpStatement(str, node + 0)) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeLabeledStatement(char **str, struct Node **ast) {
  struct Node *node[3];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (3));
  char *start = *str;
  if (TakeIdentifier(str, node + 0) && TakeString(str, ":") &&
      TakeStatement(str, node + 1)) {
    *ast = Node(LABEL, 2, node);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "case") && TakeConditionalExpression(str, node + 0) &&
      TakeString(str, ":") && TakeStatement(str, node + 1)) {
    *ast = Node(CASE, 2, node);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "default") && TakeString(str, ":") &&
      TakeStatement(str, node + 0)) {
    *ast = Node(DEFAULT, 2, node);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeExpressionStatement(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeExpression(str, node + 0) && TakeString(str, ";")) {
    *ast = node[0];
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, ";")) {
    *ast = node[0];
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeSelectionStatement(char **str, struct Node **ast) {
  struct Node *node[4];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (4));
  char *start = *str;
  if (TakeString(str, "if") && TakeString(str, "(") &&
      TakeExpression(str, node + 0) && TakeString(str, ")") &&
      TakeStatement(str, node + 1)) {
    char *start = *str;
    if (TakeString(str, "else") && TakeStatement(str, node + 2)) {
      *ast = FuncApp(Symbol(sym("_Ternary")),
                     List(node[0], List(node[1], List(node[2], NULL))));
      return start_;
    }
    *str = start;
    FreeNodes(node + 2);
    if (1) {
      *ast = FuncApp(Symbol(sym("_Ternary")),
                     List(node[0], List(node[1], List(NULL, NULL))));
      return start_;
    }
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "switch") && TakeString(str, "(") &&
      TakeExpression(str, node + 0) && TakeString(str, ")") &&
      TakeStatement(str, node + 1)) {
    *ast = Node(SWITCH, 2, node);
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeIterationStatement(char **str, struct Node **ast) {
  struct Node *node[5];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (5));
  char *start = *str;
  if (TakeString(str, "while") && TakeString(str, "(") &&
      TakeExpression(str, node + 0) && TakeString(str, ")") &&
      TakeStatement(str, node + 1)) {
    *ast = FuncApp(Symbol(sym("_While")), List(node[0], List(node[1], NULL)));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "do") && TakeStatement(str, node + 0) &&
      TakeString(str, "while") && TakeString(str, "(") &&
      TakeExpression(str, node + 1) && TakeString(str, ")") &&
      TakeString(str, ";")) {
    *ast = FuncApp(Symbol(sym("_DoWhile")), List(node[0], List(node[1], NULL)));
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "for") && TakeString(str, "(")) {
    char *start = *str;
    if (TakeExpression(str, node + 0) && TakeString(str, ";")) {
      char *start = *str;
      if (TakeExpression(str, node + 1) && TakeString(str, ";")) {
        char *start = *str;
        if (TakeExpression(str, node + 2) && TakeString(str, ")") &&
            TakeStatement(str, node + 3)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(node[0], List(node[1], List(node[2], List(node[3], NULL)))));
          return start_;
        }
        *str = start;
        FreeNodes(node + 2);
        if (TakeString(str, ")") && TakeStatement(str, node + 2)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(node[0], List(node[1], List(NULL, List(node[2], NULL)))));
          return start_;
        }
      }
      *str = start;
      FreeNodes(node + 1);
      if (TakeString(str, ";")) {
        char *start = *str;
        if (TakeExpression(str, node + 1) && TakeString(str, ")") &&
            TakeStatement(str, node + 2)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(node[0], List(NULL, List(node[1], List(node[2], NULL)))));
          return start_;
        }
        *str = start;
        FreeNodes(node + 1);
        if (TakeString(str, ")") && TakeStatement(str, node + 1)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(node[0], List(NULL, List(NULL, List(node[1], NULL)))));
          return start_;
        }
      }
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, ";")) {
      char *start = *str;
      if (TakeExpression(str, node + 0) && TakeString(str, ";")) {
        char *start = *str;
        if (TakeExpression(str, node + 1) && TakeString(str, ")") &&
            TakeStatement(str, node + 2)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(NULL, List(node[0], List(node[1], List(node[2], NULL)))));
          return start_;
        }
        *str = start;
        FreeNodes(node + 1);
        if (TakeString(str, ")") && TakeStatement(str, node + 1)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(NULL, List(node[0], List(NULL, List(node[1], NULL)))));
          return start_;
        }
      }
      *str = start;
      FreeNodes(node + 0);
      if (TakeString(str, ";")) {
        char *start = *str;
        if (TakeExpression(str, node + 0) && TakeString(str, ")") &&
            TakeStatement(str, node + 1)) {
          *ast = FuncApp(
              Symbol(sym("_For")),
              List(NULL, List(NULL, List(node[0], List(node[1], NULL)))));
          return start_;
        }
        *str = start;
        FreeNodes(node + 0);
        if (TakeString(str, ")") && TakeStatement(str, node + 0)) {
          *ast =
              FuncApp(Symbol(sym("_For")),
                      List(NULL, List(NULL, List(NULL, List(node[0], NULL)))));
          return start_;
        }
      }
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}

char *TakeJumpStatement(char **str, struct Node **ast) {
  struct Node *node[2];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (2));
  char *start = *str;
  if (TakeString(str, "goto") && TakeIdentifier(str, node + 0) &&
      TakeString(str, ";")) {
    *ast = Node(GOTO, 1, node);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "continue") && TakeString(str, ";")) {
    *ast = Node(CONTINUE, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "break") && TakeString(str, ";")) {
    *ast = Node(BREAK, 0, NULL);
    return start_;
  }
  *str = start;
  FreeNodes(node + 0);
  if (TakeString(str, "return")) {
    char *start = *str;
    if (TakeExpression(str, node + 0) && TakeString(str, ";")) {
      *ast = Node(RETURN, 1, node);
      return start_;
    }
    *str = start;
    FreeNodes(node + 0);
    if (TakeString(str, ";")) {
      *ast = Node(RETURN, 1, node);
      return start_;
    }
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}
