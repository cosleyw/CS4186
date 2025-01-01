char *TakeIdentifier(char **, struct Node **);
char *TakeIntegerConstant(char **, struct Node **);
char *TakeFloatingConstant(char **, struct Node **);
char *TakeCharacterConstant(char **, struct Node **);
char *TakeStringLiteral(char **, struct Node **);

char *TakeIdentifier(char **str, struct Node **ast) {
  struct Node *node[1];
  char *start_ = *str;
  memset(node, 0, sizeof(struct Node *) * (1));
  if (TakeWhiteSpace(str), TakeIdentifier_(str)) {
    *ast = Symbol(get_sym(TakeIdentifier_, start_));
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
  if (TakeWhiteSpace(str), TakeIntegerConstant_(str)) {
    *ast = TypeInit(Symbol(sym("int")), Symbol(get_sym(TakeIntegerConstant_, start_)));
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
  if (TakeWhiteSpace(str), TakeFloatingConstant_(str)) {
    *ast = TypeInit(Symbol(sym("float")), Symbol(get_sym(TakeFloatingConstant_, start_)));
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
  if (TakeWhiteSpace(str), TakeCharacterConstant_(str)) {
    *ast = TypeInit(Symbol(sym("char")), Symbol(get_sym(TakeCharacterConstant_, start_)));
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
  if (TakeWhiteSpace(str), TakeStringLiteral_(str)) {
    *ast = TypeInit(
		    SetType(List(Symbol(sym("char")), NULL), POINTER),
		    Symbol(get_sym(TakeStringLiteral_, start_)));
    return start_;
  }
  *str = start_;
  FreeNodes(node);
  return NULL;
}


