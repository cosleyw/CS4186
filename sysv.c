
/* x86_64-abi-0.99 */

enum SysvClass {
	Sv_INTEGER,
	Sv_SSE,
	Sv_MEMORY,
	Sv_NO_CLASS
}

enum RegisterID {
	R_RDI, //1st arg
	R_RSI, //2nd arg
	R_RDX, //3rd arg / second return register
	R_RCX, //4th arg
	R_R8,  //5th arg
	R_R9,  //6th arg
	R_R10,
	R_R11,
	R_R12, //callee saved
	R_R13, //callee saved
	R_R14, //callee saved
	R_R15, //callee saved
	R_RBX, //callee saved
	R_RSP, //callee saved
	R_RBP, //callee saved
	R_RAX, //first return register
};

struct Register {
	enum RegisterID reg;	
};

struct Memory {
	size_t offset, size;
};

enum SysvClass SvClassify(struct Node **ref, struct Node *node){
	size_t size;

	if(TypeOf(node) == ref[MY_INT])
		return Sv_INTEGER;

	if(TypeOf(node) == ref[MY_FLOAT])
		return Sv_SSE;

	if(TypeOf(node) == ref[MY_DOUBLE])
		return Sv_SSE;

	size = SizeOf(ref, node);
	if(size & 0x7) /* round to next 8 byte */
		size = (size & ~0x7) + 1;

	if(size > 8)
		return Sv_MEMORY;

	/* TODO struct/union */
}

void SvInitParam(struct Node **ref, struct Node *param){
	struct Node *cur, *nd;
	enum SysvClass class;

	cur = param;
	while(cur){
		class = SvClassify(cur->node[0])

		switch(class){
			case Sv_MEMORY:
				nd = Node(MEMORY, 0, NULL); 
				nd->item = malloc(sizeof(struct Memory));
				/* TODO */
				*((struct Memory*)nd->item) = (struct Memory){
					
				}
				break;
			case Sv_INTEGER:
				nd = Node(REGISTER, 0, NULL); 
				nd->item = malloc(sizeof(struct Register));
				/* TODO */
				*((struct Register*)nd->item) = (struct Register){
					
				}
				break;
			case Sv_SSE:
				nd = Node(MEMORY, 0, NULL); 
				nd->item = malloc(sizeof(struct Register));
				/* TODO */
				*((struct Register*)nd->item) = (struct Register){
					
				}
				break;
		}

		cur = param->node[1];
	}
} 

