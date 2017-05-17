#define TRUE 1
#define FALSE 0

#define BUFLEN 256
#define MAXRESERVED 6 //the number of reserved words
#define MAXTOKENLEN 60 //the maximum size of a token
#define MAXCHILDREN 3

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef enum { START, INASSIGN, INDIFF, INLT, INGT, ISCOMMENT, INCOMMENT1, INCOMMENT2, INNUM, INNUM_ERR, INID, DONE }StateType;
typedef enum {
	ENDFILE, ERROR,
	ELSE, IF, INT, RETURN, VOID, WHILE,//reserved words
	PLUS, MINUS, TIMES, OVER, LT, SLT, GT, SGT, SAME, DIFF, ASSIGN, SEMI, COL, LPAREN, RPAREN, SLPAREN, SRPAREN, LB, RB, //special symbols(19)
	ID, NUM
}TokenType;

typedef enum { StmtK, ExpK } NodeKind;
typedef enum { IfK, WhileK, AssignK, ReadK, WriteK } StmtKind;
typedef enum { OpK, ConstK, Idk } ExpKind;
typedef enum { Void, Integer, Boolean } ExpType;

static struct {
	char *str;
	TokenType tok;
}reservedWords[MAXRESERVED] = { { "if",IF },{ "else",ELSE },{ "int",INT },{ "return",RETURN },{ "void",VOID },{ "while",WHILE } };

typedef struct treeNode
{
	struct treeNode *child[MAXCHILDREN];
	struct treeNode *sibling;
	int lineno;
	NodeKind nodekind;
	union { StmtKind stmt; ExpKind exp; }kind;
	union {
		TokenType op;
		int val;
		char *name;
	}attr;
	ExpType type;
}TreeNode;

TokenType getToken(void);

static TokenType token;

TreeNode * parse(void);
TreeNode * newStmtNode(StmtKind kind);
TreeNode * newExpNode(ExpKind kind);


static TreeNode * stat_sequence(void);
static TreeNode * statement(void);
static TreeNode * if_stmt(void);
static TreeNode * repeat_stmt(void);
static TreeNode * assign_stmt(void);
static TreeNode * read_stmt(void);
static TreeNode * write_stmt(void);
static TreeNode * exp(void);
static TreeNode * simple_exp(void);
static TreeNode * term(void);
static TreeNode * factor(void);

char * copyString(char *s);

void printToken(TokenType token, const char* tokenstring);
FILE * source;
FILE *listing;

int lineno = 0;
int TraceScan = TRUE;
int EchoSource = TRUE;

char tokenString[MAXTOKENLEN + 1];
static char linebuf[BUFLEN];
static int linepos = 0;
static int bufsize = 0;

static char getNextChar(void);
static void ungetNextChar(void);

static TokenType reservedLookUp(char * s);

int main(int argc, char *argv[])
{
	char source_name[20];

	if (argc != 3)
	{
		fprintf(stderr, "usage: %s <filename>\n", argv[0]);
		exit(1);
	}

	strcpy(source_name, argv[1]);
	if (strchr(source_name, '.') == NULL)
		strcat(source_name, ".c");

	source = fopen(source_name, "r");
	if (source == NULL)
	{
		exit(1);
	}

	listing = fopen(argv[2], "w");
	fprintf(listing, "\n C-  COMPILATION: %s\n", source_name);

	//while (getToken() != ENDFILE);
	parse();

	fclose(source);
	return 0;
}

void printToken(TokenType token, const char* tokenstring)
{
	switch (token)
	{
	case ELSE:
	case IF:
	case INT:
	case RETURN:
	case VOID:
	case WHILE:
		fprintf(listing, "reserved word: %s\n", tokenstring);break;
	case PLUS: fprintf(listing, "+\n");break;
	case MINUS:fprintf(listing, "-\n");break;
	case TIMES:fprintf(listing, "*\n");break;
	case OVER:fprintf(listing, "/\n");break;
	case LT: fprintf(listing, "<\n");break;
	case SLT: fprintf(listing, "<=\n");break;
	case GT:fprintf(listing, ">\n");break;
	case SGT:fprintf(listing, ">=\n");break;
	case SAME: fprintf(listing, "==\n");break;
	case DIFF: fprintf(listing, "!=\n");break;
	case ASSIGN: fprintf(listing, "=\n");break;
	case SEMI: fprintf(listing, ";\n");break;
	case COL:fprintf(listing, ",\n");break;
	case LPAREN: fprintf(listing, "(\n");break;
	case RPAREN: fprintf(listing, ")\n");break;
	case SLPAREN:fprintf(listing, "[\n");break;
	case SRPAREN:fprintf(listing, "]\n");break;
	case LB:fprintf(listing, "{\n");break;
	case RB:fprintf(listing, "}\n");break;
	case NUM:
		fprintf(listing, "NUM, val =%s\n", tokenstring);break;
	case ID:
		fprintf(listing, "ID, name=%s\n", tokenstring);break;
	case ERROR:
		fprintf(listing, "ERROR: %s\n", tokenstring);break;
	case ENDFILE:
		fprintf(listing, "EOF\n");break;
	default:
		fprintf(listing, "Unkown token: %d\n", token);break;
	}
}

TokenType getToken(void)
{
	int tokenStringIndex = 0;//tokenStringIndex�� 0���� �ʱ�ȭ 
	TokenType currentToken;//TokenType ����ü Ÿ���� ���� currentToken ����
	StateType state = START;//StateType ����ü Ÿ���� ���� state�� ���� �� START�� �ʱ�ȭ
	int save;

	while (state != DONE)
	{
		char c = getNextChar();//inputbuffer�� �ִ� character �ϳ��� �޾ƿ�
		save = TRUE;//save�� TRUE�� ����, save�� character�� tokenString�� ������ �� ������ �����Ѵ�.
		switch (state)
		{
		case START://state�� START�� ��
			if (isdigit(c))
				state = INNUM;//���ڰ� ���� INNUM state�� ����.
			else if (isalpha(c))
				state = INID;//���ڰ� ���� INID state�� ����.
			else if (c == '=')
				state = INASSIGN;//'='�� ���� INASSIGN state�� ����.
			else if (c == '!')
				state = INDIFF;//'!'�� ���� INDIFF state�� ����.
			else if (c == '<')
				state = INLT;//'<'�� ���� INLT state�� ����.
			else if (c == '>')
				state = INGT;//'>'�� ���� INGT state�� ����.
			else if (c == '/') {
				save = FALSE;
				state = ISCOMMENT;
			}//'/'�� ���� ISCOMMENT state�� ����. �ּ��� ���� �ֱ� ������ �ϴ� tokenString�� �������� �ʴ´�.
			else if ((c == ' ') || (c == '\t') || (c == '\n'))
				save = FALSE;//������ ������ �������� �ʰ� state�� ������ START�̴�. �� �����Ѵ�.
			else
			{//�� �ܿ� �������� ������ DONE state(final state)�� ����.
				state = DONE;
				switch (c)
				{
				case EOF:
					save = FALSE;
					currentToken = ENDFILE;
					break;
				case '+':
					currentToken = PLUS;
					break;
				case '-':
					currentToken = MINUS;
					break;
				case '*':
					currentToken = TIMES;
					break;

				case '(':
					currentToken = LPAREN;
					break;
				case ')':
					currentToken = RPAREN;
					break;
				case '{':
					currentToken = LB;
					break;
				case '}':
					currentToken = RB;
					break;
				case '[':
					currentToken = SLPAREN;
					break;
				case ']':
					currentToken = SRPAREN;
					break;
				case ';':
					currentToken = SEMI;
					break;
				case ',':
					currentToken = COL;
					break;
				default:
					currentToken = ERROR;
					break;
				}
			}
			break;
		case ISCOMMENT://state�� ISCOMMENT�� ���(COMMENT�� �� �� �ִ��� �Ǵ�)
			state = DONE;
			if (c == '*') { save = FALSE;state = INCOMMENT1; }//character�� '*'�̸� tokenString�� �������� �ʰ� �ּ� ������ INCOMMENT1 state�� ����.
			else {//character�� '*'�� �ƴϸ� tokenString�� �������� �ʰ� DONE state�� ����. inputbuffer�� linepos�� ������� �������� ���� token�� OVER�� �����Ѵ�.
				ungetNextChar();
				save = FALSE;
				currentToken = OVER;
			}
			break;
		case INCOMMENT1://���� INCOMMENT1 state���
			save = FALSE;//token String�� �������� �ʰ�
			if (c == '*')state = INCOMMENT2;//���� character�� '*'�̸� INCOMMENT2 string���� ����.
			else {//���� character�� '*'�� �ƴϸ� ��� INCOMMENT1�� �ӹ���.
				state = INCOMMENT1;
			}
			break;
		case INCOMMENT2://���� state�� INCOMMENT2�� ��� 
			save = FALSE;//tokenString�� �������� �Ȱ� 
			if (c == '/')state = START;//���� character�� '/'�̸� �ּ��� �������Ƿ� �ٽ� START ���·� �Ѿ��. 
			else if (c == '*')state = INCOMMENT2;// ���� character�� '*'�̸� INCOMMENT2state�� �ӹ���.
			else if (c == ' ') {//���� character�� ' '�̸� �����̱� ������ "stop before ending" �����޼����� ����Ѵ�. 
				fprintf(listing, "stop before ending\n");
			}
			else//������ ���ڿ� ���ؼ��� �ٽ� INCOMMENT1 string���� �Ѿ��.
				state = INCOMMENT1;
			break;
		case INASSIGN://���� ���°� INASSIGN ������ ���('='�� ���� ����) 
			state = DONE;//DONE���·� �Ѿ��
			if (c == '=')//���� ���°� '='�̸� '==' �� ����ϵ��� currentToken �� SAME���� ���Ѵ�.
				currentToken = SAME;
			else
			{//���� ���°� '='�� �ƴϸ� '='�� ����ϵ��� currentToken�� ASSIGN���� �ϰ� �޾Ҵ� character�� �����ش�.
				ungetNextChar();
				save = FALSE;
				currentToken = ASSIGN;
			}
			break;
		case INDIFF://'!'�� ���� ������ ���
			state = DONE;
			if (c == '=')//���� '='�� �޾����� 
				currentToken = DIFF;//'!='�� ����ؾ� �ϹǷ� ���� token�� DIFF�̴�. ���������� tokenString���� '!='�� ����Ǿ��ִ�.
			else
			{//���� �ٸ� charater�� �޾����� ERROR�� ����ؾ� �ϹǷ� ���� ���ڸ� �����ְ� currentToken�� ERROR�� �����Ѵ�. 
				ungetNextChar();
				save = FALSE;
				currentToken = ERROR;
			}
			break;
		case INLT://���� '<'�� ���� ��� 
			state = DONE;
			if (c == '=')//���� '='�� ������
				currentToken = SLT;//'<='�� ����ؾ� �ϹǷ� currentToken�� SLT�� �����Ѵ�. ���������� tokenString���� '<=' �� ����Ǿ��ִ�. 
			else
			{//�ٸ� character�� ������ '<'�� ����ؾ� �ϹǷ� ���� ���ڸ� inputbuffer�� �����ְ� currentToken�� LT�� �����Ѵ�.
				ungetNextChar();
				save = FALSE;
				currentToken = LT;
			}
			break;
		case INGT://���� '>'�� ���� ���
			state = DONE;
			if (c == '=')//���� '='�� ������
				currentToken = SGT;//'>='�� ����ؾ� �ϹǷ� currenToken �� SGT�� �����Ѵ�. ���������� tokenString���� '>='�� ����Ǿ��ִ�.
			else
			{//�ٸ� character�� ������ '>'�� ����ؾ� �ϹǷ� ���� ���ڸ� inputbuffer�� �����ְ� currnetToken�� GT�� �����Ѵ�.
				ungetNextChar();
				save = FALSE;
				currentToken = GT;
			}
			break;
		case INNUM://���� ����(digit)�� ���� ���
			if (!isdigit(c)) {//���� ���ڰ� digit�̸� digit�� �ƴ� ���� ���� ������ tokenString�� �����ϰ� INNUM ���¿� �ӹ�����.
				if (isalpha(c)) {//���� ���ڰ� alphabet�̸� INNUM_ERR���·� �Ѿ��.
					state = INNUM_ERR;
				}
				else {//���� ���ڰ� digit�� �ƴϰ� alphabet�� �ƴ�
					ungetNextChar();//���� character�� inputbuffer�� �����ش�.
					save = FALSE;//tokenString�� �������� �ʴ´�.
					state = DONE;//DONE���·� �Ѿ��.
					currentToken = NUM;//digit�� ����ؾ� �ϹǷ� currentToken�� NUM�� �����Ѵ�. 
				}
			}break;
		case INNUM_ERR://digit������ �ٷ� alphabet�� �� ���
			if (isalpha(c)) {//�� alphabet�� ������ 
				state = INNUM_ERR;//��� INNUM_ERR���¿� �ӹ�����.
			}
			else {//digit������ alphabet�� �ƴ� �ٸ� ���� �� ���
				save = FALSE;//tokenString�� �������� �ʴ´�.
				currentToken = ERROR;//error�� ����ؾ� �ϹǷ� currenToken �� ERROR�� �����Ѵ�.
				state = DONE;
				ungetNextChar();//���� ���ڸ� �����ش�.
			}break;

		case INID://���� letter�� ���� ���
			if (!isalpha(c))
			{//letter�� �ƴ� ���
				ungetNextChar();//inputbuffer���� ���� ���ڸ� �ٽ� �������´�.
				save = FALSE;//tokenString�� �������� �ʴ´�.
				state = DONE;//DONE ���·� �Ѿ��.
				currentToken = ID;//currentToken �� ID�� �����Ѵ�.
			}break;
		case DONE:
		default:
			fprintf(listing, "Scanner Bug: state = %d\n", state);//�߸��� ���°� ���� ��� ���� ������ ����Ѵ�.
			state = DONE;
			currentToken = ERROR;
			break;
		}
		if ((save) && (tokenStringIndex <= MAXTOKENLEN))//save�� true�̰� tokenStringdml �ε����� �ִ� token ���̺��� �۰ų� ������ 
			tokenString[tokenStringIndex++] = c;//tokenString�� ���� input buffer���� ���� ���ڸ� �����Ѵ�.
		if (state == DONE)//state�� DONE(FINAL)�� ���
		{
			tokenString[tokenStringIndex] = '\0';//printToken���� String�� ����ؾ� �ϹǷ� tokenString�� ���� '\0'�� �����Ѵ�.
			if (currentToken == ID)//currentToken �� ID�� ��� 
				currentToken = reservedLookUp(tokenString);//currentToken�� ��������� �ƴ��� �Ǵ��ϰ� ������ ���� �ش��ϴ� token�� �޴´�.
		}
	}
	if (TraceScan) {
		fprintf(listing, "\t%d:", lineno);
		printToken(currentToken, tokenString);
	}
	return currentToken;
}

static char getNextChar(void)
{
	if (!(linepos < bufsize))
	{
		lineno++;
		if (fgets(linebuf, BUFLEN - 1, source))
		{
			if (EchoSource)fprintf(listing, "%d:  %s", lineno, linebuf);
			bufsize = strlen(linebuf);
			linepos = 0;
			return linebuf[linepos++];
		}
		else return EOF;
	}
	else return linebuf[linepos++];
}

static void ungetNextChar(void)
{
	linepos--;
}

static TokenType reservedLookUp(char *s)
{
	int i;
	for (i = 0;i < MAXRESERVED;i++)
		if (!strcmp(s, reservedWords[i].str))//strcmp:���ڿ��� ������ 0�� ��ȯ
			return reservedWords[i].tok;
	return ID;
}

static void syntaxError(char * message)
{
	fprintf(listing, "\n>>> ");
	fprintf(listing, "Syntax error at line %d: %s", lineno, message);
	//Error = TRUE;
}

TreeNode * newStmtNode(StmtKind kind)
{
	TreeNode * t = (TreeNode *)malloc(sizeof(TreeNode));
	int i;
	if (t == NULL)
		fprintf(listing, "Out of memory error at line %d\n", lineno);
	else {
		for (i = 0;i < MAXCHILDREN;i++)t->child[i] = NULL;
		t->sibling = NULL;
		t->nodekind = StmtK;
		t->kind.stmt = kind;
		t->lineno = lineno;
	}
	return t;
}

TreeNode * newExpNode(ExpKind kind)
{
	TreeNode *t = (TreeNode *)malloc(sizeof(TreeNode));
	int i;
	if (t == NULL)
		fprintf(listing, "Out of memory error at line %d\n", lineno);
	else {
		for (i = 0;i < MAXCHILDREN;i++)t->child[i] = NULL;
		t->sibling = NULL;
		t->nodekind = StmtK;
		t->kind.stmt = kind;
		t->lineno = lineno;
	}
	return t;
}

static void match(TokenType expected)
{
	if (token == expected)token = getToken();
	else {
		syntaxError("unexpected token-> ");
		printToken(token, tokenString);
		fprintf(listing, "       ");
	}
}

static TreeNode * stmt_sequence(void)
{
	TreeNode *t = statement();
	TreeNode *p = t;
	while ((token != ENDFILE) && /*(token != END)*/(token != ELSE) /*(token != UNTIL)*/)
	{
		TreeNode *q;
		match(SEMI);
		q = statement();
		if (q != NULL) {
			if (t != NULL)t = p = q;
			else {
				p->sibling = q;
				p = q;
			}
		}
	}
	return t;
}

static TreeNode * statement(void)
{
	TreeNode *t = NULL;
	switch (token) {
	case IF: t = if_stmt();break;
	case WHILE: t = repeat_stmt();break;
	case ID: t = assign_stmt();break;
	//case WRITE: t = write_stmt();break;
	default: syntaxError("unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}

static TreeNode * if_stmt(void)
{
	TreeNode *t = newStmtNode(IfK);
	match(IF);
	if (t!=NULL) t->child[0] = exp();
	match(LPAREN);
	if (t != NULL)t->child[1] = stmt_sequence();
	if (token == ELSE) {
		match(ELSE);
		if (t!=NULL)t->child[2] = stmt_sequence();
	}
	match(ENDFILE);
	return t;
}

static TreeNode * repeat_stmt(void)
{
	TreeNode * t = newStmtNode(WhileK);
	match(WHILE);
	if (t != NULL)t->child[0] = stmt_sequence();
	//match(UNTIL);
	//if (t != NULL)t->child[1] = exp();
	return t;
}

static TreeNode * assign_stmt(void)
{
	TreeNode *t = newStmtNode(AssignK);
	if ((t != NULL) && (token == ID))
		t->attr.name = copyString(tokenString);
	match(ID);
	match(ASSIGN);
	if (t != NULL)t->child[0] = exp();
	return t;
}

static TreeNode * read_stmt(void)
{
	TreeNode *t = newStmtNode(ReadK);
	//match(READ);
	if ((t != NULL) && (token == ID))
		t->attr.name = copyString(tokenString);
	match(ID);
	return t;
}

static TreeNode * write_stmt(void)
{
	TreeNode * t = newStmtNode(WriteK);
	//match(WRITE);
	if (t != NULL)t->child[0] = exp();
	return t;
}

static TreeNode * exp(void)
{
	TreeNode * t = simple_exp();
	if ((token == LT) || (token == SAME)) {
		TreeNode *p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
		}
		match(token);
		if (t != NULL)
			t->child[1] = simple_exp();
	}
	return t;
}

static TreeNode * simple_exp(void)
{
	TreeNode *t = term();
	while ((token == PLUS) || (token == MINUS))
	{
		TreeNode * p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			t->child[1] = term();
		}
	}
	return t;
}

static TreeNode * term(void)
{
	TreeNode *t = factor();
	while ((token == TIMES) || (token == OVER))
	{
		TreeNode * p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			p->child[1] = factor();
		}
	}
	return t;
}

static TreeNode * factor(void)
{
	TreeNode *t = NULL;
	switch (token) {
	case NUM:
		t = newExpNode(ConstK);
		if ((t != NULL) && (token == NUM))
			t->attr.val = atoi(tokenString);
		match(NUM);
		break;
	case ID:
		t = newExpNode(Idk);
		if ((t != NULL) && (token == ID))
			t->attr.name = copyString(tokenString);
		match(ID);
		break;
	default:
		syntaxError("unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}

TreeNode * parse(void)
{
	TreeNode *t;
	token = getToken();
	t = stmt_sequence();
	if (token != ENDFILE)
		syntaxError("Code ends before file\n");
	return t;
}

char * copyString(char *s)
{
	int n;
	char *t;
	if (s == NULL)return NULL;
	n = strlen(s) + 1;
	t = malloc(n);
	if (t == NULL)
		fprintf(listing, "Out of memory error at line %d\n", lineno);
	else strcpy(t, s);
	return t;
}
