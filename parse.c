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
	int tokenStringIndex = 0;//tokenStringIndex를 0으로 초기화 
	TokenType currentToken;//TokenType 구조체 타입의 변수 currentToken 선언
	StateType state = START;//StateType 구조체 타입의 변수 state를 선언 후 START로 초기화
	int save;

	while (state != DONE)
	{
		char c = getNextChar();//inputbuffer에 있는 character 하나를 받아옴
		save = TRUE;//save를 TRUE로 저장, save는 character를 tokenString에 저장할 지 말지를 결정한다.
		switch (state)
		{
		case START://state가 START일 때
			if (isdigit(c))
				state = INNUM;//숫자가 오면 INNUM state로 간다.
			else if (isalpha(c))
				state = INID;//문자가 오면 INID state로 간다.
			else if (c == '=')
				state = INASSIGN;//'='이 오면 INASSIGN state로 간다.
			else if (c == '!')
				state = INDIFF;//'!'이 오면 INDIFF state로 간다.
			else if (c == '<')
				state = INLT;//'<'이 오면 INLT state로 간다.
			else if (c == '>')
				state = INGT;//'>'이 오면 INGT state로 간다.
			else if (c == '/') {
				save = FALSE;
				state = ISCOMMENT;
			}//'/'이 오면 ISCOMMENT state로 간다. 주석일 수도 있기 때문에 일단 tokenString에 저장하지 않는다.
			else if ((c == ' ') || (c == '\t') || (c == '\n'))
				save = FALSE;//공백이 나오면 저장하지 않고 state는 여전히 START이다. 즉 무시한다.
			else
			{//그 외에 나머지가 나오면 DONE state(final state)로 간다.
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
		case ISCOMMENT://state가 ISCOMMENT인 경우(COMMENT가 될 수 있는지 판단)
			state = DONE;
			if (c == '*') { save = FALSE;state = INCOMMENT1; }//character가 '*'이면 tokenString에 저장하지 않고 주석 시작인 INCOMMENT1 state로 간다.
			else {//character가 '*'이 아니면 tokenString에 저장하지 않고 DONE state로 간다. inputbuffer의 linepos를 원래대로 돌려놓고 현재 token을 OVER로 설정한다.
				ungetNextChar();
				save = FALSE;
				currentToken = OVER;
			}
			break;
		case INCOMMENT1://현재 INCOMMENT1 state경우
			save = FALSE;//token String에 저장하지 않고
			if (c == '*')state = INCOMMENT2;//현재 character가 '*'이면 INCOMMENT2 string으로 간다.
			else {//현재 character가 '*'이 아니면 계속 INCOMMENT1에 머문다.
				state = INCOMMENT1;
			}
			break;
		case INCOMMENT2://현재 state가 INCOMMENT2인 경우 
			save = FALSE;//tokenString에 저장하지 안고 
			if (c == '/')state = START;//현재 character가 '/'이면 주석이 끝났으므로 다시 START 상태로 넘어간다. 
			else if (c == '*')state = INCOMMENT2;// 현재 character가 '*'이면 INCOMMENT2state에 머문다.
			else if (c == ' ') {//현재 character가 ' '이면 오류이기 때문에 "stop before ending" 오류메세지를 출력한다. 
				fprintf(listing, "stop before ending\n");
			}
			else//나며지 문자에 대해서는 다시 INCOMMENT1 string으로 넘어간다.
				state = INCOMMENT1;
			break;
		case INASSIGN://현재 상태가 INASSIGN 상태인 경우('='을 받은 상태) 
			state = DONE;//DONE상태로 넘어간다
			if (c == '=')//현재 상태가 '='이면 '==' 을 출력하도록 currentToken 을 SAME으고 정한다.
				currentToken = SAME;
			else
			{//현재 상태가 '='이 아니면 '='을 출력하도록 currentToken을 ASSIGN으로 하고 받았던 character를 돌려준다.
				ungetNextChar();
				save = FALSE;
				currentToken = ASSIGN;
			}
			break;
		case INDIFF://'!'을 받은 상태인 경우
			state = DONE;
			if (c == '=')//현재 '='을 받았으면 
				currentToken = DIFF;//'!='을 출력해야 하므로 현재 token이 DIFF이다. 최종적으로 tokenString에는 '!='이 저장되어있다.
			else
			{//만약 다른 charater를 받았으면 ERROR를 출력해야 하므로 받은 문자를 돌려주고 currentToken에 ERROR를 대입한다. 
				ungetNextChar();
				save = FALSE;
				currentToken = ERROR;
			}
			break;
		case INLT://전에 '<'를 받은 경우 
			state = DONE;
			if (c == '=')//현재 '='를 받으면
				currentToken = SLT;//'<='를 출력해야 하므로 currentToken에 SLT를 대입한다. 최종적으로 tokenString에는 '<=' 이 저장되어있다. 
			else
			{//다른 character를 받으면 '<'만 출력해야 하므로 받은 문자를 inputbuffer에 돌려주고 currentToken에 LT를 대입한다.
				ungetNextChar();
				save = FALSE;
				currentToken = LT;
			}
			break;
		case INGT://전에 '>'를 받은 경우
			state = DONE;
			if (c == '=')//현재 '='를 받으면
				currentToken = SGT;//'>='를 출력해야 하므로 currenToken 에 SGT를 대입한다. 최종적으로 tokenString에는 '>='이 저장되어있다.
			else
			{//다른 character를 받으면 '>'만 출력해야 하므로 받은 문자를 inputbuffer에 돌려주고 currnetToken에 GT를 대입한다.
				ungetNextChar();
				save = FALSE;
				currentToken = GT;
			}
			break;
		case INNUM://전에 숫자(digit)을 받은 경우
			if (!isdigit(c)) {//다음 문자가 digit이면 digit이 아닌 값이 나올 때까지 tokenString에 저장하고 INNUM 상태에 머무른다.
				if (isalpha(c)) {//다음 문자가 alphabet이면 INNUM_ERR상태로 넘어간다.
					state = INNUM_ERR;
				}
				else {//다음 문자가 digit도 아니고 alphabet도 아닌
					ungetNextChar();//받은 character를 inputbuffer에 돌려준다.
					save = FALSE;//tokenString에 저장하지 않는다.
					state = DONE;//DONE상태로 넘어간다.
					currentToken = NUM;//digit을 출력해야 하므로 currentToken에 NUM을 대입한다. 
				}
			}break;
		case INNUM_ERR://digit다음에 바로 alphabet이 온 경우
			if (isalpha(c)) {//또 alphabet이 나오면 
				state = INNUM_ERR;//계속 INNUM_ERR상태에 머무른다.
			}
			else {//digit다음에 alphabet이 아닌 다른 값이 올 경우
				save = FALSE;//tokenString에 저장하지 않는다.
				currentToken = ERROR;//error를 출력해야 하므로 currenToken 에 ERROR를 대입한다.
				state = DONE;
				ungetNextChar();//받은 문자를 돌려준다.
			}break;

		case INID://전에 letter를 받은 경우
			if (!isalpha(c))
			{//letter가 아닌 경우
				ungetNextChar();//inputbuffer에서 받은 문자를 다시 돌려놓는다.
				save = FALSE;//tokenString에 저장하진 않는다.
				state = DONE;//DONE 상태로 넘어간다.
				currentToken = ID;//currentToken 에 ID를 대입한다.
			}break;
		case DONE:
		default:
			fprintf(listing, "Scanner Bug: state = %d\n", state);//잘못된 상태가 들어온 경우 에러 문장을 출력한다.
			state = DONE;
			currentToken = ERROR;
			break;
		}
		if ((save) && (tokenStringIndex <= MAXTOKENLEN))//save가 true이고 tokenStringdml 인덱스가 최대 token 길이보다 작거나 같으면 
			tokenString[tokenStringIndex++] = c;//tokenString에 현재 input buffer에서 받은 문자를 저장한다.
		if (state == DONE)//state가 DONE(FINAL)인 경우
		{
			tokenString[tokenStringIndex] = '\0';//printToken에서 String을 출력해야 하므로 tokenString의 끝에 '\0'를 대입한다.
			if (currentToken == ID)//currentToken 이 ID인 경우 
				currentToken = reservedLookUp(tokenString);//currentToken이 예약어인지 아닌지 판단하고 맞으면 예약어에 해당하는 token을 받는다.
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
		if (!strcmp(s, reservedWords[i].str))//strcmp:문자열이 같으면 0을 반환
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
