enum Kind {
	UNKNOWN,EOF,COMENTARIO,IDENTIFIER,NUMBER,CHARCONST
	//Palavras Reservadas
	,CLASS,PUBLIC, STATIC, VOID, MAIN
	,EXTENDS, RETURN, INT, CHAR, BOOLEAN, IF, ELSE, WHILE, LENGTH
	,FINAL, TRUE, FALSE, HIGH, LOW, INPUT, OUTPUT, THIS, NEW
	//Operadores
	,PONTO,ABRE_COLCHETES,FECHA_COLCHETES,ABRE_CHAVES,FECHA_CHAVES,ABRE_PARENTESES
	,FECHA_PARENTESES,E,OU,MENOR_QUE,MENOR_IGUAL_QUE,MAIOR_QUE,MAIOR_IGUAL_QUE,MAIS,MENOS,VEZES,DIVIDIDO
	,IGUAL,DIFERENTE,ATRIBUICAO,PONTO_E_VIRGULA,VIRGULA,INVERSAO
}
class Token {
	Kind kind
	int line,col,val
	String string = ""
}

class Scanner{	
	def static final keywords = 
		["class":Kind.CLASS
		,"public":Kind.PUBLIC
		,"int":Kind.INT
		,"char":Kind.CHAR
		,"boolean":Kind.BOOLEAN
		,"if":Kind.IF
		,"else":Kind.ELSE
		,"while":Kind.WHILE
		,"length":Kind.LENGTH
		,"final":Kind.FINAL
		,"true":Kind.TRUE
		,"false":Kind.FALSE
		,"high":Kind.HIGH
		,"low":Kind.LOW
		,"input":Kind.INPUT
		,"output":Kind.OUTPUT
		,"this":Kind.THIS
		,"new":Kind.NEW
		]
	def static final simple_tokens = 
		['.':Kind.PONTO
		,'[':Kind.ABRE_COLCHETES
		,']':Kind.FECHA_COLCHETES
		,'{':Kind.ABRE_CHAVES
		,'}':Kind.FECHA_CHAVES
		,'(':Kind.ABRE_PARENTESES
		,')':Kind.FECHA_PARENTESES
		,'+':Kind.MAIS
		,'-':Kind.MENOS
		,'*':Kind.VEZES
		,'/':Kind.DIVIDIDO
		,';':Kind.PONTO_E_VIRGULA
		,',':Kind.VIRGULA
		,'.':Kind.PONTO
		]
	def static final double_tokens = 
		['&&':Kind.E
		,'||':Kind.OU
		]
	def static equals_tokens = 
		['<':Kind.MENOR_QUE
		,'<=':Kind.MENOR_IGUAL_QUE
		,'>':Kind.MAIOR_QUE
		,'>=':Kind.MAIOR_IGUAL_QUE
		,'!':Kind.INVERSAO
		,'!=':Kind.DIFERENTE
		,'=':Kind.ATRIBUICAO
		,'==':Kind.IGUAL
		]	
	def static reader,line,col,size
	def static char ch
	def static eofCh = '\u0080' as char
	def static init(f){
		reader = new InputStreamReader(new FileInputStream(f))	
		size = f.size()
		line=1; col=0; nextCh() 
	}
	private static def nextCh(){
		ch = reader.read()
		if(ch ==~ /[\n]/){
			line++;col=0
		}else{
			 col++
		}
		size--
		//println "NextChar: ${Character.toString(ch)}"
	}
	def static Token next(){		
		if(ch.SIZE==0) nextCh()
		if(ch ==~ /[ \n]/){nextCh()}
		if(ch ==~/\t/) nextCh()
		if(ch ==~/\r/) nextCh()
		def t = ['line':line,'col':col] as Token	
		switch(ch){
			case ~/[a-z]|[A-Z]/:
				readName(t);break;
			case ~/[0-9]/:
				readNumber(t);break;
			case '/':
				t.string = ch;nextCh();
				if(ch==('/' as char)){
					while(ch!='\n') nextCh()
					t.kind = Kind.COMENTARIO
				}else{
					t.kind = Kind.DIVIDIDO
				}
				break;
			case ~/[\[\]{}()+-;,.]||\*/:
				t.string = ch
				t.kind = simple_tokens[ch as String];nextCh();break;
			case ~/[<>=!]/:
				t.string = ch;nextCh()
				if(ch=='=' as char){
					t.string +=ch;nextCh()	
				} 
				t.kind = equals_tokens[t.string]
				break;
			case eofCh:
				t.kind = Kind.EOF;nextCh();break;
			case '\'':	
				readCharCon(t)
				break;
			case ~/[&|]/:
				t.string = ch;nextCh();
				if(ch==t.string as char){
					t.string += ch;nextCh()		
					t.kind = double_tokens[t.string]
					break;
				}
			default:
				println "Erro: Token desconhecido ${t.string}"
				t.kind = Kind.UNKNOWN;nextCh()
		}
		t
	}
	def static boolean hasNext(){
		size>0
	}
	def static readName(t){
		while(ch ==~ /[a-z]|[A-Z]|[0-9]/){
			t.string += ch
			nextCh()
		}
		t.kind=keywords[t.string]?:Kind.IDENTIFIER
	}
	def static readNumber(t){
		def s = ""
		while(ch ==~ /[0-9]/){
			s += ch
			nextCh()
		}
		t.kind=Kind.NUMBER
		t.string = s
		t.val = Integer.valueOf(s)
	}
	def static readCharCon(t){
		t.kind = Kind.CHARCONST	
		t.string += ch;nextCh();
		while(ch != ('\'' as char) && !(ch ==~ /[\n]/)){
			t.string += ch;nextCh();
		}		
		
		if(ch == ('\'' as char)){
			t.string+=ch
			nextCh();
			if( ! ( t.string ==~ /('.')|('\\[rn\\]')/ )){
				println "Cadeia Inválida ${t.string} Linha: ${t.line} Coluna: ${t.col}";return
			}
			t.val = t.string.replace('\'',"").codePointAt(0)
		}else{	
			println "Cadeia Incompleta ${t.string} Linha:${t.line} Coluna:${t.col}"
			nextCh()
		}
		
	}
}

def static main(args){
	println "Running Javuino Compiler for file ${args[0]}"
	File f = new File(args[0])
	Scanner.init(f)
	while(Scanner.hasNext()){
		def t = Scanner.next()
		println "Token -> Kind: ${t.kind} String:${t.string} Value: ${t.val}"
	}
}



