//tab_size=4
Token struct #RefType {
	type TokenType
	value string
	span IntRange
	outerSpan IntRange
	indent int
}

TokenType enum {
	identifier
	numberLiteral
	operator
	openParen
	closeParen
	openBrace
	closeBrace
	openBracket
	closeBracket
	comma
	end
	invalid
}

Error struct {
	unit CodeUnit
	span IntRange
	text string
	
	at(unit CodeUnit, span IntRange, text string) {
		assert(span.from <= span.to)
		return Error { unit: unit, span: span, text: text }
	}

	atIndex(unit CodeUnit, index int, text string) {
		return Error { unit: unit, span: IntRange(index, index), text: text }
	}
}

Node tagged_pointer {
    CodeUnit
    ConstDef
	StructDef
	FieldDef
    ModuleDef
    ModuleInputDef
    Block
    AssignStatement    
    ClockStatement
    IfStatement
    UnaryOperatorExpression
    DotExpression
    BinaryOperatorExpression
    TernaryOperatorExpression
    MatchExpression
	MatchExpressionCase
    CallExpression
	StructInitializerExpression
    IndexExpression
    NumberExpression
    ParenExpression
    BraceExpression
	ArrayExpression
    Token

	hash(n Node) {
		return cast(transmute(pointer_cast(n, pointer), usize) >> 3, uint)
	}
}

CodeUnit struct #RefType {    
    path string
    source string
    contents List<Node>
    id int
}

ConstDef struct #RefType {    
    colon Token
    name Token
	type Token
    assign Token
    expr Node
	unit CodeUnit
	// Non AST
	id int
	flags ConstFlags
}

ConstFlags enum #Flags {
	typeCheckStarted
	typeCheckDone
}

StructDef struct #RefType {
	name Token
	keyword Token
	body Block
	fields List<FieldDef>
	unit CodeUnit
	// Non AST
	id int
	symbols Map<string, FieldDef>
	flags StructFlags
}

StructFlags enum #Flags {
	//typeCheckStarted
	typeCheckDone
}

FieldDef struct #RefType {
	name Token
	type Token
	// Non AST
	fieldIndex int
}

ModuleDef struct #RefType {
	name Token
	blackboxKeyword Token
	openParen Token
	inputs List<ModuleInputDef>
	inputsContents List<Node>
	closeParen Token
    body Block
	unit CodeUnit
	// Non AST
	id int
	symbols Map<string, Node>
	outputs List<AssignStatement>
	flags ModuleFlags
	numCalls int
	numInputSlots int
	numRegSlots int	
}

ModuleFlags enum #Flags {
	top
	typeCheckStarted
	typeCheckDone
}

ModuleInputDef struct #RefType {
    name Token
    type Token
	// Non AST
	flags ModuleInputFlags
	localId int
}

ModuleInputFlags enum #Flags {
	static
}

Block struct #RefType {
    openBrace Token
    contents List<Node>
    closeBrace Token
}

ClockStatement struct #RefType {
    keyword Token
    name Token
    body Block
}

IfStatement struct #RefType {
    ifKeyword Token
    expr Node
    ifBody Block
	elseKeyword Token
	elseBranch Node // IfStatement or Block
}

AssignStatement struct #RefType {
    outKeyword Token
    regKeyword Token
    nameExpr Node
    type Token
    op Token
    expr Node
	module ModuleDef
	// Non AST
	flags AssignFlags
	localId int
	lhsFieldIndex int
	outputIndex int
}

AssignFlags enum #Flags {
	reg
	regUpdate
	wire
	static
	typeCheckStarted
	typeCheckDone
}

UnaryOperatorExpression struct #RefType {
	op Token
	expr Node
}

DotExpression struct #RefType {
	lhs Node
	dot Token
	rhs Token
}

BinaryOperatorExpression struct #RefType {
	lhs Node
	op Token
	rhs Node
}

TernaryOperatorExpression struct #RefType {
	conditionExpr Node
	question Token
	trueExpr Node
	colon Token
	falseExpr Node
}

MatchExpression struct #RefType {
    keyword Token
	target Node
    openBrace Token
	cases List<MatchExpressionCase>
	contents List<Node>
    closeBrace Token
}

MatchExpressionCase struct #RefType {
    valueExpr Node
	colon Token
    resultExpr Node
}

CallExpression struct #RefType {
	target Node
	openParen Token
	args List<CallArg>
	contents List<Node>
	closeParen Token
	builtin BuiltinCall
	// Non-AST
	callId int
	calleeLocalIdToArgIndex Array<int>
}

BuiltinCall enum {
	none
	rep
	cast_
	slice
	chunk
	swizzle
}

CallArg struct #RefType {
    name Token
    colon Token
    expr Node
}

StructInitializerExpression struct #RefType {
	target Node
	openBrace Token
	args List<CallArg>
	contents List<Node>
	closeBrace Token
	// Non-AST
	fieldIndexToArgIndex Array<int>
}

ParenExpression struct #RefType {
	openParen Token
	expr Node
	closeParen Token
}

BraceExpression struct #RefType {
	openBrace Token
	args List<Node>
	contents List<Node>
	closeBrace Token
}

IndexExpression struct #RefType {
	target Node
	openBracket Token
	upperExpr Node
    colon Token
    lowerExpr Node
	closeBracket Token
}

NumberExpression struct #RefType {
	token Token
	//valueSpan IntRange
	flags NumberFlags
	value ulong
	dontCare ulong
    width int
}

NumberFlags enum #Flags {
	none = 0
	valid = 1
    exactWidth = 2
	dontCare = 4
}

ArrayExpression struct #RefType {
	openBracket Token
	contents List<Node>
	data List<byte>
	closeBracket Token
}

Compilation struct #RefType {
	sources List<SourceFile>
	flags CompilationFlags
	units List<CodeUnit>
	symbols Map<string, Node>
	entities List<Node>
	typeMap Map<Node, Tag>
	constMap Map<Node, Value>
	errors List<Error>
	nonSyntaxErrorStart int
	elapsedSeconds double
}

CompilationFlags enum #Flags {
	generate
	simulate
}
