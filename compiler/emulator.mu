ModuleInstance struct #RefType {
	def ModuleDef
	fullName string
	localName string
	localState Array<int> // localId -> stateId
	calls Array<int> // callId -> moduleInstanceId
	caller ModuleInstance
	callExpr CallExpression
	genGlobalName string
	genLocalName string
	genLocalsSet Set<string>
	genLocals Map<NodeWithCtx, string>
}

SlotInfo struct {
	name string
	tag Tag
	inst ModuleInstance
	node Node
	field int
	isStatic bool
	isReg bool
}

EmulatorState struct #RefType {
	comp Compilation
	symbols Map<string, Node>
	typeMap Map<Node, Tag>
	constMap Map<Node, Value>
	entities List<Node>
	errors List<Error>
	moduleInstances List<ModuleInstance>
	rs List<Value>
	ws List<Value>
	infos List<SlotInfo>
	evalOrder List<int>
	nextId Map<Node, int>

	started Array<bool>
	done Array<bool>	
	inst ModuleInstance	
	evalCtxField int
	evalCtxOutput int

	tape List<Instruction>
	resetProgram List<Instruction>
	propagateProgram List<Instruction>
	updateProgram List<Instruction>

	stack List<Value>
}

Emulator {
	init(comp Compilation, top ModuleDef) {
		s := new EmulatorState {
			comp: comp,
			symbols: comp.symbols,
			typeMap: comp.typeMap,
			constMap: comp.constMap,
			entities: comp.entities,
			errors: comp.errors,
			moduleInstances: new List<ModuleInstance>{},
			nextId: new Map.create<Node, int>(),
			rs: new List<Value>{},
			ws: new List<Value>{},
			infos: new List<SlotInfo>{},
			evalOrder: new List<int>{},
			evalCtxField: -1,
			evalCtxOutput: -1,			
			resetProgram: new List<Instruction>{},
			propagateProgram: new List<Instruction>{},
			updateProgram: new List<Instruction>{},
			stack: new List<Value>{},
		}
		
		s.stack.reserve(256)

		EmulatorAllocator.top(s, top)
		
		commitValues(s)

		EmulatorOrderCalculator.comp(s)

		if s.errors.count > 0 {
			return s
		}

		s.tape = s.resetProgram
		for si in s.evalOrder {
			if s.infos[si].isStatic || s.infos[si].isReg {
				EmulatorStep.slot(s, si)
			}
		}

		if s.comp.flags & CompilationFlags.simulate != 0 {
			s.tape = s.propagateProgram
			for si in s.evalOrder {
				if !s.infos[si].isStatic && !s.infos[si].isReg {
					EmulatorStep.slot(s, si)
				}
			}

			s.tape = s.updateProgram
			for mi in s.moduleInstances {
				EmulatorStep.module(s, mi)
				assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
			}
		}

		return s
	}

	reset(s EmulatorState) {
		for si := 0; si < s.rs.count {
			EmulatorRunner.setSlot(s, si, Value { kind: ValueKind.ulong_, z: 0 })
		}
		EmulatorRunner.run(s, s.resetProgram)
		Emulator.commitValues(s)
	}

	step(s EmulatorState) {
		EmulatorRunner.run(s, s.propagateProgram)
		EmulatorRunner.run(s, s.updateProgram)
		Emulator.commitValues(s)
		EmulatorRunner.run(s, s.propagateProgram)
	}

	getSlotIndex(inst ModuleInstance, name string) {
		sym := inst.def.symbols.get(name)
		match sym {
			ModuleInputDef: return inst.localState[sym.localId]
			AssignStatement: return inst.localState[sym.localId]
		}
	}

	commitValues(s EmulatorState) {
		for i := 0; i < s.ws.count {
			s.rs[i] = s.ws[i]
		}
	}
}

EmulatorStep {
	module(s EmulatorState, inst ModuleInstance) {
		s.inst = inst
		block(s, inst.def.body)
	}

	block(s EmulatorState, block Block) {
		for n in block.contents {
			match n {
				ClockStatement: clock(s, n)
				IfStatement: if_(s, n)
				AssignStatement: assign(s, n)
			}
		}
	}

	clock(s EmulatorState, st ClockStatement) {
		token(s, st.name) // TODO: does not actually check edges, just looks at current state
		if st.keyword.value != "posedge" {
			emit(s, Opcode.not)
		}
		pc := emit(s, Opcode.jumpIfZero)
		block(s, st.body)
		patch(s, pc)
	}

	if_(s EmulatorState, st IfStatement) {
		expression(s, st.expr)
		pc := emit(s, Opcode.jumpIfZero)
		block(s, st.ifBody)
		if st.elseBranch.is(IfStatement) {
			ifEnd := emit(s, Opcode.jump)
			patch(s, pc)
			if_(s, st.elseBranch.as(IfStatement))
			patch(s, ifEnd)
		} else if st.elseBranch.is(Block) {
			ifEnd := emit(s, Opcode.jump)
			patch(s, pc)
			block(s, st.elseBranch.as(Block))
			patch(s, ifEnd)
		} else {
			patch(s, pc)
		}
	}
	
	assign(s EmulatorState, st AssignStatement) {
		if st.flags & AssignFlags.regUpdate != 0 {
			si := s.inst.localState[st.localId]
			if st.nameExpr.is(Token) {
				tag := s.typeMap.get(st.expr)
				if tag.kind == TagKind.number {
					expression(s, st.expr)
					emiti(s, Opcode.store, si)
				} else if tag.kind == TagKind.struct_ {
					def := s.entities[tag.q].as(StructDef)
					for f, i in def.fields {
						s.evalCtxField = i
						expression(s, st.expr)
						emiti(s, Opcode.store, si + i)
					}
					s.evalCtxField = -1
				} else {
					abandon()
				}
			} else if st.nameExpr.is(DotExpression) {
				tag := s.typeMap.get(st.expr)
				if tag.kind == TagKind.number {
					expression(s, st.expr)
					emiti(s, Opcode.store, si + st.lhsFieldIndex)
				} else {
					abandon()
				}
			} else if st.nameExpr.is(CallExpression) {
				call := st.nameExpr.as(CallExpression)
				if call.builtin == BuiltinCall.slice {
					assignSlice(s, st, call, si)
				} else {
					abandon()
				}
			} else {
				abandon()
			}
		}
	}

	slot(s EmulatorState, si int) {
		info := s.infos[si]
		node := info.node
		s.evalCtxField = info.field
		match node {
			ModuleInputDef: {
				caller := info.inst.caller
				e := info.inst.callExpr
				s.inst = caller
				expression(s, e.args[e.calleeLocalIdToArgIndex[node.localId]].expr)
			}
			AssignStatement: {
				s.inst = info.inst
				expression(s, node.expr)
			}
		}
		emiti(s, Opcode.store, si)
	}

	pushValue(s EmulatorState, val Value) {
		if val.kind == ValueKind.ulong_ {
			emitz(s, Opcode.push, val.z)
		} else if val.kind == ValueKind.byteArray {
			emitz(s, Opcode.pushArray, val.z)
		} else {
			abandon()
		}
	}

	expression(s EmulatorState, e Node) {
		val := s.constMap.getOrDefault(e)
		if val.kind != ValueKind.none {
			assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
			pushValue(s, val)
		} else {
			expressionInner(s, e)
		}
	}

	expressionInner(s EmulatorState, e Node) {
		match e {
			Token: token(s, e)
			NumberExpression: number(s, e)
			UnaryOperatorExpression: unaryOperator(s, e)
			BinaryOperatorExpression: binaryOperator(s, e)
			DotExpression: dot(s, e)
			TernaryOperatorExpression: ternaryOperator(s, e)
			MatchExpression: match_(s, e)
			ParenExpression: paren(s, e)
			IndexExpression: index(s, e)
			CallExpression: call(s, e)
			StructInitializerExpression: structInit(s, e)
			BraceExpression: brace(s, e)
		}
	}

	token(s EmulatorState, e Token) {
		name := e.value
		node := s.inst != null ? s.inst.def.symbols.getOrDefault(name) : null
		match node {
			ModuleInputDef: {
				si := s.inst.localState[node.localId] + max(s.evalCtxField, 0)
				emiti(s, Opcode.load, si)
			}
			AssignStatement: {
				si := s.inst.localState[node.localId]
				if si >= 0 {
					emiti(s, Opcode.load, si + max(s.evalCtxField, 0))
				} else {
					expression(s, node.expr)
				}
			}
			default: {
				node = s.symbols.getOrDefault(name)
				match node {
					ConstDef: {
						assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
						pushValue(s, s.constMap.get(node.expr))
					}
				}
			}
		}
	}

	number(s EmulatorState, e NumberExpression) {
		emitz(s, Opcode.push, e.value)
	}

	unaryOperator(s EmulatorState, e UnaryOperatorExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		expression(s, e.expr)
		op := e.op.value
		if op == "-" {
			emit(s, Opcode.neg)
			mask(s, s.typeMap.get(e))
		} else if op == "~" {
			emit(s, Opcode.invert)
			mask(s, s.typeMap.get(e))
		} else if op == "zx" {
			// OK
		} else {
			abandon()
		}		
	}
	
	binaryOperator(s EmulatorState, e BinaryOperatorExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		expression(s, e.lhs)
		expression(s, e.rhs)
		op := e.op.value
		if op == "+" {
			emit(s, Opcode.add)
		} else if op == "-" {
			emit(s, Opcode.sub)
		} else if op == "&" {
			emit(s, Opcode.and)
		} else if op == "|" {
			emit(s, Opcode.or)
		} else if op == "^" {
			emit(s, Opcode.xor)
		} else if op == "*" {
			emit(s, Opcode.mul)
		} else if op == "==" {
			emit(s, Opcode.eq)
		} else if op == "!=" {
			emit(s, Opcode.neq)
		} else if op == "<=" {
			emit(s, Opcode.lte)
		} else if op == "<" {
			emit(s, Opcode.lt)
		} else if op == ">=" {
			emit(s, Opcode.gte)
		} else if op == ">" {
			emit(s, Opcode.gt)
		} else if op == "<<" {
			emit(s, Opcode.shl)
		} else if op == ">>" {
			emit(s, Opcode.shr)
		} else {
			abandon()
		}
		mask(s, s.typeMap.get(e))
	}
	
	dot(s EmulatorState, e DotExpression) {
		tag := s.typeMap.get(e.lhs)
		if tag.kind == TagKind.struct_ {
			assert(s.evalCtxField == -1)
			def := s.entities[tag.q].as(StructDef)
			s.evalCtxField = def.symbols.get(e.rhs.value).fieldIndex
			expression(s, e.lhs)
			s.evalCtxField = -1
		} else if tag.kind == TagKind.moduleOut {
			assert(s.evalCtxOutput == -1)
			def := s.entities[tag.q].as(ModuleDef)
			s.evalCtxOutput = def.symbols.get(e.rhs.value).as(AssignStatement).outputIndex
			expression(s, e.lhs)
			s.evalCtxOutput = -1
		} else {
			abandon()
		}
	}

	ternaryOperator(s EmulatorState, e TernaryOperatorExpression) {
		expression(s, e.conditionExpr)
		pc := emit(s, Opcode.jumpIfZero)
		expression(s, e.trueExpr)
		trueEnd := emit(s, Opcode.jump)
		patch(s, pc)
		expression(s, e.falseExpr)
		patch(s, trueEnd)
	}

	match_(s EmulatorState, e MatchExpression) {
		jumps := new List<int>{}
		expression(s, e.target)
		for c in e.cases {
			emit(s, Opcode.dup)
			expression(s, c.valueExpr)
			emit(s, Opcode.eq)
			next := emit(s, Opcode.jumpIfZero)
			emit(s, Opcode.discard)
			expression(s, c.resultExpr)
			jumps.add(emit(s, Opcode.jump))
			patch(s, next)
		}
		emit(s, Opcode.discard)
		emit(s, Opcode.push)
		for j in jumps {
			patch(s, j)
		}
	}

	paren(s EmulatorState, e ParenExpression) {
		expression(s, e.expr)
	}

	index(s EmulatorState, e IndexExpression) {
		upper := s.constMap.get(e.upperExpr).z
		lower := e.lowerExpr != null ? s.constMap.get(e.lowerExpr).z : upper
		expression(s, e.target)
		bits := (upper - lower + 1)
		if bits < 64 {
			emitz(s, Opcode.index, (lower << 32) | (bits & 0xffffffff_uL))
		} else {
			assert(upper == 63 && lower == 0)
		}		
	}

	call(s EmulatorState, e CallExpression) {
		if e.builtin == BuiltinCall.rep {
			rep(s, e)
			return
		} else if e.builtin == BuiltinCall.cast_ {
			cast_(s, e)
			return
		} else if e.builtin == BuiltinCall.slice {
			slice(s, e)
			return
		} else if e.builtin == BuiltinCall.chunk {
			chunk(s, e)
			return
		} else if e.builtin == BuiltinCall.swizzle {
			swizzle(s, e)
			return
		}
		assert(e.builtin == BuiltinCall.none)
		assert(s.evalCtxOutput >= 0)
		target := s.moduleInstances[s.inst.calls[e.callId]]
		assert(target.def.blackboxKeyword == null)
		output := target.def.outputs[s.evalCtxOutput]
		si := target.localState[output.localId] + max(s.evalCtxField, 0)
		emiti(s, Opcode.load, si)
	}

	rep(s EmulatorState, e CallExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		tag := s.typeMap.get(e.args[0].expr)
		assert(tag.kind == TagKind.number && tag.q > 0)
		n := TypeChecker.unpackInt(s.constMap.get(e.args[1].expr))
		expression(s, e.args[0].expr)
		for i := 1; i < n {
			emit(s, Opcode.dup)
			emiti(s, Opcode.shlOr, tag.q)
		}
	}

	cast_(s EmulatorState, e CallExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		fromTag := s.typeMap.get(e.args[0].expr)
		tag := s.typeMap.get(e.args[1].expr)
		assert(fromTag.kind == TagKind.number)
		assert(tag.kind == TagKind.number && tag.q > 0)
		expression(s, e.args[0].expr)
		if fromTag.q == 0 {
			if tag.q <= 64 {
				mask(s, tag)
			}
		} else if tag.q < fromTag.q {
			if tag.q <= 64 {
				if fromTag.q > 64 {
					emit(s, Opcode.toULong)
				}
				mask(s, tag)
			} else {
				abandon()
			}
		}
	}

	slice(s EmulatorState, e CallExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		target := s.typeMap.get(e.args[0].expr)
		expression(s, e.args[0].expr)
		expression(s, e.args[1].expr)
		expression(s, e.args[2].expr)
		emiti(s, Opcode.slice, TypeChecker.unpackWidth(target))
	}

	assignSlice(s EmulatorState, st AssignStatement, e CallExpression, si int) {
		expression(s, e.args[1].expr)
		expression(s, e.args[2].expr)
		expression(s, st.expr)
		emiti(s, Opcode.storeSlice, si)
	}

	chunk(s EmulatorState, e CallExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		target := s.typeMap.get(e.args[0].expr)
		targetSize := TypeChecker.unpackWidth(target)
		index := TypeChecker.unpackInt(s.constMap.get(e.args[1].expr))
		numChunks := TypeChecker.unpackInt(s.constMap.get(e.args[2].expr))
		assert(targetSize % numChunks == 0)
		chunkSize := targetSize / numChunks
		offset := index * chunkSize
		expression(s, e.args[0].expr)
		emiti(s, Opcode.push, offset)
		emiti(s, Opcode.push, chunkSize)
		emiti(s, Opcode.slice, targetSize)
	}

	swizzle(s EmulatorState, e CallExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		targetWidth := TypeChecker.unpackWidth(s.typeMap.get(e.args[0].expr))
		seqSize := TypeChecker.unpackInt(s.constMap.get(e.args[1].expr))
		readStep := TypeChecker.unpackInt(s.constMap.get(e.args[2].expr))
		blockSize := TypeChecker.unpackInt(s.constMap.get(e.args[3].expr))
		expression(s, e.args[0].expr)
		emiti(s, Opcode.push, targetWidth)
		emiti(s, Opcode.push, seqSize)
		emiti(s, Opcode.push, readStep)
		emiti(s, Opcode.push, blockSize)
		emit(s, Opcode.swizzle)
	}

	structInit(s EmulatorState, e StructInitializerExpression) {
		assert(s.evalCtxField >= 0)
		tag := s.typeMap.get(e)
		assert(tag.kind == TagKind.struct_)
		def := s.entities[tag.q].as(StructDef)
		argIndex := e.fieldIndexToArgIndex[s.evalCtxField]
		if argIndex >= 0 {
			arg := e.args[argIndex].expr
			prevCtxField := s.evalCtxField
			s.evalCtxField = -1
			expression(s, arg)
			s.evalCtxField = prevCtxField
		} else {
			emit(s, Opcode.push)
		}
	}

	brace(s EmulatorState, e BraceExpression) {
		assert(s.evalCtxField == -1 && s.evalCtxOutput == -1)
		for arg, i in e.args {
			tag := s.typeMap.get(arg)
			assert(tag.kind == TagKind.number && tag.q > 0)
			expression(s, arg)
			if i > 0 {
				emiti(s, Opcode.shlOr, tag.q)
			}
		}
	}

	emit(s EmulatorState, op Opcode) {
		index := s.tape.count
		s.tape.add(Instruction { op: op })
		return index
	}

	emiti(s EmulatorState, op Opcode, z int) {
		index := s.tape.count
		s.tape.add(Instruction { op: op, z: checked_cast(z, ulong) })
		return index
	}

	emitz(s EmulatorState, op Opcode, z ulong) {
		index := s.tape.count
		s.tape.add(Instruction { op: op, z: z })
		return index
	}

	patch(s EmulatorState, index int) {
		op := s.tape[index].op
		assert(op == Opcode.jump || op == Opcode.jumpIfZero)
		s.tape[index].z = checked_cast(s.tape.count - index - 1, ulong)
	}

	mask(s EmulatorState, tag Tag) {
		assert(tag.kind == TagKind.number && tag.q <= 64)
		if tag.q > 0 && tag.q < 64 {
			emitz(s, Opcode.mask, (1_uL << tag.q) - 1)
		}
	}
}





Util {
	writeByteHexTo(b byte, sb StringBuilder) {
		if b < 16 {
			sb.write("0")
		}
		ulong.writeHexTo(b, sb)
	}

	valEquals(a Value, b Value) {
		if a.kind != b.kind {
			return false
		}
		if a.kind == ValueKind.ulong_ {
			return a.z == b.z
		} else if a.kind == ValueKind.byteArray {
			return a.z == b.z || arrayEquals(EmulatorRunner.unpackArray(a), EmulatorRunner.unpackArray(b))
		} else {
			abandon()
		}
	}

	arrayEquals(a Array<T>, b Array<T>) {
		if a.count != b.count {
			return false
		}
		for i := 0; i < a.count {
			if a[i] != b[i] {
				return false
			}
		}
		return true
	}

	shlArray(a Array<byte>, n int) {
		assert(0 <= n && n < 8)
		if n == 0 {
			return
		}
		m := 8 - n
		for i := 0; i < a.count - 1 {
			a[i] = cast((a[i] >> n) | (a[i + 1] << m), byte)
		}
		a[a.count - 1] = cast(a[a.count - 1] >> n, byte)
	}
}
