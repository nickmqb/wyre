EmulatorAllocator {
	top(s EmulatorState, def ModuleDef) {
		for i := 0; i < def.numRegSlots {
			s.rs.add(Value{})
			s.infos.add(SlotInfo{})
		}
		module(s, def, "")
		assert(s.ws.count == def.numRegSlots)		
	}

	module(s EmulatorState, def ModuleDef, name string) int {
		fullName := s.inst != null ? formatName(s.inst.fullName, name) : name
		s.inst = new ModuleInstance {
			def: def,
			localName: name,
			fullName: fullName,
			localState: new Array<int>(def.symbols.count + 1),
			calls: new Array<int>(def.numCalls + 1),
		}
		for i := 0; i <= def.symbols.count {
			s.inst.localState[i] = -1
		}
		id := s.moduleInstances.count
		s.moduleInstances.add(s.inst)
		for inp in def.inputs {
			s.inst.localState[inp.localId] = s.rs.count
			tag := s.typeMap.get(inp)
			isStatic := inp.flags & ModuleInputFlags.static != 0
			if tag.kind == TagKind.number {
				s.rs.add(createValue(s, tag, true))
				s.infos.add(SlotInfo { name: formatName(s.inst.fullName, inp.name.value), tag: tag, inst: s.inst, node: inp, field: -1, isStatic: isStatic })
			} else if tag.kind == TagKind.struct_ {
				struct_ := s.entities[tag.q].as(StructDef)
				for f, fi in struct_.fields {
					s.rs.add(Value { kind: ValueKind.ulong_ })
					fname := formatName(formatName(s.inst.fullName, inp.name.value), f.name.value)
					ftag := s.typeMap.get(f)
					s.infos.add(SlotInfo { name: fname, tag: ftag, inst: s.inst, node: inp, field: fi })
				}
			} else {
				abandon()
			}
		}
		block(s, def.body)
		return id
	}

	block(s EmulatorState, st Block) {
		for n in st.contents {
			match n {
				ClockStatement: block(s, n.body)
				IfStatement: if_(s, n)
				AssignStatement: assign(s, n)
			}
		}
	}
	
	if_(s EmulatorState, st IfStatement) {
		expression(s, st.expr)
		block(s, st.ifBody)
		if st.elseBranch.is(IfStatement) {
			if_(s, st.elseBranch.as(IfStatement))
		} else if st.elseBranch.is(Block) {
			block(s, st.elseBranch.as(Block))
		}
	}

	assign(s EmulatorState, st AssignStatement) {
		if st.flags & AssignFlags.reg != 0 {			
			index := s.ws.count
			s.inst.localState[st.localId] = index
			tag := s.typeMap.get(st)
			if tag.kind == TagKind.number {
				s.ws.add(createValue(s, tag, true))
				s.infos[index] = SlotInfo { name: formatName(s.inst.fullName, st.nameExpr.as(Token).value), tag: tag, inst: s.inst, node: st, field: -1, isReg: true }
			} else if tag.kind == TagKind.struct_ {
				def := s.entities[tag.q].as(StructDef)
				for f, fi in def.fields {
					s.ws.add(Value { kind: ValueKind.ulong_ })
					fname := formatName(formatName(s.inst.fullName, st.nameExpr.as(Token).value), f.name.value)
					ftag := s.typeMap.get(f)
					s.infos[index] = SlotInfo { name: fname, tag: ftag, inst: s.inst, node: st, field: fi, isReg: true }
					index += 1
				}
			} else {
				abandon()
			}
		} else if st.flags & AssignFlags.wire != 0 {
			tag := s.typeMap.get(st)
			isStatic := st.flags & AssignFlags.static != 0
			if tag.kind == TagKind.number {
				s.inst.localState[st.localId] = s.rs.count
				s.rs.add(createValue(s, tag, true))
				s.infos.add(SlotInfo { name: formatName(s.inst.fullName, st.nameExpr.as(Token).value), tag: tag, inst: s.inst, node: st, field: -1, isStatic: isStatic })
			} else if tag.kind == TagKind.struct_ {
				s.inst.localState[st.localId] = s.rs.count
				def := s.entities[tag.q].as(StructDef)
				for f, fi in def.fields {
					s.rs.add(Value { kind: ValueKind.ulong_ })
					fname := formatName(formatName(s.inst.fullName, st.nameExpr.as(Token).value), f.name.value)
					ftag := s.typeMap.get(f)
					s.infos.add(SlotInfo { name: fname, tag: ftag, inst: s.inst, node: st, field: fi, isStatic: isStatic })
				}
			} else if tag.kind == TagKind.moduleOut {
				// OK
			} else {
				abandon()
			}
		}
		if st.nameExpr.is(Token) && st.expr.is(CallExpression) {
			// Special case to assign better name to module instance
			call(s, st.expr.as(CallExpression), st.nameExpr.as(Token).value)
		} else if st.expr != null {
			expression(s, st.expr)
		}
	}

	createValue(s EmulatorState, tag Tag, allocArray bool) {
		if tag.kind == TagKind.number && tag.q > 64 {
			return Value { kind: ValueKind.byteArray, z: allocArray ? transmute(new Array<byte>((tag.q + 7) / 8), ulong) : 0 }
		} else if tag.kind == TagKind.number {
			return Value { kind: ValueKind.ulong_ }
		} else {
			abandon()
		}
	}

	expression(s EmulatorState, e Node) {
		match e {
			Token: {}
			NumberExpression: {}
			UnaryOperatorExpression: {
				expression(s, e.expr)
			}
			BinaryOperatorExpression: { 
				expression(s, e.lhs)
				expression(s, e.rhs)
			}
			DotExpression: {
				expression(s, e.lhs)
				expression(s, e.rhs)
			}
			TernaryOperatorExpression: {
				expression(s, e.conditionExpr)
				expression(s, e.trueExpr)
				expression(s, e.falseExpr)
			}
			MatchExpression: {
				expression(s, e.target)
				for c in e.cases {
					expression(s, c.valueExpr)
					expression(s, c.resultExpr)
				}
			}
			ParenExpression: {
				expression(s, e.expr)
			}
			IndexExpression: {
				expression(s, e.target)
				expression(s, e.upperExpr)
				expression(s, e.lowerExpr)
			}
			BraceExpression: {
				for arg in e.args {
					expression(s, arg)
				}
			}
			CallExpression: {
				call(s, e, "")
			}
			StructInitializerExpression: {
				for arg in e.args {
					expression(s, arg.expr)
				}
			}
			ArrayExpression: {}
			null: {}
		}
	}

	call(s EmulatorState, e CallExpression, name string) {
		for arg in e.args {
			if arg.expr != null {
				expression(s, arg.expr)
			}
		}
		if e.builtin != BuiltinCall.none {
			return
		}
		def := s.symbols.get(e.target.as(Token).value).as(ModuleDef)
		if name == "" {
			id := s.nextId.getOrDefault(def)
			s.nextId.addOrUpdate(def, id + 1)
			name = format("{}_{}", def.name.value, id)
			
			assert(s.symbols.getOrDefault(name) == null) // TODO: error message
			assert(s.inst.def.symbols.getOrDefault(name) == null) // TODO: error message
		}
		prevInst := s.inst
		instanceId := module(s, def, name)
		s.inst.caller = prevInst
		s.inst.callExpr = e
		s.inst = prevInst
		s.inst.calls[e.callId] = instanceId		
	}

	formatName(a string, b string) {
		if a == "" {
			return b
		}
		return format("{}.{}", a, b)
	}
}

EmulatorOrderCalculator {
	comp(s EmulatorState) {
		s.started = new Array<bool>(s.rs.count)
		s.done = new Array<bool>(s.rs.count)

		top := s.moduleInstances[0]
		for inp in top.def.inputs {
			si := top.localState[inp.localId]
			tag := s.infos[si].tag
			if tag.kind == TagKind.number {
				s.started[si] = true
				s.done[si] = true
			} else if tag.kind == TagKind.struct_ {
				struct_ := s.entities[tag.q].as(StructDef)
				for f, fi in struct_.fields {
					s.started[si + fi] = true
					s.done[si + fi] = true
				}
			}
		}

		for mi in s.moduleInstances {
			module(s, mi)
		}
	}

	module(s EmulatorState, inst ModuleInstance) {
		s.inst = inst
		if inst.def.blackboxKeyword == null {
			block(s, inst.def.body)	
		} else {
			for inp in inst.def.inputs {
				input(s, inp)
			}
		}		
	}

	block(s EmulatorState, st Block) {
		for n in st.contents {
			match n {
				ClockStatement: clock(s, n)
				IfStatement: if_(s, n)
				AssignStatement: assign(s, n)
			}
		}
	}
	
	clock(s EmulatorState, st ClockStatement) {
		token(s, st.name)
		block(s, st.body)
	}

	if_(s EmulatorState, st IfStatement) {
		expression(s, st.expr)
		block(s, st.ifBody)
		if st.elseBranch.is(IfStatement) {
			if_(s, st.elseBranch.as(IfStatement))
		} else if st.elseBranch.is(Block) {
			block(s, st.elseBranch.as(Block))
		}
	}

	assign(s EmulatorState, st AssignStatement) {
		if st.flags & AssignFlags.reg != 0 && st.flags & AssignFlags.regUpdate == 0 && st.expr != null {			
			tag := s.typeMap.get(st)
			if tag.kind == TagKind.number {
				expression(s, st.expr)
				s.evalOrder.add(s.inst.localState[st.localId])
			} else if tag.kind == TagKind.struct_ {
				def := s.entities[tag.q].as(StructDef)
				for f, fi in def.fields {
					s.evalCtxField = fi
					expression(s, st.expr)
					s.evalOrder.add(s.inst.localState[st.localId] + fi)
				}
				s.evalCtxField = -1
			} else {
				abandon()
			}
		}
		if st.flags & AssignFlags.wire != 0 && st.outKeyword != null && s.inst == s.moduleInstances[0] {
			tag := s.typeMap.get(st)
			if tag.kind == TagKind.number {
				wire(s, st)
			} else if tag.kind == TagKind.struct_ {
				def := s.entities[tag.q].as(StructDef)
				for f, i in def.fields {
					s.evalCtxField = i
					wire(s, st)
				}
				s.evalCtxField = -1
			} else if tag.kind == TagKind.moduleOut {
				// OK
			} else {
				abandon()
			}
		}
		if st.flags & AssignFlags.regUpdate != 0 {
			si := s.inst.localState[st.localId]
			if st.nameExpr.is(Token) {
				tag := s.typeMap.get(st.expr)
				if tag.kind == TagKind.number {
					expression(s, st.expr)
				} else if tag.kind == TagKind.struct_ {
					def := s.entities[tag.q].as(StructDef)
					for f, i in def.fields {
						s.evalCtxField = i
						expression(s, st.expr)
					}
					s.evalCtxField = -1
				} else {
					abandon()
				}
			} else if st.nameExpr.is(DotExpression) {
				tag := s.typeMap.get(st.expr)
				if tag.kind == TagKind.number {
					expression(s, st.expr)
				} else {
					abandon()
				}
			} else if st.nameExpr.is(CallExpression) {
				callExpr := st.nameExpr.as(CallExpression)
				if callExpr.builtin == BuiltinCall.slice {
					call(s, callExpr)
					expression(s, st.expr)
				} else {
					abandon()
				}
			} else {
				abandon()
			}
		}
	}

	expression(s EmulatorState, e Node) {
		match e {
			Token: token(s, e)
			NumberExpression: {}
			UnaryOperatorExpression: {
				expression(s, e.expr)
			}
			BinaryOperatorExpression: { 
				expression(s, e.lhs)
				expression(s, e.rhs)
			}
			DotExpression: dot(s, e)
			TernaryOperatorExpression: {
				expression(s, e.conditionExpr)
				expression(s, e.trueExpr)
				expression(s, e.falseExpr)
			}
			MatchExpression: {
				expression(s, e.target)
				for c in e.cases {
					expression(s, c.valueExpr)
					expression(s, c.resultExpr)
				}
			}
			ParenExpression: {
				expression(s, e.expr)
			}
			IndexExpression: {
				expression(s, e.target)
				expression(s, e.upperExpr)
				expression(s, e.lowerExpr)
			}
			BraceExpression: {
				for arg in e.args {
					expression(s, arg)
				}
			}
			CallExpression: call(s, e)
			StructInitializerExpression: structInit(s, e)
			ArrayExpression: {}
			null: {}
		}
	}

	input(s EmulatorState, inp ModuleInputDef) {
		si := s.inst.localState[inp.localId] + max(s.evalCtxField, 0)
		if s.done[si] {
			return
		}
		if !s.started[si] {
			caller := s.inst.caller
			callExpr := s.inst.callExpr
			s.started[si] = true
			prevInst := s.inst
			s.inst = caller
			expression(s, callExpr.args[callExpr.calleeLocalIdToArgIndex[inp.localId]].expr)
			s.evalOrder.add(si)
			s.inst = prevInst
			s.done[si] = true
		} else {
			logicLoop(s, inp.name)
		}
	}

	wire(s EmulatorState, st AssignStatement) {
		si := s.inst.localState[st.localId]
		if si == -1 {
			expression(s, st.expr)
			return
		}
		si += max(s.evalCtxField, 0)
		if s.done[si] {
			return
		} else if !s.started[si] {
			s.started[si] = true
			expression(s, st.expr)
			s.evalOrder.add(si)
			s.done[si] = true
		} else {
			logicLoop(s, st.nameExpr)
		}
	}

	assignRef(s EmulatorState, st AssignStatement) {
		si := s.inst.localState[st.localId]
		if st.flags & AssignFlags.reg != 0 {
			assert(s.evalCtxOutput == -1)
		} else if st.flags & AssignFlags.wire != 0 {
			wire(s, st)
		} else {
			abandon()
		}
	}

	token(s EmulatorState, e Token) {
		name := e.value
		node := s.inst != null ? s.inst.def.symbols.getOrDefault(name) : null
		match node {
			ModuleInputDef: {
				input(s, node)
			}
			AssignStatement: {
				assignRef(s, node)
			}
			null: {}
		}
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

	call(s EmulatorState, e CallExpression) {
		if e.builtin != BuiltinCall.none {
			assert(s.evalCtxOutput == -1)
			for arg in e.args {
				expression(s, arg.expr)
			}
			return
		}
		assert(s.evalCtxOutput >= 0)
		target := s.moduleInstances[s.inst.calls[e.callId]]
		if target.def.blackboxKeyword != null {
			return
		}
		output := target.def.outputs[s.evalCtxOutput]
		prevInst := s.inst
		prevCtxOutput := s.evalCtxOutput
		s.inst = target
		s.evalCtxOutput = -1
		assignRef(s, output)
		s.inst = prevInst
		s.evalCtxOutput = prevCtxOutput
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
		}
	}

	logicLoop(s EmulatorState, e Node) {
		s.errors.add(Error.at(s.inst.def.unit, RangeFinder.find(e), "Logic loop detected"))
	}
}
