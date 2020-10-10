IndexerState struct #RefType {
	comp Compilation
	unit CodeUnit
	module ModuleDef
}

Indexer {
	// Global pass to construct symbol tables
	comp(comp Compilation) {
		comp.symbols = new Map.create<string, Node>()
		comp.entities = new List<Node>{}
		comp.nonSyntaxErrorStart = comp.errors.count
		s := new IndexerState {
			comp: comp,			
		}
		for u in comp.units {
			s.unit = u
			unit(s, u)
		}
	}

	unit(s IndexerState, unit CodeUnit) {
		comp := s.comp
		for n in unit.contents {
			match n {
				ConstDef: {
					if n.name != null {
						name := n.name.value
						if comp.symbols.tryAdd(name, n) {
							n.id = comp.entities.count
							comp.entities.add(n)
						} else {
							duplicateSymbol(s, n.name)
						}
					}
				}
				StructDef: {
					name := n.name.value
					if comp.symbols.tryAdd(name, n) {
						n.id = comp.entities.count
						comp.entities.add(n)
					} else {
						duplicateSymbol(s, n.name)
					}
				}
				ModuleDef: {
					name := n.name.value
					if comp.symbols.tryAdd(name, n) {
						n.id = comp.entities.count
						comp.entities.add(n)
					} else {
						duplicateSymbol(s, n.name)
					}
					s.module = n
					if name.endsWith("_builtin") {
						s.module.flags |= ModuleFlags.builtin
					}
					n.symbols = new Map.create<string, Node>()
					n.outputs = new List<AssignStatement>{}
					for input in n.inputs {
						if s.module.symbols.tryAdd(input.name.value, input) {
							input.localId = s.module.symbols.count
						} else {
							duplicateSymbol(s, input.name)
						}
					}
					block(s, n.body)
				}
				default: {}
			}
		}
	}

	block(s IndexerState, st Block) {
		for n in st.contents {
			match n {
				ClockStatement: block(s, n.body)
				IfStatement: if_(s, n)
				AssignStatement: assign(s, n)
				default: {}
			}
		}
	}

	if_(s IndexerState, st IfStatement) {
		block(s, st.ifBody)
		if st.elseBranch.is(IfStatement) {
			if_(s, st.elseBranch.as(IfStatement))
		} else if st.elseBranch.is(Block) {
			block(s, st.elseBranch.as(Block))
		}
	}

	assign(s IndexerState, st AssignStatement) {
		module := s.module
		if st.regKeyword != null {
			st.flags |= AssignFlags.reg
			if st.nameExpr.is(Token) {
				nameToken := st.nameExpr.as(Token)
				name := nameToken.value
				if s.module.symbols.tryAdd(name, st) {
					st.localId = s.module.symbols.count
				} else {
					duplicateSymbol(s, nameToken)
				}
			}
		} else if st.op != null && st.op.value == "<=" {
			st.flags |= AssignFlags.regUpdate
		} else {
			st.flags |= AssignFlags.wire
			if st.nameExpr.is(Token) {
				nameToken := st.nameExpr.as(Token)
				name := nameToken.value
				if s.module.symbols.tryAdd(name, st) {
					st.localId = s.module.symbols.count
				} else {
					duplicateSymbol(s, nameToken)
				}
			}
		}
	}

	duplicateSymbol(s IndexerState, token Token) {
		s.comp.errors.add(Error.at(s.unit, token.span, "A symbol with the same name has already been defined"))
	}
}
