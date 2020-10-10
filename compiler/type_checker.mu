TypeCheckerState struct #RefType {
	comp Compilation
	unit CodeUnit
	module ModuleDef
	struct_ StructDef
	symbols Map<string, Node>
	typeMap Map<Node, Tag>
	constMap Map<Node, Value>
	queue List<QueuedCallArgs>
	gap bool
	gapTag Tag
	inClock bool
	isStaticExpr bool
	allowDontCare bool
	errors List<Error>
}

TypeCheckerContext struct {
	unit CodeUnit
	module ModuleDef
	inClock bool
	allowDontCare bool
	isStaticExpr bool
}

TagKind enum {
	unknown
	number
	moduleOut
	struct_
}

Tag struct {
	kind TagKind
	q int

	cons(kind TagKind, q int) {
		return Tag { kind: kind, q: q }
	}

	isValid(t Tag) {
		return t.kind != TagKind.unknown
	}
}

ValueKind enum {
	none
	ulong_
	byteArray
}

Value struct {
	kind ValueKind
	z ulong
}

TypeCheckResult struct {
	tag Tag
	value Value
}

QueuedCallArgs struct {
	e CallExpression
	def ModuleDef
	context ModuleDef
}

TypeChecker {
	comp(comp Compilation) {
		comp.typeMap = new Map.create<Node, Tag>()
		comp.constMap = new Map.create<Node, Value>()
		s := new TypeCheckerState {
			comp: comp,
			errors: comp.errors,
			symbols: comp.symbols,
			typeMap: comp.typeMap,
			constMap: comp.constMap,
			queue: new List<QueuedCallArgs>{},
		}

		for u in comp.units {
			unit(s, u)
		}

		index := 0
		while index < s.queue.count {
			item := s.queue[index]
			s.unit = item.context.unit
			s.module = item.context
			callArgs(s, item.e, item.def)
			index += 1
		}
	}

	unit(s TypeCheckerState, unit CodeUnit) {
		s.unit = unit
		for n in unit.contents {
			match n {
				ConstDef: const(s, n)
				StructDef: struct_(s, n)
				ModuleDef: module(s, n)
				default: {}
			}
		}
	}

	const(s TypeCheckerState, def ConstDef) {
		if def.flags & ConstFlags.typeCheckDone != 0 {
			return
		}
		if !isValidEntityName(def.name.value) {
			invalidNamePrefix(s, def.name)
		}
		def.flags |= ConstFlags.typeCheckStarted
		dt := Tag{}
		if def.type != null {
			dt = typename(s, def, def.type)
		}
		if def.expr != null {
			s.isStaticExpr = true
			etr := expressionWithGap(s, def.expr, def.type != null, dt)
			s.isStaticExpr = false
			if etr.tag.kind != TagKind.unknown {
				if def.type != null {
					if canAssign_andUpgrade(s, etr.tag, etr.value, def.expr, dt) {
						if etr.value.kind != ValueKind.none {
							// OK
						} else {
							expectedConstant(s, def.expr)
						}
					} else {
						badAssign(s, def.assign, etr.tag, dt)
					}
				} else {
					s.typeMap.add(def, etr.tag)
					if etr.value.kind != ValueKind.none {
						// OK
					} else {
						expectedConstant(s, def.expr)
					}
				}
			}
		}
		def.flags |= ConstFlags.typeCheckDone
	}

	ensureConstDone(s TypeCheckerState, def ConstDef, reference Node) {
		if def.flags & ConstFlags.typeCheckDone != 0 {
			// OK
		} else if def.flags & ConstFlags.typeCheckStarted == 0 {
			prev := pushContext(s, def.unit, null)
			const(s, def)
			restoreContext(s, prev)
		} else {
			s.errors.add(Error.at(s.unit, RangeFinder.find(reference), format("Recursive reference to const: {}", def.name.value)))
		}
	}

	struct_(s TypeCheckerState, def StructDef) {
		if def.flags & StructFlags.typeCheckDone != 0 {
			return
		}
		if !isValidEntityName_nonStatic(def.name.value) {
			invalidNamePrefix_nonStatic(s, def.name)
		}
		if def.fields.count == 0 {
			s.errors.add(Error.atIndex(s.unit, def.body.openBrace != null ? def.body.openBrace.span.to : def.name.span.to, "Struct must define at least one field"))
		}
		def.symbols = new Map.create<string, FieldDef>()
		s.struct_ = def
		block(s, def.body)
		s.struct_ = null
		def.flags |= StructFlags.typeCheckDone
	}

	ensureStructDone(s TypeCheckerState, def StructDef) {
		if def.flags & StructFlags.typeCheckDone != 0 {
			prev := pushContext(s, def.unit, null)
			struct_(s, def)
			restoreContext(s, prev)
		}
	}

	fieldDef(s TypeCheckerState, def FieldDef) {		
		if s.struct_.symbols.tryAdd(def.name.value, def) {
			def.fieldIndex = s.struct_.symbols.count - 1
		} else {
			duplicateSymbol(s, def.name)
		}
		if !isValidEntityName_nonStatic(def.name.value) {
			invalidNamePrefix_nonStatic(s, def.name)
		}
		if def.type != null {
			tag := typename(s, def, def.type)
			if tag.isValid() {
				if tag.kind == TagKind.number && tag.q > 0 {
					if tag.q <= 64 {
						// OK
					} else {
						s.errors.add(Error.at(s.unit, def.type.span, "Field type cannot be larger than $64"))
					}
				} else {
					expectedFixedNumberType(s, def.type)
				}
			}
		}
	}

	module(s TypeCheckerState, def ModuleDef) {
		if def.flags & ModuleFlags.typeCheckDone != 0 {
			return
		}
		def.flags |= ModuleFlags.typeCheckStarted
		s.module = def
		if !isValidEntityName_nonStatic(def.name.value) {
			invalidNamePrefix_nonStatic(s, def.name)
		}
		if def.blackboxKeyword != null && s.comp.flags & CompilationFlags.simulate != 0 && def.flags & ModuleFlags.builtin == 0 {
			s.errors.add(Error.at(s.unit, def.blackboxKeyword.span, "Cannot simulate blackbox module"))
		}
		for inp in def.inputs {
			moduleInput(s, inp)
		}
		block(s, def.body)
		def.flags |= ModuleFlags.typeCheckDone
	}

	ensureModuleDone(s TypeCheckerState, def ModuleDef, location Node) {
		if def.flags & ModuleFlags.typeCheckDone != 0 {
			// OK
		} else if def.flags & ModuleFlags.typeCheckStarted == 0 {
			prev := pushContext(s, def.unit, s.module)
			module(s, def)
			restoreContext(s, prev)
		} else {
			s.errors.add(Error.at(s.unit, RangeFinder.find(location), format("Recursive reference to module: {}", def.name.value)))
		}
	}

	moduleInput(s TypeCheckerState, input ModuleInputDef) {
		if s.module.flags & ModuleFlags.top != 0 {
			if input.flags & ModuleInputFlags.static != 0 {
				s.errors.add(Error.at(s.unit, input.name.span, "Top module input cannot be static"))
			}
		}
		if input.type != null {
			tag := typename(s, input, input.type)
			if tag.isValid() {
				if tag.kind == TagKind.number || s.module.blackboxKeyword == null {
					// OK
				} else {
					s.errors.add(Error.at(s.unit, input.type.span, "Blackbox module input must be of fixed numeric type"))
				}
				s.module.numInputSlots += numSlots(s.comp, tag)
			}
		}
	}

	block(s TypeCheckerState, st Block) {
		for n in st.contents {
			match n {
				FieldDef: fieldDef(s, n)
				ClockStatement: clock(s, n)
				IfStatement: if_(s, n)
				AssignStatement: assign(s, n)
				default: {}
			}
		}
	}

	clock(s TypeCheckerState, st ClockStatement) {
		if s.module.blackboxKeyword != null {
			statementNotAllowedInsideBlackbox(s, st.keyword)
		}

		if st.name != null {
			name := st.name.value
			if !s.module.symbols.containsKey(name) {
				badSymbol(s, st.name)
			}
		}
		if s.inClock {
			s.errors.add(Error.at(s.unit, st.keyword.span, "Clock statements may not be nested"))
		}
		s.inClock = true
		block(s, st.body)
		s.inClock = false
	}

	if_(s TypeCheckerState, st IfStatement) {
		if !s.inClock {
			s.errors.add(Error.at(s.unit, st.ifKeyword.span, "Statement must be placed inside clock statement"))
		}
		if s.module.blackboxKeyword != null {
			statementNotAllowedInsideBlackbox(s, st.ifKeyword)
		}

		cond := st.expr != null ? expression(s, st.expr) : TypeCheckResult{}
		if cond.tag.isValid() {
			if cond.tag.kind == TagKind.number && cond.tag.q == 1 {
				// OK
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(st.expr), "Expected: expression of type $1"))
			}
		}

		block(s, st.ifBody)
		if st.elseBranch.is(IfStatement) {
			if_(s, st.elseBranch.as(IfStatement))
		} else if st.elseBranch.is(Block) {
			block(s, st.elseBranch.as(Block))
		}
	}

	assign(s TypeCheckerState, st AssignStatement) {
		assignSym(s, st)
		if st.flags & AssignFlags.regUpdate != 0 {
			if s.inClock {
				// OK
			} else {
				s.errors.add(Error.at(s.unit, st.outKeyword != null ? st.outKeyword.span : RangeFinder.find(st.nameExpr), "Statement must be placed inside clock statement"))
			}
		}
	}

	assignSym(s TypeCheckerState, st AssignStatement) {
		if st.flags & AssignFlags.typeCheckDone != 0 {
			return
		}
		st.flags |= AssignFlags.typeCheckStarted
		if st.flags & AssignFlags.reg != 0 {
			reg(s, st)
		} else if st.flags & AssignFlags.wire != 0 {
			wire(s, st)
		} else if st.flags & AssignFlags.regUpdate != 0 {
			regUpdate(s, st)
		} else {
			abandon()
		}
		st.flags |= AssignFlags.typeCheckDone
	}

	ensureAssignDone(s TypeCheckerState, st AssignStatement, reference Node) {
		assert(st.flags & (AssignFlags.reg | AssignFlags.wire) != 0)
		if st.flags & AssignFlags.typeCheckDone != 0 {
			// OK
		} else if st.flags & AssignFlags.typeCheckStarted == 0 {
			prev := pushContext(s, st.module.unit, st.module)
			assignSym(s, st)
			restoreContext(s, prev)
		} else {
			kind := (st.flags & AssignFlags.reg != 0) ? "register" : "wire"
			s.errors.add(Error.at(s.unit, RangeFinder.find(reference), format("Type of {} cannot be inferred due to recursive reference, please specify the type explicity", kind)))
		}
	}

	reg(s TypeCheckerState, st AssignStatement) {
		if st.outKeyword != null {
			st.outputIndex = s.module.outputs.count
			s.module.outputs.add(st)
		}

		if s.module.blackboxKeyword != null {
			statementNotAllowedInsideBlackbox(s, st.regKeyword)
		}

		if st.nameExpr.is(Token) {
			if isValidEntityName_nonStatic(st.nameExpr.as(Token).value) {
				// OK
			} else {
				invalidNamePrefix_nonStatic(s, st.nameExpr)
			}
		} else if st.nameExpr != null {
			s.errors.add(Error.at(s.unit, RangeFinder.find(st.nameExpr), "Expected: identifier"))
		}

		if st.op != null {
			if st.op.value == "=" {
				if st.expr != null {
					// OK
				} else {
					s.errors.add(Error.atIndex(s.unit, st.op.span.to, "Expected: expression"))
				}
			} else if st.op.value == "<=" {
				st.flags |= AssignFlags.regUpdate
				if st.expr != null {
					// OK
				} else {
					s.errors.add(Error.atIndex(s.unit, st.op.span.to, "Expected: expression"))
				}
			} else {
				s.errors.add(Error.at(s.unit, st.op.span, "Expected: = or <="))
			}
		} else {
			if st.expr != null {
				s.errors.add(Error.at(s.unit, RangeFinder.find(st.expr), "Expected: newline"))
			}
		}

		if st.expr != null {
			if st.type != null {
				tag := typename(s, st, st.type)
				// Mark as done so reg can be referenced by expression. This is fine as we have just determined the type of the register.
				st.flags |= AssignFlags.typeCheckDone 
				s.isStaticExpr = st.flags & AssignFlags.regUpdate == 0
				rhs := expressionWithGap(s, st.expr, st.type != null, tag)
				s.isStaticExpr = false
				if st.op != null && tag.isValid() {
					if isFixedNumberOrStruct(tag) {
						if rhs.tag.isValid() {
							if canAssign_andUpgrade(s, rhs.tag, rhs.value, st.expr, tag) {
								s.module.numRegSlots += numSlots(s.comp, tag)
							} else {
								badAssign(s, st.op, rhs.tag, tag)
							}
						}
					} else {
						expectedFixedNumberOrStruct(s, st.type)
					}
				}
			} else {
				s.isStaticExpr = st.flags & AssignFlags.regUpdate == 0
				rhs := expression(s, st.expr)
				s.isStaticExpr = false
				if rhs.tag.isValid() {
					if isFixedNumberOrStruct(rhs.tag) {
						s.typeMap.add(st, rhs.tag)
						s.module.numRegSlots += numSlots(s.comp, rhs.tag)
					} else {
						expectedFixedNumberOrStructExpression(s, st.expr)
					}
				}
			}
		} else {
			if st.type != null {
				tag := typename(s, st, st.type)
				if tag.isValid() {
					if isFixedNumberOrStruct(tag) {
						s.module.numRegSlots += numSlots(s.comp, tag)
					} else {
						expectedFixedNumberOrStruct(s, st.type)
					}
				}
			} else {
				s.errors.add(Error.atIndex(s.unit, RangeFinder.find(st.nameExpr).to, "Expected: type"))
			}
		}
	}

	wire(s TypeCheckerState, st AssignStatement) {
		if st.outKeyword != null {
			st.outputIndex = s.module.outputs.count
			s.module.outputs.add(st)
		}

		if s.module.blackboxKeyword != null && st.outKeyword == null {
			s.errors.add(Error.atIndex(s.unit, RangeFinder.find(st.nameExpr).from, "Expected: out"))
		}

		if st.nameExpr.is(Token) {
			if st.outKeyword == null {
				if isValidEntityName(st.nameExpr.as(Token).value) {
					// OK
				} else {
					invalidNamePrefix(s, st.nameExpr)
				}
			} else {
				if isValidEntityName_nonStatic(st.nameExpr.as(Token).value) {
					// OK
				} else {
					invalidNamePrefix_nonStatic(s, st.nameExpr)
				}
			}
		} else if st.nameExpr != null {
			s.errors.add(Error.at(s.unit, RangeFinder.find(st.nameExpr), "Expected: identifier"))
		}

		if s.module.blackboxKeyword == null {
			if st.op != null {
				if st.op.value == ":=" {
					if st.expr != null {
						// OK
					} else {
						s.errors.add(Error.atIndex(s.unit, st.op.span.to, "Expected: expression"))
					}
				} else {
					s.errors.add(Error.at(s.unit, st.op.span, "Expected: :="))
				}
			} else {
				s.errors.add(Error.atIndex(s.unit, RangeFinder.find(st.type != null ? st.type : (st.nameExpr != null ? st.nameExpr : st.outKeyword)).to, "Expected: :="))
			}

			if st.expr != null {
				if st.type != null {
					tag := typename(s, st, st.type)
					// Mark as done so reg can be referenced by expression. This is fine as we have just determined the type of the wire.
					st.flags |= AssignFlags.typeCheckDone 
					s.isStaticExpr = st.flags & AssignFlags.static != 0
					rhs := expressionWithGap(s, st.expr, st.type != null, tag)
					s.isStaticExpr = false
					if st.op != null && tag.isValid() {
						if isFixedNumberOrStruct(tag) {
							if canAssign_andUpgrade(s, rhs.tag, rhs.value, st.expr, tag) {
								// OK
							} else {
								badAssign(s, st.op, rhs.tag, tag)
							}
						} else {
							expectedFixedNumberOrStruct(s, st.type)
						}
					}
				} else {
					s.isStaticExpr = st.flags & AssignFlags.static != 0
					rhs := expression(s, st.expr)
					s.isStaticExpr = false
					if rhs.tag.isValid() {
						if isFixedNumberOrStructOrModuleOut(rhs.tag) {
							s.typeMap.add(st, rhs.tag)
							if rhs.tag.kind == TagKind.moduleOut && st.outKeyword != null {
								s.errors.add(Error.at(s.unit, st.outKeyword.span, "out keyword can only be used with wire of fixed with numeric type or struct type"))
							}					
						} else {
							if st.flags & AssignFlags.static != 0 {
								expectedFixedNumberOrStructExpression(s, st.expr)
							} else {
								s.errors.add(Error.at(s.unit, RangeFinder.find(st.expr), "Expected: expression of fixed width numeric type or struct type or module"))
							}
						}
					}
				}
			}
		} else {
			if st.type != null {
				tag := typename(s, st, st.type)
				if tag.kind == TagKind.number {
					// OK
				} else {
					s.errors.add(Error.at(s.unit, st.type.span, "Blackbox module output must be of fixed numeric type"))
				}
			} else {
				s.errors.add(Error.atIndex(s.unit, RangeFinder.find(st.nameExpr != null ? st.nameExpr : st.outKeyword).to, "Expected: type"))
			}

			if st.op != null {
				if st.expr != null {
					s.errors.add(Error.at(s.unit, IntRange(st.op.span.from, RangeFinder.find(st.expr).to), "Cannot assign expression to blackbox output"))
				} else {
					s.errors.add(Error.at(s.unit, st.op.span, "Cannot assign expression to blackbox output"))
				}
			} else if st.expr != null {
				s.errors.add(Error.at(s.unit, RangeFinder.find(st.expr), "Cannot assign expression to blackbox output"))
			}
		}
	}

	regUpdate(s TypeCheckerState, st AssignStatement) {
		if s.module.blackboxKeyword != null {
			statementNotAllowedInsideBlackbox(s, st.outKeyword != null ? st.outKeyword : st.nameExpr)
		}

		if st.outKeyword != null {
			s.errors.add(Error.at(s.unit, st.outKeyword.span, "out keyword can only be used with register or wire declaration"))
		}

		if st.type != null {
			s.errors.add(Error.at(s.unit, st.type.span, "Expected: <="))
		}

		if st.expr == null {
			s.errors.add(Error.atIndex(s.unit, st.op.span.to, "Expected: expression"))
		}

		nameExpr := st.nameExpr
		match nameExpr {
			Token: {
				nameToken := nameExpr
				tag := Tag{}			
				sym := s.module.symbols.getOrDefault(nameToken.value)
				if sym != null {
					if sym.is(AssignStatement) && (sym.as(AssignStatement).flags & AssignFlags.reg) != 0 {
						reg := sym.as(AssignStatement)
						st.localId = reg.localId
						ensureAssignDone(s, reg, nameToken)
						tag = s.typeMap.getOrDefault(reg)
					} else {
						s.errors.add(Error.at(s.unit, nameToken.span, "Expected: register"))
					}
				} else {
					badSymbol(s, nameToken)
				}
				if st.expr != null {
					rhs := expressionWithGap(s, st.expr, true, tag)
					if canAssign_andUpgrade(s, rhs.tag, rhs.value, st.expr, tag) {
						// OK
					} else {
						badAssign(s, st.op, rhs.tag, tag)
					}
				}
			}
			DotExpression: {		
				dot := nameExpr		
				if dot.lhs.is(Token) {
					tag := Tag{}
					nameToken := dot.lhs.as(Token)
					sym := s.module.symbols.getOrDefault(nameToken.value)
					if sym != null {
						if sym.is(AssignStatement) && (sym.as(AssignStatement).flags & AssignFlags.reg) != 0 {
							reg := sym.as(AssignStatement)
							st.localId = reg.localId
							ensureAssignDone(s, reg, nameToken)
							regTag := s.typeMap.getOrDefault(reg)
							if dot.rhs != null && regTag.isValid() {
								if regTag.kind == TagKind.struct_ {
									def := s.comp.entities[regTag.q].as(StructDef)
									ensureStructDone(s, def)
									field := def.symbols.getOrDefault(dot.rhs.value)
									if field != null {
										tag = s.typeMap.getOrDefault(field)
										st.lhsFieldIndex = field.fieldIndex
									} else {
										s.errors.add(Error.at(s.unit, dot.rhs.span, format("Undefined field: {}.{}", def.name.value, dot.rhs.value)))
									}
								} else {
									s.errors.add(Error.at(s.unit, nameToken.span, "Expected: register of type struct"))
								}
							}
						} else {
							s.errors.add(Error.at(s.unit, nameToken.span, "Expected: register"))
						}
					} else {
						badSymbol(s, nameToken)
					}
					if st.expr != null {
						rhs := expressionWithGap(s, st.expr, true, tag)
						if canAssign_andUpgrade(s, rhs.tag, rhs.value, st.expr, tag) {
							// OK
						} else {
							badAssign(s, st.op, rhs.tag, tag)
						}
					}
				} else {
					unsupportedRegUpdate(s, dot)
				}
			}
			CallExpression: {
				call := nameExpr
				if call.builtin == BuiltinCall.slice {
					assignSlice(s, st, call)
				} else {
					unsupportedRegUpdate(s, call)
				}
			}
			default: {
				unsupportedRegUpdate(s, nameExpr)
			}
			null: {
			}
		}
	}

	expression(s TypeCheckerState, e Node) TypeCheckResult {
		prevGap := s.gap
		s.gap = false
		tr := expressionContinueGap(s, e)
		s.gap = prevGap
		return tr
	}

	expressionWithGap(s TypeCheckerState, e Node, gap bool, gapTag Tag) TypeCheckResult {
		s.gap = gap
		s.gapTag = gapTag
		tr := expressionContinueGap(s, e)
		s.gap = false
		return tr
	}

	expressionContinueGap(s TypeCheckerState, e Node) TypeCheckResult {
		tr := expressionInner(s, e)
		if tr.tag.kind != TagKind.unknown {
			s.typeMap.add(e, tr.tag)
		}
		if tr.value.kind != ValueKind.none {
			s.constMap.add(e, tr.value)
		}
		return tr
	}

	expressionInner(s TypeCheckerState, e Node) TypeCheckResult {
		match e {
			Token: return token(s, e)
			NumberExpression: return number(s, e)
			UnaryOperatorExpression: return unaryOperator(s, e)
			BinaryOperatorExpression: return binaryOperator(s, e)
			DotExpression: return dot(s, e)
			TernaryOperatorExpression: return ternaryOperator(s, e)
			MatchExpression: return match_(s, e)
			ParenExpression: return paren(s, e)
			IndexExpression: return index(s, e)
			BraceExpression: return brace(s, e)
			CallExpression: return call(s, e)
			StructInitializerExpression: return structInit(s, e)
			ArrayExpression: return array(s, e)
		}
	}

	token(s TypeCheckerState, e Token) {
		sym := s.module != null ? s.module.symbols.getOrDefault(e.value) : null
		match sym {
			ModuleInputDef: {
				if !s.isStaticExpr || sym.flags & ModuleInputFlags.static != 0 {
					return TypeCheckResult { tag: s.typeMap.getOrDefault(sym) }
				} else {
					expectedStaticConstant(s, e)
					return TypeCheckResult{}
				}
			}
			AssignStatement: {
				if !s.isStaticExpr || sym.flags & AssignFlags.static != 0 {
					ensureAssignDone(s, sym, e)
					return TypeCheckResult { tag: s.typeMap.getOrDefault(sym) }
				} else {
					expectedStaticConstant(s, e)
					return TypeCheckResult{}
				}
			}			
			default | null: {
				sym = s.symbols.getOrDefault(e.value)
				match sym {
					ConstDef: {
						ensureConstDone(s, sym, e)
						return TypeCheckResult { tag: s.typeMap.getOrDefault(sym), value: s.constMap.getOrDefault(sym.expr) }
					}
					default: s.errors.add(Error.at(s.unit, e.span, "Expected: register, wire or constant"))
					null: badSymbol(s, e)
				}
			}
		}
		return TypeCheckResult{}
	}

	number(s TypeCheckerState, e NumberExpression) {
		if e.flags & NumberFlags.valid != 0 {
			if e.flags & NumberFlags.dontCare == 0 {
				return TypeCheckResult { tag: Tag { kind: TagKind.number, q: e.width }, value: Value { kind: ValueKind.ulong_, z: e.value } }
			} else if s.allowDontCare {
				// Note: don't return a value, so generator cannot check const map and will look at the NumberExpression to correctly generate don't cares.
				if e.width > 0 {
					return TypeCheckResult { tag: Tag { kind: TagKind.number, q: e.width } }
				} else {
					// ---
					assert(e.value == 0)
					if s.gap && s.gapTag.kind == TagKind.number && s.gapTag.q > 0 {
						return TypeCheckResult { tag: Tag { kind: TagKind.number, q: s.gapTag.q } }
					} else {
						s.errors.add(Error.at(s.unit, e.token.span, "Don't care values are not allowed here"))
					}
				}
			} else {
				s.errors.add(Error.at(s.unit, e.token.span, "Don't care values are not allowed here"))
			}
		}
		return TypeCheckResult{}
	}

	unaryOperator(s TypeCheckerState, e UnaryOperatorExpression) {
		result := TypeCheckResult{}
		arg := e.expr != null ? expression(s, e.expr) : TypeCheckResult{}
		valid := arg.tag.isValid()
		op := e.op.value
		if op == "-" || op == "~" || op == "zx" {
			if valid {
				if arg.tag.kind == TagKind.number {
					if op == "-" || op == "~" {
						if arg.tag.q <= 64 {
							result.tag = arg.tag
						} else {
							unsupportedUnaryOp(s, e.op, arg.tag)
						}
						if result.tag.isValid() && arg.value.kind == ValueKind.ulong_ {
							if op == "-" {
								result.value = Value { kind: ValueKind.ulong_, z: -arg.value.z }
							} else if op == "~" {
								result.value = Value { kind: ValueKind.ulong_, z: ~arg.value.z }
							}
							if result.tag.q > 0 && result.tag.q < 64 {
								result.value.z &= (1_uL << result.tag.q) - 1
							}
						}
					} else if op == "zx" {
						if arg.tag.q > 0 {
							if s.gap {
								if s.gapTag.isValid() {
									if s.gapTag.kind == TagKind.number && s.gapTag.q > arg.tag.q {
										result.tag = s.gapTag
									} else {
										badGapArgument(s, e.op, arg.tag, s.gapTag)
									}
								}
							} else {
								badGap(s, e.op)
							}
						} else {
							badUnaryOp(s, e.op, arg.tag)
						}
						if result.tag.isValid() && arg.value.kind != ValueKind.none {
							result.value = arg.value
						}
					}
				} else {
					badUnaryOp(s, e.op, arg.tag)
				}
			}
		} else {
			s.errors.add(Error.at(s.unit, e.op.span, format("Invalid unary operator: {}", op)))
		}
		return result
	}

	binaryOperator(s TypeCheckerState, e BinaryOperatorExpression) {
		result := TypeCheckResult{}
		lhs := expression(s, e.lhs)
		rhs := e.rhs != null ? expression(s, e.rhs) : TypeCheckResult{}
		valid := lhs.tag.isValid() && rhs.tag.isValid()
		op := e.op.value
		if op == "+" || op == "-" || op == "&" || op == "|" || op == "^" {
			if valid {
				if lhs.tag.kind == TagKind.number && rhs.tag.kind == TagKind.number {
					if lhs.tag.q > 64 || rhs.tag.q > 64 {
						unsupportedBinaryOp(s, e.op, lhs.tag, rhs.tag)
					} else if lhs.tag.q > 0 || rhs.tag.q > 0 {
						if lhs.tag.q == 0 {
							if canConvertFreeConst_andUpgrade(s, lhs.value, e.lhs, rhs.tag) {
								result.tag = Tag { kind: TagKind.number, q: rhs.tag.q }
							} else {
								badBinaryOperandConversion(s, RangeFinder.find(e.lhs), rhs.tag)
							}
						} else if rhs.tag.q == 0 {
							if canConvertFreeConst_andUpgrade(s, rhs.value, e.rhs, lhs.tag) {
								result.tag = Tag { kind: TagKind.number, q: lhs.tag.q }
							} else {
								badBinaryOperandConversion(s, RangeFinder.find(e.rhs), lhs.tag)
							}
						} else {
							if lhs.tag.q == rhs.tag.q {
								result.tag = Tag { kind: TagKind.number, q: lhs.tag.q }
							} else {
								badBinaryOp(s, e.op, lhs.tag, rhs.tag)
							}
						}
					} else {
						result.tag = Tag { kind: TagKind.number }
					}
					if result.tag.isValid() && lhs.value.kind == ValueKind.ulong_ && rhs.value.kind == ValueKind.ulong_ {
						if op == "+" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z + rhs.value.z }
						} else if op == "-" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z - rhs.value.z }
						} else if op == "&" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z & rhs.value.z }
						} else if op == "|" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z | rhs.value.z }
						} else if op == "|" {
							result.value = Value { kind: ValueKind.ulong_, z: xor(lhs.value.z, rhs.value.z) }
						}
						if result.tag.q > 0 && result.tag.q < 64 {
							result.value.z &= ((1_uL << result.tag.q) - 1)
						}
					}
				} else {
					badBinaryOp(s, e.op, lhs.tag, rhs.tag)
				}
			}
		} else if op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=" {
			if valid {
				if lhs.tag.kind == TagKind.number && rhs.tag.kind == TagKind.number {										
					if lhs.tag.q > 64 || rhs.tag.q > 64 {
						unsupportedBinaryOp(s, e.op, lhs.tag, rhs.tag)
					} else if lhs.tag.q > 0 || rhs.tag.q > 0 {
						if lhs.tag.q == 0 {
							if canConvertFreeConst_andUpgrade(s, lhs.value, e.lhs, rhs.tag) {
								result.tag = Tag { kind: TagKind.number, q: 1 }
							} else {
								badBinaryOperandConversion(s, RangeFinder.find(e.lhs), rhs.tag)
							}
						} else if rhs.tag.q == 0 {
							if canConvertFreeConst_andUpgrade(s, rhs.value, e.rhs, lhs.tag) {
								result.tag = Tag { kind: TagKind.number, q: 1 }
							} else {
								badBinaryOperandConversion(s, RangeFinder.find(e.rhs), lhs.tag)
							}
						} else {
							if lhs.tag.q == rhs.tag.q {
								result.tag = Tag { kind: TagKind.number, q: 1 }
							} else {
								badBinaryOp(s, e.op, lhs.tag, rhs.tag)
							}
						}
					} else {
						result.tag = Tag { kind: TagKind.number, q: 1 }
					}
					if result.tag.isValid() && lhs.value.kind == ValueKind.ulong_ && rhs.value.kind == ValueKind.ulong_ {
						if op == "==" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z == rhs.value.z ? 1_uL : 0 }
						} else if op == "!=" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z != rhs.value.z ? 1_uL : 0 }
						} else if op == "<" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z < rhs.value.z ? 1_uL : 0 }
						} else if op == "<=" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z <= rhs.value.z ? 1_uL : 0 }
						} else if op == ">" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z > rhs.value.z ? 1_uL : 0 }
						} else if op == ">=" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z >= rhs.value.z ? 1_uL : 0 }
						}
					}
				} else {
					badBinaryOp(s, e.op, lhs.tag, rhs.tag)
				}
			}
		} else if op == "*" {
			if valid {
				if lhs.tag.kind == TagKind.number && rhs.tag.kind == TagKind.number {										
					if lhs.tag.q > 64 || rhs.tag.q > 64 {
						unsupportedBinaryOp(s, e.op, lhs.tag, rhs.tag)
					} else {
						result.tag = Tag { kind: TagKind.number }
					}
					if result.tag.isValid() && lhs.value.kind == ValueKind.ulong_ && rhs.value.kind == ValueKind.ulong_ {
						result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z * rhs.value.z }
					}
				} else {
					badBinaryOp(s, e.op, lhs.tag, rhs.tag)
				}
			}
		} else if op == "<<" || op == ">>" {
			if valid {
				if lhs.tag.kind == TagKind.number && rhs.tag.kind == TagKind.number {										
					if lhs.tag.q > 64 || rhs.tag.q > 64 {
						unsupportedBinaryOp(s, e.op, lhs.tag, rhs.tag)
					} else {
						result.tag = lhs.tag
					}
					if result.tag.isValid() && lhs.value.kind == ValueKind.ulong_ && rhs.value.kind == ValueKind.ulong_ {
						if op == "<<" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z << rhs.value.z }
						} else if op == ">>" {
							result.value = Value { kind: ValueKind.ulong_, z: lhs.value.z >> rhs.value.z }
						}
						if result.tag.q > 0 && result.tag.q < 64 {
							result.value.z &= (1_uL << result.tag.q) - 1
						}
					}
				} else {
					badBinaryOp(s, e.op, lhs.tag, rhs.tag)
				}
			}
		} else {
			s.errors.add(Error.at(s.unit, e.op.span, format("Invalid binary operator: {}", op)))
		}
		return result
	}

	ternaryOperator(s TypeCheckerState, e TernaryOperatorExpression) {
		result := TypeCheckResult{}
		cond := expression(s, e.conditionExpr)
		if cond.tag.isValid() {
			if cond.tag.kind == TagKind.number && cond.tag.q == 1 {
				// OK
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e.conditionExpr), "Expected: expression of type $1"))
			}
		}
		te := e.trueExpr != null ? expressionContinueGap(s, e.trueExpr) : TypeCheckResult{}
		fe := e.falseExpr != null ? expressionContinueGap(s, e.falseExpr) : TypeCheckResult{}
		if s.gap && s.gapTag.kind == TagKind.number && s.gapTag.q > 0 {
			if te.tag.isValid() {
				if canAssign_andUpgrade(s, te.tag, te.value, e.trueExpr, s.gapTag) {
					// OK
				} else {
					badConversion(s, e.trueExpr, te.tag, s.gapTag)
				}
			}
			if fe.tag.isValid() {
				if canAssign_andUpgrade(s, fe.tag, fe.value, e.falseExpr, s.gapTag) {
					// OK
				} else {
					badConversion(s, e.falseExpr, fe.tag, s.gapTag)
				}
			}
			result.tag = s.gapTag
		} else {
			if te.tag.isValid() && fe.tag.isValid() {
				if te.tag.kind == TagKind.number && fe.tag.kind == TagKind.number {										
					if te.tag.q > 0 || fe.tag.q > 0 {
						if te.tag.q == 0 {
							if canConvertFreeConst_andUpgrade(s, te.value, e.trueExpr, fe.tag) {
								result.tag = Tag { kind: TagKind.number, q: fe.tag.q }
							} else {
								badBinaryOperandConversion(s, RangeFinder.find(e.trueExpr), fe.tag)
							}
						} else if fe.tag.q == 0 {
							if canConvertFreeConst_andUpgrade(s, fe.value, e.falseExpr, te.tag) {
								result.tag = Tag { kind: TagKind.number, q: te.tag.q }
							} else {
								badBinaryOperandConversion(s, RangeFinder.find(e.falseExpr), te.tag)
							}
						} else {
							if te.tag.q == fe.tag.q {
								result.tag = Tag { kind: TagKind.number, q: te.tag.q }
							} else {
								badUnify(s, e.question, te.tag, fe.tag)
							}
						}
					} else {
						result.tag = Tag { kind: TagKind.number }
					}
				} else {
					badUnify(s, e.question, te.tag, fe.tag)
				}
			}
		}
		return result
	}

	dot(s TypeCheckerState, e DotExpression) {
		result := TypeCheckResult{}
		lhs := expression(s, e.lhs)
		if lhs.tag.isValid() {
			if lhs.tag.kind == TagKind.moduleOut {
				def := s.comp.entities[lhs.tag.q].as(ModuleDef)
				assert(def.flags & ModuleFlags.typeCheckStarted != 0)
				if e.rhs != null {
					output := def.symbols.getOrDefault(e.rhs.value)
					if output.is(AssignStatement) && output.as(AssignStatement).outKeyword != null {
						result.tag = s.typeMap.getOrDefault(output)
					} else {
						s.errors.add(Error.at(s.unit, e.rhs.span, format("Undefined output: {}.{}", def.name.value, e.rhs.value)))
					}
				}
			} else if lhs.tag.kind == TagKind.struct_ {
				def := s.comp.entities[lhs.tag.q].as(StructDef)
				ensureStructDone(s, def)
				if e.rhs != null {
					field := def.symbols.getOrDefault(e.rhs.value)
					if field != null {
						result.tag = s.typeMap.getOrDefault(field)
					} else {
						s.errors.add(Error.at(s.unit, e.rhs.span, format("Undefined field: {}.{}", def.name.value, e.rhs.value)))
					}
				}
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e.lhs), "Expected: expression of type $module_out"))
			}
		}
		return result
	}

	match_(s TypeCheckerState, e MatchExpression) {
		target := TypeCheckResult{}
		
		condValid := false
		if e.target != null {
			target = expression(s, e.target)
			if target.tag.isValid() {
				if target.tag.kind == TagKind.number && target.tag.q > 0 {
					condValid = true
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "Expected: expression of fixed width numeric type"))
				}
			}
		}

		result := TypeCheckResult{}
		for c in e.cases {
			vex := expression(s, c.valueExpr)
			if vex.tag.isValid() {
				if vex.tag.kind == TagKind.number && vex.value.kind == ValueKind.ulong_ {
					if condValid {
						if canAssign_andUpgrade(s, vex.tag, vex.value, c.valueExpr, target.tag) {
							// OK
						} else {
							badConstConversion(s, c.valueExpr, vex.tag, target.tag)
						}
					}
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(c.valueExpr), "Expected: numeric constant"))
				}
			}
			if c.resultExpr != null {
				rex := expressionContinueGap(s, c.resultExpr)
				if rex.tag.isValid() {
					if result.tag.isValid() {
						if rex.tag.kind == TagKind.number && rex.tag.q == result.tag.q {
							// OK
						} else {
							s.errors.add(Error.at(s.unit, RangeFinder.find(c.resultExpr), "Must match type of previous case"))
						}
					} else {
						if rex.tag.kind == TagKind.number && rex.tag.q > 0 {
							result.tag = rex.tag
						} else {
							s.errors.add(Error.at(s.unit, RangeFinder.find(c.resultExpr), "Expected: expression of fixed width numeric type"))
						}
					}
				}
			}
		}

		return result
	}

	paren(s TypeCheckerState, e ParenExpression) {
		return e.expr != null ? expression(s, e.expr) : TypeCheckResult{}
	}

	index(s TypeCheckerState, e IndexExpression) {
		result := TypeCheckResult{}

		target := expression(s, e.target)
		targetValid := false
		if target.tag.isValid() {
			if target.tag.kind == TagKind.number && target.tag.q > 0 {
				if target.tag.q <= 64 {
					targetValid = true
				} else {
					s.errors.add(Error.at(s.unit, e.openBracket.span, format("Unsupported expression: cannot index argument of type {}", tagString(s.comp, target.tag))))
				}
			} else {
				s.errors.add(Error.at(s.unit, e.openBracket.span, format("Cannot index argument of type {}", tagString(s.comp, target.tag))))
			}
		}

		if e.upperExpr != null {
			upper := expression(s, e.upperExpr)
			upperValid := false
			if upper.tag.isValid() {
				if upper.tag.kind == TagKind.number && upper.value.kind == ValueKind.ulong_ {
					if targetValid {
						if upper.value.z < cast(target.tag.q, ulong) {
							upperValid = true
						} else {
							s.errors.add(Error.at(s.unit, RangeFinder.find(e.upperExpr), "Index out of range"))
						}
					}
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(e.upperExpr), "Expected: numeric constant"))
				}
			}

			if e.lowerExpr != null {
				lower := expression(s, e.lowerExpr)
				if lower.tag.isValid() {
					if lower.tag.kind == TagKind.number && lower.value.kind == ValueKind.ulong_ {
						if targetValid {
							if lower.value.z < cast(target.tag.q, ulong) {
								if upperValid {
									if lower.value.z <= upper.value.z {
										result.tag = Tag { kind: TagKind.number, q: cast(upper.value.z, int) - cast(lower.value.z, int) + 1 }
									} else {
										s.errors.add(Error.at(s.unit, RangeFinder.find(e.lowerExpr), "Index out of range: must be lower than first value"))
									}
								}
							} else {
								s.errors.add(Error.at(s.unit, RangeFinder.find(e.lowerExpr), "Index out of range"))
							}
						}
					} else {
						s.errors.add(Error.at(s.unit, RangeFinder.find(e.lowerExpr), "Expected: numeric constant"))
					}
				}
			} else {
				result.tag = Tag { kind: TagKind.number, q: 1 }
			}
		}

		return result
	}

	brace(s TypeCheckerState, e BraceExpression) {
		allValid := true
		bits := 0

		for arg in e.args {
			prevAllowDontCare := s.allowDontCare
			s.allowDontCare = arg.is(NumberExpression)
			atr := expression(s, arg)
			s.allowDontCare = prevAllowDontCare
			if atr.tag.isValid() {
				if atr.tag.kind == TagKind.number && atr.tag.q > 0 {
					bits += atr.tag.q
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(arg), "Expected: expression of fixed width numeric type"))
				}
			} else {
				allValid = false
			}
		}
		
		if bits > 64 {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Unsupported expression; type of expression cannot be larger than $64"))
		}

		return allValid ? TypeCheckResult { tag: Tag { kind: TagKind.number, q: bits } } : TypeCheckResult{}
	}

	call(s TypeCheckerState, e CallExpression) {
		if e.builtin == BuiltinCall.rep {
			return rep(s, e)
		} else if e.builtin == BuiltinCall.cast_ {
			return cast_(s, e)
		} else if e.builtin == BuiltinCall.slice {
			return slice(s, e)
		} else if e.builtin == BuiltinCall.chunk {
			return chunk(s, e)
		} else if e.builtin == BuiltinCall.swizzle {
			return swizzle(s, e)
		}

		if s.isStaticExpr {
			expectedStaticConstant(s, e)
			return TypeCheckResult{}
		}

		target := cast(null, Token)
		def := cast(null, ModuleDef)
		if e.target.is(Token) && e.target.as(Token).type == TokenType.identifier {
			target = e.target.as(Token)
			node := s.symbols.getOrDefault(target.value)
			if node.is(ModuleDef) {
				def = node.as(ModuleDef)
				s.module.numCalls += 1
				e.callId = s.module.numCalls
				ensureModuleDone(s, def, target)
				s.module.numRegSlots += def.numRegSlots
			} else if node != null {
				s.errors.add(Error.at(s.unit, target.span, "Expected: module name"))
			} else {
				badSymbol(s, target)
			}
		} else {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "Expected: module or function name"))
		}

		s.queue.add(QueuedCallArgs { e: e, def: def, context: s.module })

		return def != null ? TypeCheckResult { tag: Tag { kind: TagKind.moduleOut, q: def.id } } : TypeCheckResult{}
	}

	callArgs(s TypeCheckerState, e CallExpression, def ModuleDef) {
		if def != null {
			e.calleeLocalIdToArgIndex = new Array<int>(def.inputs.count + 1)
			for i := 0; i < e.calleeLocalIdToArgIndex.count {
				e.calleeLocalIdToArgIndex[i] = -1
			}

			for arg, i in e.args {
				tag := Tag{}
				isStatic := false
				if arg.name != null {
					sym := def.symbols.getOrDefault(arg.name.value)
					if sym != null && sym.is(ModuleInputDef) {
						input := sym.as(ModuleInputDef)
						isStatic = input.flags & ModuleInputFlags.static != 0
						if e.calleeLocalIdToArgIndex[input.localId] == -1 {
							e.calleeLocalIdToArgIndex[input.localId] = i
						} else {
							s.errors.add(Error.at(s.unit, arg.name.span, "Input has already been specified"))	
						}
						tag = s.typeMap.getOrDefault(input)
					} else {
						s.errors.add(Error.at(s.unit, arg.name.span, format("Undefined input: {}.{}", def.name.value, arg.name.value)))
					}
				}

				if arg.expr != null {
					s.allowDontCare = arg.expr.is(NumberExpression) || arg.expr.is(BraceExpression) || (arg.expr.is(CallExpression) && arg.expr.as(CallExpression).builtin == BuiltinCall.rep)
					s.isStaticExpr = isStatic
					rhs := expressionWithGap(s, arg.expr, true, tag)
					s.allowDontCare = false
					s.isStaticExpr = false
					if !canAssign_andUpgrade(s, rhs.tag, rhs.value, arg.expr, tag) {
						badAssign(s, arg.colon, rhs.tag, tag)
					}
					if arg.name == null {
						s.errors.add(Error.atIndex(s.unit, RangeFinder.find(arg.expr).from, "Expected: parameter name"))
					}
				}
			}

			missing := List<string>{}
			for i := 0; i < def.inputs.count {
				if e.calleeLocalIdToArgIndex[i + 1] == -1 {
					missing.add(def.inputs[i].name.value)
				}
			}

			if missing.count > 0 {
				names := string.join(", ", ref missing.slice(0, missing.count))
				s.errors.add(Error.atIndex(s.unit, e.openParen.span.from, format("Missing required parameters: {}", names)))
			}
		} else {
			for arg in e.args {
				if arg.expr != null {
					expression(s, arg.expr)
				}
			}
		}
	}

	structInit(s TypeCheckerState, e StructInitializerExpression) {
		target := cast(null, Token)
		def := cast(null, StructDef)
		if e.target.is(Token) && e.target.as(Token).type == TokenType.identifier {
			target = e.target.as(Token)
			node := s.symbols.getOrDefault(target.value)
			if node.is(StructDef) {
				def = node.as(StructDef)
				ensureStructDone(s, def)
			} else if node != null {
				s.errors.add(Error.at(s.unit, target.span, "Expected: struct type"))
			} else {
				badSymbol(s, target)
			}
		} else {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "Expected: struct type"))
		}

		if def != null {
			e.fieldIndexToArgIndex = new Array<int>(def.fields.count)
			for i := 0; i < def.fields.count {
				e.fieldIndexToArgIndex[i] = -1
			}
		
			for arg, i in e.args {			
				tag := Tag{}
				if arg.name != null {
					field := def.symbols.getOrDefault(arg.name.value)
					if field != null {						
						if e.fieldIndexToArgIndex[field.fieldIndex] == -1 {
							e.fieldIndexToArgIndex[field.fieldIndex] = i
						} else {
							s.errors.add(Error.at(s.unit, arg.name.span, "Field has already been specified"))
						}
						tag = s.typeMap.getOrDefault(field)
					} else {
						s.errors.add(Error.at(s.unit, arg.name.span, format("Undefined field: {}.{}", def.name.value, arg.name.value)))
					}
				}

				if arg.expr != null {
					rhs := expressionWithGap(s, arg.expr, true, tag)
					if !canAssign_andUpgrade(s, rhs.tag, rhs.value, arg.expr, tag) {
						badAssign(s, arg.colon, rhs.tag, tag)
					}
					if arg.name == null {
						s.errors.add(Error.atIndex(s.unit, RangeFinder.find(arg.expr).from, "Expected: field name"))
					}
				}
			}
			
			return TypeCheckResult { tag: Tag { kind: TagKind.struct_, q: def.id } }
		} else {
			for arg in e.args {
				if arg.expr != null {
					expression(s, arg.expr)
				}
			}
			return TypeCheckResult{}
		}
	}
	
	array(s TypeCheckerState, e ArrayExpression) {
		data := new e.data.slice(0, e.data.count)
		width := data.count * 8
		return TypeCheckResult { 
			tag: Tag { kind: TagKind.number, q: width },
			value: width > 64 ? EmulatorRunner.packArray(data) : EmulatorRunner.packULong(data)
		}
	}

	typename(s TypeCheckerState, node Node, name Token) {
		tag := tag(s, name.value)
		if tag.isValid() {
			s.typeMap.add(node, tag)
		} else {
			s.errors.add(Error.at(s.unit, name.span, format("Undefined type: {}", name.value)))
		}
		return tag
	}

	pushContext(s TypeCheckerState, unit CodeUnit, def ModuleDef) {
		result := TypeCheckerContext {
			unit: s.unit,
			module: s.module,
			inClock: s.inClock,
			isStaticExpr: s.isStaticExpr,
			allowDontCare: s.allowDontCare,
		}
		s.unit = unit
		s.module = def
		s.inClock = false
		s.isStaticExpr = false
		s.allowDontCare = false
		return result
	}

	restoreContext(s TypeCheckerState, ctx TypeCheckerContext) {
		s.unit = ctx.unit
		s.module = ctx.module
		s.inClock = ctx.inClock
		s.isStaticExpr = ctx.isStaticExpr
		s.allowDontCare = ctx.allowDontCare
	}
}
