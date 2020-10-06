TypeChecker {
	rep(s TypeCheckerState, e CallExpression) {
		builtinArgs(s, e, 2)
		target := fixedNumberArg(s, e, 0)
		nArg := constantArg(s, e, 1)
		result := TypeCheckResult{}
		if nArg.tag.isValid() {
			n := tryUnpackInt(nArg.value)
			if n.hasValue && 0 < n.value {
				if target.tag.isValid() {
					w := target.tag.q * n.value
					if w <= 64 {
						result.tag = Tag { kind: TagKind.number, q: w }
						if target.value.kind == ValueKind.ulong_ {
							result.value = Value { kind: ValueKind.ulong_, z: 0 }
							for i := 0; i < n.value {
								result.value.z <<= target.tag.q
								result.value.z |= target.value.z
							}
						}
					} else {
						s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Unsupported expression; type of expression cannot be larger than $64"))
					}
				}
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[1].expr), "Expected: constant, > 0"))
			}
		}
		return result
	}

	cast_(s TypeCheckerState, e CallExpression) {
		if s.isStaticExpr {
			// OK
		} else if s.comp.flags & CompilationFlags.simulate == 0 {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "cast() can only be used in static initializer or in simulator"))
		}
		builtinArgs(s, e, 2)
		target := numberArg(s, e, 0)
		typeArg := fixedNumericTypeArg(s, e, 1)
		result := TypeCheckResult{}
		if target.tag.isValid() && typeArg.tag.isValid() {
			result.tag = typeArg.tag
			if typeArg.tag.q >= target.tag.q || typeArg.tag.q <= 64 {
				// OK
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Unsupported cast; downcast not allowed for expressions larger than $64"))
			}
			if typeArg.tag.q >= target.tag.q && target.tag.q > 0 {
				result.value = target.value
			}
		}
		return result
	}

	slice(s TypeCheckerState, e CallExpression) {
		if s.module == null {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "Cannot use slice() in constant initializer"))
		}
		if s.isStaticExpr {
			// OK
		} else if s.comp.flags & CompilationFlags.simulate == 0 {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "slice() can only be used in static initializer or in simulator"))
		}
		builtinArgs(s, e, 3)
		fixedNumberArg(s, e, 0)
		numberArg(s, e, 1)
		widthArg := constantArg(s, e, 2)
		result := TypeCheckResult{}
		if widthArg.tag.isValid() {
			width := tryUnpackInt(widthArg.value)
			if s.isStaticExpr {
				if width.hasValue && 0 < width.value {
					result.tag = Tag { kind: TagKind.number, q: width.value }
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[2].expr), "Expected: constant, > 0"))
				}			
			} else {
				if width.hasValue && 1 <= width.value && width.value <= 64 {
					result.tag = Tag { kind: TagKind.number, q: width.value }
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[2].expr), "Expected: constant between 1 and 64 inclusive"))
				}			
			}
		}
		return result
	}

	assignSlice(s TypeCheckerState, st AssignStatement, e CallExpression) {
		if s.comp.flags & CompilationFlags.simulate == 0 {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "slice() can only be used in static initializer or in simulator"))
		}
		builtinArgs(s, e, 3)
		if e.args.count >= 1 {
			regArg := e.args[0].expr
			if regArg.is(Token) {
				nameToken := regArg.as(Token)
				name := nameToken.value
				sym := s.module.symbols.getOrDefault(nameToken.value)
				if sym != null {
					if sym.is(AssignStatement) && (sym.as(AssignStatement).flags & AssignFlags.reg) != 0 {
						reg := sym.as(AssignStatement)
						st.localId = reg.localId
						ensureAssignDone(s, reg, nameToken)
						regTag := s.typeMap.getOrDefault(reg)
						if regTag.kind == TagKind.number && regTag.q > 0 {
							// OK
						} else {
							expectedFixedNumberExpression(s, regArg)
						}
					} else {
						s.errors.add(Error.at(s.unit, nameToken.span, "Expected: register"))
					}
				} else {
					badSymbol(s, nameToken)
				}
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(regArg), "Expected: register"))
			}
		}	
		numberArg(s, e, 1)
		widthArg := constantArg(s, e, 2)
		tag := Tag{}
		if widthArg.tag.isValid() {
			width := tryUnpackInt(widthArg.value)
			if width.hasValue && 1 <= width.value && width.value <= 64 {
				tag = Tag { kind: TagKind.number, q: width.value }
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[2].expr), "Expected: constant between 1 and 64 inclusive"))
				return TypeCheckResult{}
			}			
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

	chunk(s TypeCheckerState, e CallExpression) {
		if s.module == null {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "Unsupported expression: cannot use chunk() in constant initializer"))
		}
		if !s.isStaticExpr {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "chunk() can only be used in static initializer"))
		}
		builtinArgs(s, e, 3)
		arrayArg := fixedNumberArg(s, e, 0)
		indexArg := constantArg(s, e, 1)
		numChunksArg := constantArg(s, e, 2)
		result := TypeCheckResult{}
		if arrayArg.tag.isValid() && indexArg.tag.isValid() && numChunksArg.tag.isValid() {
			numBits := unpackWidth(arrayArg.tag)
			index := tryUnpackInt(indexArg.value)
			numChunks := tryUnpackInt(numChunksArg.value)
			if numChunks.hasValue && numChunks.value > 0 && (numBits % numChunks.value) == 0 {				
				result.tag = Tag { kind: TagKind.number, q: numBits / numChunks.value }
				if index.hasValue {
					if index.value < numChunks.value {
						// OK
					} else {
						s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[1].expr), "Expected: constant; must be lower than number of chunks"))
					}
				}
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[2].expr), format("Expected: constant; must be a divisor of target type {}", tagString(s.comp, arrayArg.tag))))
			}
		}
		return result
	}

	swizzle(s TypeCheckerState, e CallExpression) {
		if s.module == null {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "Unsupported expression: cannot use swizzle() in constant initializer"))
		}
		if !s.isStaticExpr {
			s.errors.add(Error.at(s.unit, RangeFinder.find(e.target), "swizzle() can only be used in static initializer"))
		}
		builtinArgs(s, e, 4)
		arrayArg := fixedNumberArg(s, e, 0)
		seqSizeArg := constantArg(s, e, 1)
		stepArg := constantArg(s, e, 2)
		blockSizeArg := constantArg(s, e, 3)
		if arrayArg.tag.isValid() && seqSizeArg.tag.isValid() && stepArg.tag.isValid() && blockSizeArg.tag.isValid() {
			numBits := unpackWidth(arrayArg.tag)
			seqSize := tryUnpackInt(seqSizeArg.value)
			step := tryUnpackInt(stepArg.value)
			blockSize := tryUnpackInt(blockSizeArg.value)
			if numBits % 8 == 0 {
				if blockSize.hasValue && blockSize.value > 0 && (numBits % blockSize.value) == 0 {
					if step.hasValue && step.value > 0 && (blockSize.value % step.value) == 0 {
						if seqSize.hasValue && seqSize.value > 0 && (step.value % seqSize.value) == 0 {
							return TypeCheckResult { tag: arrayArg.tag }
						} else {
							s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[1].expr), "Expected: constant; must be a divisor of step size"))
						}
					} else {
						s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[2].expr), "Expected: constant; must be a divisor of block size"))
					}
				} else {
					s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[3].expr), "Expected: constant; must be a divisor of target type size"))
				}
			} else {
				s.errors.add(Error.at(s.unit, RangeFinder.find(e.args[0].expr), "Target type size must be a multiple of 8"))
			}
		}
		return TypeCheckResult{}
	}

	builtinArgs(s TypeCheckerState, e CallExpression, count int) {
		if e.args.count != count {
			s.errors.add(Error.at(s.unit, e.openParen.span, format("Expected: {} args but got {} args", count, e.args.count)))
		}
		for arg, i in e.args {			
			if arg.name != null {
				s.errors.add(Error.at(s.unit, arg.name.span, "Expected: expression"))
			}
			if i >= count {
				expression(s, arg.expr)
			}
		}
	}

	fixedNumberArg(s TypeCheckerState, e CallExpression, index int) {
		if index >= e.args.count {
			return TypeCheckResult{}
		}
		arg := e.args[index]
		if arg.expr == null {
			return TypeCheckResult{}
		}
		tr := expression(s, arg.expr)
		if tr.tag.isValid() {
			if tr.tag.kind == TagKind.number && tr.tag.q > 0 {
				// OK
			} else {
				expectedFixedNumberExpression(s, arg.expr)
				return TypeCheckResult{}
			}
		}
		return tr
	}

	numberArg(s TypeCheckerState, e CallExpression, index int) {
		if index >= e.args.count {
			return TypeCheckResult{}
		}
		arg := e.args[index]
		if arg.expr == null {
			return TypeCheckResult{}
		}
		tr := expression(s, arg.expr)
		if tr.tag.isValid() {
			if tr.tag.kind == TagKind.number {
				// OK
			} else {
				expectedNumberExpression(s, arg.expr)
				return TypeCheckResult{}
			}
		}
		return tr
	}

	constantArg(s TypeCheckerState, e CallExpression, index int) {
		if index >= e.args.count {
			return TypeCheckResult{}
		}
		arg := e.args[index]
		if arg.expr == null {
			return TypeCheckResult{}
		}
		tr := expression(s, arg.expr)
		if tr.tag.isValid() {
			if tr.tag.kind == TagKind.number && tr.value.kind == ValueKind.ulong_ {
				// OK
			} else {
				expectedConstant(s, arg.expr)
				return TypeCheckResult{}
			}
		}
		return tr
	}

	fixedNumericTypeArg(s TypeCheckerState, e CallExpression, index int) {
		if index >= e.args.count {
			return TypeCheckResult{}
		}
		arg := e.args[index]
		if arg.expr == null {
			return TypeCheckResult{}
		}
		if arg.expr.is(Token) {
			tag := typename(s, arg.expr, arg.expr.as(Token))
			if tag.isValid() {
				if tag.kind == TagKind.number && tag.q > 0 {
					return TypeCheckResult { tag: tag }
				} else {
					expectedFixedNumberType(s, arg.expr)
				}
			}
		} else {
			tr := expression(s, arg.expr)
			if tr.tag.isValid() {
				expectedFixedNumberType(s, arg.expr)
			}
		}
		return TypeCheckResult{}
	}
}
