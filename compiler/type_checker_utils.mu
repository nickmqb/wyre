TypeChecker {
	tag(s TypeCheckerState, name string) {
		if name[0] == '$' {
			pr := int.tryParse(name.slice(1, name.length))
			if pr.hasValue && pr.value > 0 {
				return Tag { kind: TagKind.number, q: pr.value }
			}
		} else {
			sym := s.symbols.getOrDefault(name)
			if sym != null && sym.is(StructDef) {
				return Tag { kind: TagKind.struct_, q: sym.as(StructDef).id }
			}
		}
		return Tag{}
	}

	tagString(comp Compilation, tag Tag) {
		if tag.kind == TagKind.number {
			if tag.q != 0 {
				return format("${}", tag.q)
			} else {
				return format("$*", tag.q)
			}
		} else if tag.kind == TagKind.moduleOut {
			return format("[{}]", comp.entities[tag.q].as(ModuleDef).name.value)
		} else if tag.kind == TagKind.struct_ {
			return format("[{}]", comp.entities[tag.q].as(StructDef).name.value)
		}
		return "[unknown]"
	}

	canAssign_andUpgrade(s TypeCheckerState, from Tag, fromValue Value, node Node, to Tag) {
		if to.kind != TagKind.unknown {
			if from.kind == TagKind.number {
				if to.kind == TagKind.number {
					assert(to.q > 0)
					if to.q == from.q {
						return true
					}
					if from.q == 0 {
						return canConvertFreeConst_andUpgrade(s, fromValue, node, to)
					}
				}
				return false
			} else if from.kind == TagKind.moduleOut || from.kind == TagKind.struct_ {
				return to.kind == from.kind && to.q == from.q
			}
		}
		return true
	}

	// Returns a value between 0 and 64
	// 0 => all zeroes; 1 to 64 => bit N-1 is the highest bit
	highestBit(n ulong) {
		result := 0
		while n != 0 {
			n >>= 1
			result += 1
		}
		return result
	}

	canConvertFreeConst_andUpgrade(s TypeCheckerState, fromValue Value, node Node, to Tag) {
		if fromValue.kind == ValueKind.ulong_ && highestBit(fromValue.z) <= to.q {
			s.typeMap.update(node, to)
			return true
		}
		return false
	}

	isFixedNumberOrStruct(tag Tag) {
		return (tag.kind == TagKind.number && tag.q > 0) || tag.kind == TagKind.struct_
	}

	isFixedNumberOrStructOrModuleOut(tag Tag) {
		return (tag.kind == TagKind.number && tag.q > 0) || tag.kind == TagKind.struct_ || tag.kind == TagKind.moduleOut
	}

	numSlots(comp Compilation, tag Tag) int {
		if tag.kind == TagKind.number {
			return 1
		} else if tag.kind == TagKind.struct_ {
			return comp.entities[tag.q].as(StructDef).fields.count
		}
		abandon()
	}

	isValidEntityName(s string) {
		return s[0] != '$'
	}

	isValidEntityName_nonStatic(s string) {
		return s[0] != '$' && s[0] != '#'
	}

	unpackWidth(tag Tag) {
		assert(tag.kind == TagKind.number)
		assert(tag.q > 0)
		return tag.q
	}

	unpackInt(value Value) {
		assert(value.kind == ValueKind.ulong_)
		assert(value.z <= cast(int.maxValue, ulong))
		return cast(value.z, int)
	}

	tryUnpackInt(value Value) {
		assert(value.kind == ValueKind.ulong_)
		if value.z <= cast(int.maxValue, ulong) {
			return Maybe.from(cast(value.z, int))
		}
		return Maybe<int>{}
	}

	badSymbol(s TypeCheckerState, token Token) {
		s.errors.add(Error.at(s.unit, token.span, format("Undefined symbol: {}", token.value)))
	}

	duplicateSymbol(s TypeCheckerState, token Token) {
		s.errors.add(Error.at(s.unit, token.span, "A symbol with the same name has already been defined"))
	}

	statementNotAllowedInsideBlackbox(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Statement is not allowed inside blackbox module"))
	}

	badAssign(s TypeCheckerState, at Token, from Tag, to Tag) {
		s.errors.add(Error.at(s.unit, at.span, format("Cannot assign {} to {}", tagString(s.comp, from), tagString(s.comp, to))))
	}

	badConversion(s TypeCheckerState, e Node, from Tag, to Tag) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), format("Cannot convert {} to {}", tagString(s.comp, from), tagString(s.comp, to))))
	}

	badConstConversion(s TypeCheckerState, e Node, from Tag, to Tag) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), format("Cannot convert constant of type {} to {}", tagString(s.comp, from), tagString(s.comp, to))))
	}

	unsupportedRegUpdate(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: register, register.field or slice(...)"))
	}

	unsupportedUnaryOp(s TypeCheckerState, op Token, tag Tag) {
		s.errors.add(Error.at(s.unit, op.span, format("Unsupported expression: cannot apply operator {} to argument of type {}", op.value, tagString(s.comp, tag))))
	}

	unsupportedBinaryOp(s TypeCheckerState, op Token, lhs Tag, rhs Tag) {
		s.errors.add(Error.at(s.unit, op.span, format("Unsupported expression: cannot apply operator {} to arguments of type {} and {}", op.value, tagString(s.comp, lhs), tagString(s.comp, rhs))))
	}

	badUnaryOp(s TypeCheckerState, op Token, tag Tag) {
		s.errors.add(Error.at(s.unit, op.span, format("Cannot apply operator {} to argument of type {}", op.value, tagString(s.comp, tag))))
	}

	badBinaryOp(s TypeCheckerState, op Token, lhs Tag, rhs Tag) {
		s.errors.add(Error.at(s.unit, op.span, format("Cannot apply operator {} to arguments of type {} and {}", op.value, tagString(s.comp, lhs), tagString(s.comp, rhs))))
	}

	badUnify(s TypeCheckerState, token Token, te Tag, fe Tag) {
		s.errors.add(Error.at(s.unit, token.span, format("Cannot unify values of type {} and {}", tagString(s.comp, te), tagString(s.comp, fe))))
	}

	badBinaryOperandConversion(s TypeCheckerState, span IntRange, tag Tag) {
		s.errors.add(Error.at(s.unit, span, format("Cannot convert operand to type {}", tagString(s.comp, tag))))
	}

	badGap(s TypeCheckerState, op Token) {
		s.errors.add(Error.at(s.unit, op.span, format("Cannot apply operator {} because target type is inferred", op.value)))
	}

	badGapArgument(s TypeCheckerState, op Token, arg Tag, target Tag) {
		s.errors.add(Error.at(s.unit, op.span, format("Cannot apply operator {} to argument of type {} and target of type {}", op.value, tagString(s.comp, arg), tagString(s.comp, target))))
	}

	expectedFixedNumberType(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: fixed width numeric type"))
	}

	expectedFixedNumberExpression(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: expression of fixed width numeric type"))
	}

	expectedFixedNumberOrStruct(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: fixed width numeric type or struct type"))
	}

	expectedFixedNumberOrStructExpression(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: expression of fixed width numeric type or struct"))
	}

	expectedNumberExpression(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: expression of numeric type"))
	}

	expectedConstant(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: constant"))
	}

	expectedStaticConstant(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Expected: expression that evaluates to constant during initialization"))
	}

	invalidNamePrefix(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Invalid name; must start with letter, _ or #"))
	}

	invalidNamePrefix_nonStatic(s TypeCheckerState, e Node) {
		s.errors.add(Error.at(s.unit, RangeFinder.find(e), "Invalid name; must start with letter or _"))
	}
}
