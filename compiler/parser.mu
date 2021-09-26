//tab_size=4
:compilerVersion = "0.1.3"

ParseCommaListState enum {
	start
	expectValue
	expectComma
}

ParseState struct #RefType {
    text string
    index int
    lineStart int
    token Token
    prevTokenTo int
    tabSize int
	ignoreIndent bool
    base int
	exprNoBrace bool
	blockBase int
    unit CodeUnit
	module ModuleDef
	struct_ StructDef
    errors List<Error>
}

Parser {
    :sameLine = int.maxValue
	:maxBase int #Mutable

    unit(text string, errors List<Error>, tabSize int) {
		maxBase = int.maxValue - 1 // Work around Muon limitation re: constant initializers

        unit := new CodeUnit { contents: new List<Node>{}, source: text }

        s := new ParseState { text: text, unit: unit, errors: errors, token: new Token{} }

		if tabSize > 0 {
			s.tabSize = tabSize
		} else {
			s.tabSize = 1
			s.ignoreIndent = true
			s.base = -1
			s.blockBase = -1
		}

        readToken(s)
        while s.token.type != TokenType.end {
            if s.token.indent == 0 {
                if s.token.type == TokenType.identifier {
					name := s.token
					readToken(s)
					if s.token.type == TokenType.identifier && s.token.value == "struct" && s.token.indent == sameLine {
						struct_(s, name, unit.contents) 
					} else if (s.token.type == TokenType.openParen || (s.token.type == TokenType.identifier && s.token.value == "blackbox")) && s.token.indent == sameLine {
                    	module(s, name, unit.contents)
					} else {
						const(s, name, unit.contents)
					}
                } else {
					parseErrorToken(s, "Expected: struct, const or module declaration", unit.contents)
                }
            } else {
                parseErrorToken(s, "Incorrect indentation: top-level declaration may not be indented", unit.contents)
            }
			while s.token.indent == sameLine {
				unit.contents.add(s.token)
				readBadToken_newline(s)
			}
        }

        return unit
    }

    const(s ParseState, name Token, out List<Node>) {
        node := new ConstDef { name: name, unit: s.unit }
        out.add(node)

        if s.token.type == TokenType.identifier && s.token.indent == sameLine {
            node.type = s.token
            readToken(s)
		}

        if s.token.value == ":=" && s.token.indent == sameLine {
            node.assign = s.token
            readToken(s)
        } else {
            expected(s, ":=")
        }

		prev := s.base
		s.base = maxBase
        node.expr = parseExpression(s, 0)
		s.base = prev
    }

    struct_(s ParseState, name Token, out List<Node>) {
        node := new StructDef { name: name, keyword: s.token, unit: s.unit, fields: new List<FieldDef>{} }
		out.add(node)
		readToken(s)
		s.struct_ = node
        node.body = block(s, false, "field declaration")
		s.struct_ = null
	}
	
	fieldDef(s ParseState, out List<Node>) {
		if s.token.type != TokenType.identifier {
			error(s, "Expected: field declaration")
			out.add(s.token)
			readToken(s)
			return
		}
		
        node := new FieldDef { name: s.token }
		out.add(node)
		s.struct_.fields.add(node)
        readToken(s)
        if s.token.type == TokenType.identifier && s.token.indent == sameLine {
            node.type = s.token
            readToken(s)
        } else {
            expected(s, "field type")
        }
	}

    module(s ParseState, name Token, out List<Node>) {
        node := new ModuleDef { name: name, inputs: new List<ModuleInputDef>{}, inputsContents: new List<Node>{}, unit: s.unit }
		out.add(node)

		if s.token.type == TokenType.identifier && s.token.value == "blackbox" && s.token.indent == sameLine {
			node.blackboxKeyword = s.token
			readToken(s)
		}

        if s.token.type == TokenType.openParen && s.token.indent == sameLine {
            node.openParen = s.token
            readToken(s)

            state := ParseCommaListState.start
            while s.token.indent > s.base && s.token.type != TokenType.closeParen && s.token.type != TokenType.openBrace {
                if state != ParseCommaListState.expectComma {
                    if s.token.type == TokenType.identifier {
                        input := moduleInput(s)
                        node.inputs.add(input)
                        node.inputsContents.add(input)
                    } else if s.token.type == TokenType.comma && s.token.indent == sameLine {
						expected(s, "module input declaration")
					} else {
						parseErrorToken(s, "Expected: module input declaration", node.inputsContents)
                    }
                    state = ParseCommaListState.expectComma
                } else {
					if s.token.indent == sameLine {
						parseComma(s, node.inputsContents)
					}
                    state = ParseCommaListState.expectValue
                }
            }

            if s.token.type == TokenType.closeParen && s.token.indent >= s.base {
                node.closeParen = s.token
                readToken(s)
            } else {
				expected(s, ")")
			}
        } else {
            expected(s, "(")
        }

		s.module = node
        node.body = block(s, true, "statement")
		s.module = null
	}

    moduleInput(s ParseState) {
        node := new ModuleInputDef { name: s.token }
		if s.token.value[0] == '#' {
			node.flags |= ModuleInputFlags.static
		}
        readToken(s)

        if s.token.type == TokenType.identifier && s.token.indent == sameLine {
            node.type = s.token
            readToken(s)
        } else {
            expected(s, "input type")
        }

        return node
    }

    block(s ParseState, statements bool, desc string) Block {
        node := new Block { contents: new List<Node>{} }

        if s.token.type == TokenType.openBrace && s.token.indent >= s.base {
            node.openBrace = s.token
            readToken(s)
        } else {
            expected(s, "{")
        }

		if s.token.indent == sameLine {
			if s.token.type != TokenType.closeBrace {
				prev := s.base
				prevBlock := s.blockBase
				s.base = maxBase
				s.blockBase = maxBase
				if statements {
					parseStatement(s, node.contents)
				} else {
					fieldDef(s, node.contents)
				}
				s.base = prev
				s.blockBase = prevBlock
			}
			if s.token.type == TokenType.closeBrace && s.token.indent == sameLine {
				node.closeBrace = s.token
				readToken(s)
				return node
			}
			while s.token.indent == sameLine {
				node.contents.add(s.token)
				readBadToken_newline(s)
			}
		}

		assert(s.token.indent < sameLine)
		firstIndent := s.token.indent

        while s.token.indent > s.base {
			if s.token.type == TokenType.closeBrace {				
				if s.ignoreIndent {
					break
				}
				parseErrorToken(s, format("Expected: {}, or: close brace must be indented less", desc), node.contents)
            } else if s.token.indent != firstIndent {
				parseErrorToken(s, format("Incorrect indentation: must match previous {}", desc), node.contents)
			} else {
				prev := s.base
				prevBlock := s.blockBase
				s.base = s.ignoreIndent ? -1 : s.token.indent
				s.blockBase = s.base
				if statements {
					parseStatement(s, node.contents)
				} else {
					fieldDef(s, node.contents)
				}
				s.base = prev
				s.blockBase = prev
			}
			while s.token.indent == sameLine {
				node.contents.add(s.token)
				readBadToken_newline(s)
			}
        }

        if s.token.type == TokenType.closeBrace {
			if s.token.indent >= s.base {
	            node.closeBrace = s.token			
	            readToken(s)
			} else {
				errorAt(s, s.token.span.from, "Incorrect indentation: close brace must be indented more")
			}
        } else {
			// TODO: improve span of this error?
        	expectedAt(s, s.token.span.from, "}")
        }

        return node
    }

    parseStatement(s ParseState, out List<Node>) {
		if s.token.value == "posedge" || s.token.value == "negedge" {
			parseClockStatement(s, out)
		} else if s.token.value == "if" {
			out.add(parseIfStatement(s))
		} else if s.token.type == TokenType.identifier {
			firstToken := s.token
			firstToken.indent = sameLine
			parseAssignStatement(s, out)
			firstToken.indent = s.base
		} else {
			parseErrorToken(s, "Expected: statement", out)
		}
    }

	parseClockStatement(s ParseState, out List<Node>) {
		node := new ClockStatement { keyword: s.token }
		out.add(node)
		readToken(s)

        if s.token.type == TokenType.identifier && s.token.indent == sameLine {
            node.name = s.token
            readToken(s)
        } else {
            expected(s, "identifier")
        }

        node.body = block(s, true, "statement")
	}

	parseIfStatement(s ParseState) IfStatement {
		node := new IfStatement { ifKeyword: s.token }
		readToken(s)

		prev := s.base
		s.base = maxBase
		s.exprNoBrace = true
		node.expr = parseExpression(s, 0)
		s.base = prev
		s.exprNoBrace = false

        node.ifBody = block(s, true, "statement")

		if s.token.type == TokenType.identifier && s.token.value == "else" && s.token.indent >= s.base {
			node.elseKeyword = s.token
			readToken(s)
			if s.token.type == TokenType.identifier && s.token.value == "if" && s.token.indent == sameLine {
				node.elseBranch = parseIfStatement(s)
			} else {
				node.elseBranch = block(s, true, "statement")
			}
		}

		return node
	}

	parseAssignStatement(s ParseState, out List<Node>) {
		node := new AssignStatement { module: s.module }
		out.add(node)

		if s.token.value == "out" && s.token.indent == sameLine {
			node.outKeyword = s.token
			readToken(s)
		}
		if s.token.value == "reg" && s.token.indent == sameLine {
			node.regKeyword = s.token
			readToken(s)
		}
		
        if s.token.type == TokenType.identifier && s.token.indent == sameLine {
			name := s.token
			if s.token.value[0] == '#' {
				node.flags |= AssignFlags.static
			}
            readToken(s)
			if (s.token.type == TokenType.openParen || (s.token.type == TokenType.operator && s.token.value == ".")) && s.token.indent == sameLine {
				node.nameExpr = parseExpressionTail(s, name, 20)
			} else {
	            node.nameExpr = name
				if s.token.type == TokenType.identifier && s.token.indent == sameLine {
					node.type = s.token
					readToken(s)
				}
			}
        } else {
            expected(s, "identifier")
        }

        if s.token.type == TokenType.operator && s.token.indent == sameLine {
            node.op = s.token
            readToken(s)
        }

		prev := s.base
		s.base = maxBase
		node.expr = parseExpression_orNull(s, 0)
		s.base = prev
	}

	parseExpression(s ParseState, minLevel int) Node {
		if s.token.indent <= s.base {
			expected(s, "expression")
			return null
		}
		leaf := parseExpressionLeaf(s)
		if leaf == null {		
			expected(s, "expression")
			return null
		}
		return parseExpressionTail(s, leaf, minLevel)
	}

	parseExpression_orNull(s ParseState, minLevel int) Node {
		if s.token.indent <= s.base {
			return null
		}
		leaf := parseExpressionLeaf(s)
		if leaf == null {		
			return null
		}
		return parseExpressionTail(s, leaf, minLevel)
	}

	parseExpression_alwaysProgress(s ParseState, minLevel int, out_all List<Node>) Node {
		leaf := parseExpressionLeaf(s)
		if leaf == null {
			out_all.add(s.token)
			error(s, "Expected: expression")
			readToken(s)
			return null
		}
		expr := parseExpressionTail(s, leaf, minLevel)
		out_all.add(expr)
		return expr
	}

	parseExpressionLeaf(s ParseState) Node {
		if s.token.type == TokenType.identifier {			
			if s.token.value == "zx" {
				result := new UnaryOperatorExpression { op: s.token }
				readToken(s)
				result.expr = parseExpression(s, 20)
				return result
			} else if s.token.value == "match" {
				return parseMatchExpression(s)
			} else {
				result := s.token
				readToken(s)
				return result
			}
		}
		if s.token.type == TokenType.numberLiteral {
			return parseNumber(s)
		}
		if s.token.type == TokenType.operator && s.token.value == "---" {
			result := new NumberExpression { token: s.token, dontCare: ulong.maxValue, flags: NumberFlags.valid | NumberFlags.dontCare }
			readToken(s)
			return result
		}
		if s.token.type == TokenType.operator && (s.token.value == "~" || s.token.value == "-") {
			result := new UnaryOperatorExpression { op: s.token }
			readToken(s)
			result.expr = parseExpression(s, 20)
			return result
		}
		if s.token.type == TokenType.openParen {
			result := new ParenExpression { openParen: s.token }
			readToken(s)
			prev := s.base
			prevNB := s.exprNoBrace
			s.exprNoBrace = false
			s.base = s.blockBase
			result.expr = parseExpression(s, 0)
			if s.token.type == TokenType.closeParen && s.token.indent >= s.base {
				result.closeParen = s.token
				readToken(s)
			} else {
				expected(s, ")")
			}
			s.base = prev
			s.exprNoBrace = prevNB
			return result
		}
		if s.token.type == TokenType.openBrace && !s.exprNoBrace {
			node := new BraceExpression { openBrace: s.token, args: new List<Node>{}, contents: new List<Node>{} }
			readToken(s)
			prev := s.base
			s.base = s.blockBase
            state := ParseCommaListState.start
            while s.token.indent > s.base && s.token.type != TokenType.closeBrace {
                if state != ParseCommaListState.expectComma {
					if s.token.type != TokenType.comma {
						expr := parseExpression_alwaysProgress(s, 0, node.contents)
						if expr != null {
							node.args.add(expr)
						}
					} else {
						expected(s, "expression")
					}
                    state = ParseCommaListState.expectComma
                } else {
					parseComma(s, node.contents)
                    state = ParseCommaListState.expectValue
                }
            }
			if s.token.type == TokenType.closeBrace && s.token.indent >= s.base {
				node.closeBrace = s.token
				readToken(s)
			} else {
				expected(s, "}")
			}
			s.base = prev
			return node
		}
		if s.token.type == TokenType.openBracket {
			return parseArrayExpression(s)
		}
		return null
	}

	parseExpressionTail(s ParseState, lhs Node, minLevel int) Node {
		while true {
			if s.token.indent <= s.base {
				return lhs
			}
			if s.token.type == TokenType.openParen {
				lhs = parseCallExpression(s, lhs)
				continue
			}
			if s.token.type == TokenType.openBrace && !s.exprNoBrace {
				lhs = parseStructInitializerExpression(s, lhs)
				continue
			}
			if s.token.type == TokenType.openBracket {
				lhs = parseIndexExpression(s, lhs)
				continue
			}
			if s.token.type != TokenType.operator {
				return lhs
			}
			level := getBindingLevel(s.token.value)
			if level < minLevel {
				return lhs
			}
			if s.token.value == "?" {
				result := new TernaryOperatorExpression { conditionExpr: lhs, question: s.token }
				readToken(s)
				result.trueExpr = parseExpression(s, 10)
				if s.token.value == ":" && s.token.indent > s.base {
					result.colon = s.token
					readToken(s)
					result.falseExpr = parseExpression(s, 10)
				} else {
					expected(s, ":")
				}
				lhs = cast(result, Node)
			} else if s.token.value == "." {
				result := new DotExpression { lhs: lhs, dot: s.token }
				readToken(s)
				if s.token.type == TokenType.identifier && s.token.indent > s.base {
					result.rhs = s.token
					readToken(s)
				} else {
					expected(s, "identifier")
				}
				lhs = cast(result, Node)
			} else {
				result := new BinaryOperatorExpression { lhs: lhs, op: s.token }
				readToken(s)
				result.rhs = parseExpression(s, level + 1)
				lhs = cast(result, Node)
			}
		}
	}

	parseMatchExpression(s ParseState) Node {
		node := new MatchExpression { keyword: s.token, cases: new List<MatchExpressionCase>{}, contents: new List<Node>{} }
		readToken(s)
		prevExprNoBrace := s.exprNoBrace
		s.exprNoBrace = true
		node.target = parseExpression(s, 0)
		s.exprNoBrace = prevExprNoBrace

        if s.token.type == TokenType.openBrace && s.token.indent >= s.base {
            node.openBrace = s.token
            readToken(s)
        } else {
            expected(s, "{")
        }

		while s.token.indent == sameLine {
			node.contents.add(s.token)
			readBadToken_newline(s)
		}

		prevBase := s.base
		s.base = s.blockBase		

		if s.token.indent > s.base {
			assert(s.token.indent < sameLine)
			firstIndent := s.token.indent

			while s.token.indent > s.base {
				if s.token.type == TokenType.closeBrace {				
					if s.ignoreIndent {
						break
					}
					parseErrorToken(s, "Expected: case item, or: close brace must be indented less", node.contents)
				} else if s.token.indent != firstIndent {
					parseErrorToken(s, "Incorrect indentation: must match previous case", node.contents)
				} else {					
					prev := s.base
					s.base = s.ignoreIndent ? -1 : s.token.indent
					parseMatchExpressionCase(s, node.cases, node.contents)
					s.base = prev
				}
				while s.token.indent == sameLine {
					node.contents.add(s.token)
					readBadToken_newline(s)
				}
			}
		} else {
			expected(s, "case item")
		}

		s.base = prevBase

        if s.token.type == TokenType.closeBrace && s.token.indent >= s.blockBase {
			node.closeBrace = s.token
			readToken(s)
        } else {
        	expected(s, "}")
        }

		return node
	}

	parseMatchExpressionCase(s ParseState, out_cases List<MatchExpressionCase>, out_all List<Node>) {
		leaf := parseExpressionLeaf(s)
		if leaf == null {
			out_all.add(s.token)
			error(s, "Expected: expression")
			readToken(s)
			return
		}

		node := new MatchExpressionCase { valueExpr: parseExpressionTail(s, leaf, 0) }
		out_cases.add(node)
		out_all.add(node)

		if s.token.value == ":" && s.token.indent > s.base {
			node.colon = s.token
			readToken(s)
		} else {
			expected(s, ":")
		}

		prev := s.base
		s.base = maxBase
		node.resultExpr = parseExpression(s, 0)
		s.base = prev
	}

	parseArrayExpression(s ParseState) {
		node := new ArrayExpression { openBracket: s.token, contents: new List<Node>{}, data: new List<byte>{} }
		readToken(s)
		prev := s.base
		s.base = s.blockBase
		while s.token.indent > s.base && s.token.type != TokenType.closeBracket {
			node.contents.add(s.token)
			str := s.token.value
			for i := 0; i < str.length; i += 2 {
				to := i + 2
				if to <= str.length {
					val := ulong.tryParseHex(str.slice(i, to))
					if val.hasValue {
						node.data.add(checked_cast(val.value, byte))
					} else {
						s.errors.add(Error.at(s.unit, IntRange(s.token.span.from + i, s.token.span.from + to), "Expected: hexadecimal byte value"))
					}
				} else {
					s.errors.add(Error.at(s.unit, IntRange(s.token.span.to - 1, s.token.span.to), "Expected: hexadecimal byte value"))
				}
			}
			readToken(s)
		}
		if s.token.type == TokenType.closeBracket && s.token.indent >= s.base {
			node.closeBracket = s.token
			readToken(s)
		} else {
			expected(s, "]")
		}
		s.base = prev
		return node
	}

	parseCallExpression(s ParseState, lhs Node) {
		node := new CallExpression { target: lhs, openParen: s.token, args: new List<CallArg>{}, contents: new List<Node>{} }
		if lhs.is(Token) {
			name := lhs.as(Token).value
			if name == "rep" {
				node.builtin = BuiltinCall.rep
			} else if name == "cast" {
				node.builtin = BuiltinCall.cast_
			} else if name == "slice" {
				node.builtin = BuiltinCall.slice
			} else if name == "chunk" {
				node.builtin = BuiltinCall.chunk
			} else if name == "swizzle" {
				node.builtin = BuiltinCall.swizzle
			}
		}
		
		readToken(s)
		prev := s.base
		s.base = s.blockBase

		state := ParseCommaListState.start
		while s.token.indent > s.base && s.token.type != TokenType.closeParen {
			if state != ParseCommaListState.expectComma {
				parseCallArg(s, node.args, node.contents)
				state = ParseCommaListState.expectComma
			} else {
				if s.token.indent == sameLine {
					parseComma(s, node.contents)
				}
				state = ParseCommaListState.expectValue
			}
		}

		if s.token.type == TokenType.closeParen && s.token.indent >= s.base {
			node.closeParen = s.token
			readToken(s)
		} else {
			expected(s, ")")
		}

		s.base = prev

		return node
	}

	parseCallArg(s ParseState, out_args List<CallArg>, out_all List<Node>) {
		prev := s.base
		s.base = maxBase

		if s.token.type == TokenType.identifier {
			node := new CallArg{}
			out_args.add(node)
			name := s.token
			readToken(s)
			if s.token.value == ":" && s.token.indent > s.base {
				node.name = name
				node.colon = s.token
				readToken(s)
				node.expr = parseExpression(s, 0)
			} else {
				node.expr = parseExpressionTail(s, name, 0)
			}
		} else if s.token.type == TokenType.comma && s.token.indent == sameLine {
			expected(s, "expression")
		} else {
			expr := parseExpression_alwaysProgress(s, 0, out_all)
			if expr != null {
				node := new CallArg { expr: expr }
				out_args.add(node)
			}
		}
		s.base = prev
	}

	parseStructInitializerExpression(s ParseState, lhs Node) {
		node := new StructInitializerExpression { target: lhs, openBrace: s.token, args: new List<CallArg>{}, contents: new List<Node>{} }
		
		readToken(s)
		prev := s.base
		s.base = s.blockBase

		state := ParseCommaListState.start
		while s.token.indent > s.base && s.token.type != TokenType.closeBrace {
			if state != ParseCommaListState.expectComma {
				parseCallArg(s, node.args, node.contents)
				state = ParseCommaListState.expectComma
			} else {
				if s.token.indent == sameLine {
					parseComma(s, node.contents)
				}
				state = ParseCommaListState.expectValue
			}
		}

		if s.token.type == TokenType.closeBrace && s.token.indent >= s.base {
			node.closeBrace = s.token
			readToken(s)
		} else {
			expected(s, "}")
		}
		s.base = prev

		return node
	}

	parseIndexExpression(s ParseState, lhs Node) {
		node := new IndexExpression { target: lhs, openBracket: s.token }
		readToken(s)
		prev := s.base
		s.base = s.blockBase

		node.upperExpr = parseExpression(s, 0)
		if s.token.value == ":" && s.token.indent > s.base {
			node.colon = s.token
			readToken(s)
			node.lowerExpr = parseExpression(s, 0)
		}

		if s.token.type == TokenType.closeBracket && s.token.indent >= s.base {
			node.closeBracket = s.token
			readToken(s)
		} else {
			expected(s, "]")
		}
		s.base = prev

		return node
	}

	getBindingLevel(op string) {
		if op == "." {
			return 20
		}
		if op == "*" || op == "<<" || op == ">>" {
			return 19
		}
		if op == "+" || op == "-" {
			return 18
		}
		if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
			return 17
		}
		if op == "&" {
			return 15
		}
		if op == "^" {
			return 14
		}
		if op == "|" {
			return 13
		}
		if op == "?" {
			return 10
		}
		return -1
	}

    readToken(s ParseState) {
        outerFrom := s.index
		indent := 0
        while true {
            ch := s.text[s.index]
            if ch == '\0' {
                finishToken(s, outerFrom, s.index, int.minValue, TokenType.end)
                return
            }
            if ch == ' ' {
                indent += 1
            } else if ch == '\t' {
                indent = (indent + s.tabSize) / s.tabSize * s.tabSize
            } else if ch == '\r' {
                // ok
            } else if ch == '\n' {
                indent = 0
                s.lineStart = s.index + 1
            } else if ch == '/' && s.text[s.index + 1] == '/' {
                s.index += 2
                ch = s.text[s.index]
                while ch != '\n' && ch != '\0' {
                    s.index += 1
                    ch = s.text[s.index]
                }
                continue
            } else {
                break
            }

            s.index += 1
        }

        if s.lineStart < outerFrom {
            indent = sameLine
        } else if s.ignoreIndent {
			indent = 0
		}

        from := s.index
        ch := s.text[s.index]
        if ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') || ch == '_' || ch == '$' || ch == '#' {
            s.index += 1
            ch = s.text[s.index]
            while ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') || ('0' <= ch && ch <= '9') || ch == '_' || ch == '$' || ch == '#' {
                s.index += 1
                ch = s.text[s.index]
            }
            finishToken(s, outerFrom, from, indent, TokenType.identifier)
            return
        }

        if ('0' <= ch && ch <= '9') || ch == '\'' {
            s.index += 1
            ch = s.text[s.index]
            while ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') || ('0' <= ch && ch <= '9') || ch == '_' || ch == '$' {
                s.index += 1
                ch = s.text[s.index]
            }
            finishToken(s, outerFrom, from, indent, TokenType.numberLiteral)
            return
        }

        if ch == ',' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.comma)
            return
        }
        if ch == '(' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.openParen)
            return
        }
        if ch == ')' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.closeParen)
            return
        }
        if ch == '{' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.openBrace)
            return
        }
        if ch == '}' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.closeBrace)
            return
        }
        if ch == '[' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.openBracket)
            return
        }
        if ch == ']' {
            s.index += 1
            finishToken(s, outerFrom, from, indent, TokenType.closeBracket)
            return
        }

		if isOperatorChar(ch) {
			s.index += 1
			ch = s.text[s.index]
			while isOperatorChar(ch) {
				s.index += 1
				ch = s.text[s.index]
			}
	        finishToken(s, outerFrom, from, indent, TokenType.operator)
			return
		}

		s.index += 1
		finishToken(s, outerFrom, from, indent, TokenType.invalid)
    }

    finishToken(s ParseState, outerFrom int, from int, indent int, type TokenType) {
		to := s.index
		ch := s.text[s.index]
        // TODO: skip comments
		while ch == ' ' || ch == '\t' || ch == '\r' {
			s.index += 1
			ch = s.text[s.index]
		}
		if ch == '\n' {
			s.index += 1
			s.lineStart = s.index
		}
		s.prevTokenTo = s.token.span.to
		s.token = new Token {
			type: type, 
			value: s.text.slice(from, to),
			indent: indent,
			span: IntRange(from, to),
			outerSpan: IntRange(outerFrom, s.index)
		}
	}

	parseNumber(s ParseState) {
		result := cast(null, NumberExpression)
		str := s.token.value
		if str[0] == '\'' {
			val := 0_uL
			dontCare := 0_uL
			invalid := false
			for i := 1; i < str.length {
				val <<= 1
				dontCare <<= 1
				ch := str[i]
				if ch == '0' {
					// OK
				} else if ch == '1' {
					val += 1
				} else if ch == 'x' {
					dontCare += 1
				} else {
					invalid = true
				}
			}
			if !invalid && str.length > 1 && str.length <= 65 {
				flags := NumberFlags.valid | NumberFlags.exactWidth | (dontCare != 0 ? NumberFlags.dontCare : NumberFlags.none)
				result = new NumberExpression { token: s.token, value: val, dontCare: dontCare, width: str.length - 1, flags: flags }
			} else {
				error(s, "Invalid binary value")
				result = new NumberExpression { token: s.token, width: str.length - 1, flags: NumberFlags.exactWidth }
			}
		} else {
			isHex := str.length >= 2 && str[0] == '0' && str[1] == 'x'
			i := isHex ? 2 : 0
			while i < str.length && str[i] != '_' {
				i += 1
			}
			prefixTo := i
			while i < str.length && str[i] == '_' {
				i += 1
			}
			suffixFrom := i
			pr := isHex ? ulong.tryParseHex(str.slice(2, prefixTo)) : ulong.tryParse(str.slice(0, prefixTo))
			suffix := str.slice(suffixFrom, str.length)
			width := parseNumberSuffix(s, suffix)
			flags := width > 0 ? NumberFlags.exactWidth : NumberFlags.none
			if pr.hasValue {
				if suffix.length == 0 || width > 0 {
					if width == 0 || TypeChecker.highestBit(pr.value) <= width {
						result = new NumberExpression { token: s.token, value: pr.value, width: width, flags: NumberFlags.valid | flags }
					} else {
						error(s, "Value does not fit into suffix type")
						result = new NumberExpression { token: s.token, width: width, flags: flags }
					}
				} else {
					error(s, "Invalid suffix type")
					result = new NumberExpression { token: s.token, flags: flags }
				}				
			} else {
				error(s, isHex ? "Invalid hexadecimal value" : "Invalid decimal value")
				result = new NumberExpression { token: s.token, width: width, flags: flags }
			}
		}
		readToken(s)
		return result
	}

	parseNumberSuffix(s ParseState, suffix string) {
		if suffix.length > 0 && suffix[0] == '$' {
			pr := int.tryParse(suffix.slice(1, suffix.length))
			if pr.hasValue && pr.value > 0 {
				return pr.value
			}
		}
		return 0
	}

	parseComma(s ParseState, out List<Node>) {
		if s.token.type == TokenType.comma {
			out.add(s.token)
			readToken(s)
		} else {
			expected(s, ",")
		}
	}

    expected(s ParseState, text string) {
        expectedAt(s, s.prevTokenTo, text)
    }

    expectedAt(s ParseState, index int, text string) {
        s.errors.add(Error.atIndex(s.unit, index, format("Expected: {}", text)))
    }

    error(s ParseState, text string) {
        s.errors.add(Error.at(s.unit, s.token.span, text))
    }

    errorAt(s ParseState, index int, text string) {
        s.errors.add(Error.atIndex(s.unit, index, text))
    }

	parseErrorToken(s ParseState, text string, out List<Node>) {
		error(s, text)
		out.add(s.token)
		readToken(s)
	}

	readBadToken_unexpected(s ParseState) {
		readBadToken(s, "Unexpected token(s)")
	}

	readBadToken_newline(s ParseState) {
		readBadToken(s, "Expected: newline")
	}

	readBadToken(s ParseState, text string) {
		if s.errors.count > 0 && (s.errors[s.errors.count - 1].span.to == s.prevTokenTo || s.errors[s.errors.count - 1].span.to == s.token.span.from) {
			s.errors[s.errors.count - 1].span.to = s.token.span.to
		} else {
			error(s, text)
		}
		readToken(s)
	}

	isOperatorChar(ch char) {
		return ch == '+' || ch == '-' || ch == '*' || ch == '&' || ch == '|' || ch == '^' || ch == '~' || ch == '.' || ch == '?' || ch == ':' || ch == '=' || ch == '!' || ch == '<' || ch == '>'
	}
}
