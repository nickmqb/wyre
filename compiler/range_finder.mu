RangeFinderState struct #RefType {
	range IntRange
}

RangeFinder {
	find(e Node) {
		s := RangeFinderState { range: IntRange { from: int.maxValue, to: int.minValue } }
		node(ref s, e)
		return s.range
	}

	node(s RangeFinderState, e Node) {
		match e {
			Token: {
				token(s, e)
			}
			NumberExpression: {
				token(s, e.token)
			}
			UnaryOperatorExpression: {
				token(s, e.op)
				node(s, e.expr)
			}
			BinaryOperatorExpression: { 
				node(s, e.lhs)
				token(s, e.op)
				node(s, e.rhs)
			}
			DotExpression: {
				node(s, e.lhs)
				token(s, e.dot)
				token(s, e.rhs)
			}
			TernaryOperatorExpression: {
				node(s, e.conditionExpr)
				token(s, e.question)
				node(s, e.trueExpr)
				token(s, e.colon)
				node(s, e.falseExpr)
			}
			MatchExpression: {
				token(s, e.keyword)
				node(s, e.target)
				token(s, e.openBrace)
				for c in e.contents {
					node(s, c)
				}
				token(s, e.closeBrace)
			}
			MatchExpressionCase: {
				node(s, e.valueExpr)
				token(s, e.colon)
				node(s, e.resultExpr)
			}
			CallExpression: {
				node(s, e.target)
				token(s, e.openParen)
				for c in e.contents {
					node(s, c)
				}
				token(s, e.closeParen)
			}
			StructInitializerExpression: {
				node(s, e.target)
				token(s, e.openBrace)
				for c in e.contents {
					node(s, c)
				}
				token(s, e.closeBrace)
			}
			ParenExpression: {
				token(s, e.openParen)
				node(s, e.expr)
				token(s, e.closeParen)
			}
			BraceExpression: {
				token(s, e.openBrace)
				for c in e.contents {
					node(s, c)
				}
				token(s, e.closeBrace)
			}
			IndexExpression: {
				node(s, e.target)
				token(s, e.openBracket)
				node(s, e.upperExpr)
				token(s, e.colon)
				node(s, e.lowerExpr)
				token(s, e.closeBracket)
			}
			ArrayExpression: {
				token(s, e.openBracket)
				for c in e.contents {
					node(s, c)
				}
				token(s, e.closeBracket)
			}
			null: {}
		}
	}

	token(s RangeFinderState, e Token) {
		if e == null {
			return
		}
		s.range.from = min(s.range.from, e.span.from)
		s.range.to = max(s.range.to, e.span.to)
	}
}
