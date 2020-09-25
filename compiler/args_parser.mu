//tab_size=4

SourceFile struct #RefType {
    path string
    text string
}

Args struct #RefType {
    sources List<SourceFile>
	top string
    outputPath string
	indent int
	maxErrors int
	printVersion bool
	printHelp bool
}

tryReadSourceFile(path string) {
    sb := new StringBuilder{}
    if !File.tryReadToStringBuilder(path, sb) {
        return null
    }
    sb.writeChar('\0')
    return new SourceFile { path: path, text: sb.compactToString() }
}

parseArgs(parser CommandLineArgsParser, isCompiler bool) {
    args := new Args { sources: new List<SourceFile>{} }

    token := parser.readToken()

	if token == "" {
		args.printHelp = true
	}

	hasIndent := false
	hasMaxErrors := false

    while token != "" {
        if token.startsWith("-") {
            if token == "--top" {
                token = parser.readToken()
                if token != "" {
                    args.top = token
                } else {
                    parser.expected("module name")
                }
            } else if token == "--output" {
                token = parser.readToken()
                if token != "" {
                    args.outputPath = token
                } else {
                    parser.expected("path")
                }
            } else if token == "--indent" {
				num := parseInt(parser)
				if num.hasValue {
					if num.value >= 0 {
						args.indent = num.value
						hasIndent = true
					} else {
						parser.error("Expected: number, >= 0")
					}
				}
            } else if token == "--max-errors" {
				num := parseInt(parser)
				if num.hasValue {
					if num.value > 0 {
						args.maxErrors = num.value
						hasMaxErrors = true
					} else {
						parser.error("Expected: number, > 0")
					}
				}
            } else if token == "--version" {
				args.printVersion = true
            } else if token == "--help" {
				args.printHelp = true
            } else {
				parser.error(format("Invalid flag: {}", token))
			}
        } else {
            sf := tryReadSourceFile(token)
            if sf != null {
                args.sources.add(sf)
            } else {
                parser.error(format("Could not read file: {}", token))
            }
        }
		token = parser.readToken()
    }

	if args.printHelp || args.printVersion {
		return args
	}

	if args.sources.count == 0 {
		parser.expected("One or more source files")
	}
	if args.top == "" {
		parser.expected("--top [module]")
	}
	if args.outputPath == "" && isCompiler {
		parser.expected("--output [path]")
	}
	if !hasIndent && isCompiler {
		parser.expected("--indent [number]")
	}
	if !hasMaxErrors {
		parser.expected("--max-errors [number]")
	}

    return args
}

parseInt(parser CommandLineArgsParser) {
	token := parser.readToken()
	if token == "" {
		parser.expected("number")
		return Maybe<int>{}
	}
	val := int.tryParse(token)
	if !val.hasValue {
		parser.error("Expected: number")
	}
	return val
}
