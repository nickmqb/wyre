//tab_size=4

main() {
    ::currentAllocator = Memory.newArenaAllocator(256 * 1024 * 1024)

	CrashHandler.enable()

    argErrors := new List<CommandLineArgsParserError>{}
	parser := new CommandLineArgsParser.from(Environment.getCommandLineArgs(), argErrors)
	args := parseArgs(parser, true)

    if argErrors.count > 0 {
		info := parser.getCommandLineInfo()
		for argErrors {
			Stderr.writeLine(CommandLineArgsParser.getErrorDesc(it, info))
		}
		exit(1)
	}

	if args.printVersion || args.printHelp {
		Stdout.writeLine(format("Wyre compiler, version {}", compilerVersion))
		if args.printHelp {
			Stdout.writeLine("For documentation, see: https://github.com/nickmqb/wyre")
		}
		exit(1)
	}

	comp := new Compilation {
		sources: args.sources,
		errors: new List<Error>{},
		units: new List<CodeUnit>{},
	}

    for sf, i in comp.sources {
        unit := Parser.unit(sf.text, comp.errors, args.indent)
        unit.path = sf.path
        unit.id = i
		comp.units.add(unit)
    }

	Indexer.comp(comp)

	top := comp.symbols.getOrDefault(args.top)
	if top.is(ModuleDef) {
		top.as(ModuleDef).flags |= ModuleFlags.top
	} else {
		comp.errors.add(Error { text: format("Top module not found: {}", args.top) })
	}

	TypeChecker.comp(comp)

	es := cast(null, EmulatorState)
	if comp.errors.count == 0 {
		es = Emulator.init(comp, top.as(ModuleDef))
		Emulator.reset(es)
	}

	// Sort errors, but always show syntax errors first
	comp.errors.slice(0, comp.nonSyntaxErrorStart).stableSort(ErrorHelper.compareErrors)
	comp.errors.slice(comp.nonSyntaxErrorStart, comp.errors.count).stableSort(ErrorHelper.compareErrors)

    if comp.errors.count > 0 {
        for e, i in comp.errors {
			if i >= args.maxErrors {
				break
			}
            if e.unit != null {
                Stdout.writeLine(ErrorHelper.getErrorDesc(e.unit.path, e.unit.source, e.span, e.text))
            } else {
                Stdout.writeLine(e.text)
            }
        }
		if comp.errors.count > args.maxErrors {
			Stdout.writeLine(format("{} errors ({} shown)", comp.errors.count, args.maxErrors))
		} else {
			Stdout.writeLine(format("{} errors", comp.errors.count))
		}
		exit(1)
    }

	out := VerilogGenerator.comp(comp, es)

	if !File.tryWriteString(args.outputPath, out.sb.compactToString()) {
		Stderr.writeLine(format("Could not write to output file: {}", args.outputPath))
		exit(1)		
	}

	Stdout.writeLine(format("Generated output: {}", args.outputPath))
}
