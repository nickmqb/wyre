# Features

### Strongly typed

All types must match exactly, which helps prevent mistakes.

### Compact bit literals

Bit literals are used a lot in hardware designs, so Wyre makes it easy to use them, using the `'` symbol. Examples: `'0`, `'101`, `'00110011`, etc.

### Type inference

Types of outputs, wires and registers can be inferred in most cases. Example: `out foo := '111`. The compiler infers that foo is an output that is 3 bits wide (if you want to be explicit, you could also write: `out foo $3 := '111`).

### Inline module instantiations

Modules can be instantiated inline. Example:

	some_module() {
		// ...

		// Instantiate module called 'adder' and connect inputs lhs, rhs and c_in
		sum := adder(lhs: x, rhs: y, c_in: 0)

		// ...
	}

If we imagine that `adder` has two outputs `val` and `c_out`, they can be accessed as `sum.val` and `sum.c_out`. There is no need to explicitly connect any output wires.

### Order independent declarations

Declarations (modules, wires, outputs, registers) can be ordered however you want. Symbols can be used before their declaration. Symbols can even be declared inside clock and if statements.

### Minimalistic syntax

Types have a compact notation: `$N`, where N is the number of bits. E.g. `$1` (single bit), `$10` (10 bits), `$1024` (1Kbit block).

Names go before types (following the trend of modern programming languages like Go, Rust, etc.).

Wyre uses C-style `{` block syntax `}` and uses newlines as statement separators. You can also separate module inputs with newlines instead of commas if you like.

There are only 9 keywords in Wyre! `reg`, `out`, `if`, `else`, `match`, `posedge`, `negedge`, `blackbox`, `struct`

### `match` expressions

`match` expressions are essentially a switch statement in expression form. For example:

	alu(a $1, b $1, op $2) {
		out val := match op {
			'00: a & b
			'01: a | b
			'10: a ^ b
			'11: a & ~b
		}
	}

### Large data literals (data blocks)

In some designs you might need to define a larger block of data, e.g. to initialize some memory. This is easy in Wyre, just specify your data as a hex string of bytes:

	[ 01ff02fe03... ]

### Builtin functions for transforming data

Wyre provides builtin functions `slice`, `chunk` and `swizzle` to slice and transform data. This can be useful for splitting data across multiple memory banks, for example.

### Structs

	Point struct {
		x $8
		y $8
	}

	// later on...
	p := Point { x: 4, y: 5 }
	q := p.x
	// etc..

### Column accurate compile error reporting

Each compile error has exact location information, so you can quickly fix problems. A typical error looks like:

	Undefined symbol: some_wire
	-> ../examples/leds.w:14
    	out o := some_wire + 1
            	 ~~~~~~~~~

### But wait: there is more!

Check out the [Language guide](language_guide.md) to learn more about:

* `#inputs` and `#wires` (for initialization)
* `blackbox` modules
* `zx` operator (for zero extension)
* `rep` builtin function (repeat value)
* `---` symbol to specify disconnected inputs
* Constants
* All supported operators (unary, binary, ternary)
* Statements (`posedge`, `negedge`, `if`, `else`, etc.)
* More details on the features discussed on this page
