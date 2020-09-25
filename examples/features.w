// This design demonstrates some features of Wyre.

subtract_module(x $8, y $8) {
	out output := x - y
}

// Declare a module with inputs clk (1 bit wide), a and b (both 4 bits wide)
some_module(clk $1, a $4, b $4) {

	// Wires
	some_wire := a + b // some_wire is inferred to be $4 (i.e. 4 bits wide)
	
	some_wire2 $4 := a + b // Equivalent

	// Literals
	foo := '10101010 // Bit literal. foo is inferred to be $8
	
	bar $8 := 3 // Need to specify type because '3' doesn't have a specific width
	
	bar2 := 3_$8 // _$N suffix can be used to treat number as specific width

	hex $4 := 0xa // Need to specify type for same reason as above

	hex2 := 0xa_$4 // Can use _$N suffix

	// Outputs
	out some_output := a | b // some_output is inferred to be $4

	out some_output2 $4 := a | b // Equivalent

	// Registers
	reg some_reg = '1111 // some_reg is inferred to be $4; initialized to '1111 (15)

	reg some_reg2 $4 = '1111 // Equivalent

	reg some_reg3 $4 // Initial value is 0 if not specified

	// Register updates; must happen inside clock statement (posedge/negedge keyword)
	posedge clk {
		some_reg <= 1

		// Conditional updates
		if a == '0101 {
			some_reg2 <= 2

			reg other_reg $4 <= 3 // Can declare registers anywhere; can combine declaration and update

			nested_wire := some_reg + other_reg // Can declare wires anywhere (unaffected by clock and if statement)
		} else {
			some_reg2 <= 0xf
		}
	}

	negedge clk {
		some_reg3 <= b
	}

	// Unary operators
	negated := -a
	inverted := ~a
	extended_wire $32 := zx a // a is zero extended to 32 bits total

	// Binary operators
	op_add := a + b
	op_subtract := a - b
	op_and := a & b
	op_or := a | b
	op_xor := a ^ b
	op_compare := a == b // Or: != < <= > >=
	op_shl := a << 1
	op_shr := a >> 1

	// Ternary operator
	c := some_wire == 3 ? a : b
	
	// Select & concatenate bits
	msb := a[3]
	rest := a[2:0]
	recombined := { a[3], a[2:0] }
	repeated_wire := rep(a[2:0], 10) // Use builtin function rep to repeat values. repeated_wire is inferred to be $30

	// match
	match_result := match some_wire[1:0] {
		'00: a & b
		'01: a | b
		'10: a ^ b
		'11: a & ~b
	}

	// Module instantiation
	sub := subtract_module(x: 8, y: 5) // Connect inputs x and y

	baz := sub.output // Use module output, type of baz is inferred to be $8

	sub2 := subtract_module(x: ---, y: 5) // All inputs must be specified; to leave disconnected, use ---

	sub3 := subtract_module(x: 'xxxxxx00, y: 5) // Or use bit literal with "don't care" value (x) for partially disconnected
}

// Module with a static input and a normal input
// The static input must be assigned a value that is constant at initialization time, and won't be part of the final hardware design
another_module(
	#some_static_input $32
	q $32
) {
	// Static variables
	#lower_half := #some_static_input[15:0] // Constant at initialization time, won't be part of final hardware design

	// Define data block; #data is inferred to be $128
	#data := [
		c0cac01ac0cac01a
		c0cac01ac0cac01a
	]

	// Swizzle #data; even bits followed by odd bits; swizzle(target, seq_size, step, block_size) (for details, see compiler/tape.mu, swizzle(...)).
	#sw := swizzle(#data, 1, 2, 128)

	// Divide #sw into two chunks ($64 each); chunk(target, chunk_index, number_of_chunks)
	#part_a := chunk(#sw, 0, 2)
	#part_b := chunk(#sw, 1, 2)
}

// Note: as you can see above, module inputs can be separated by newlines if you prefer. This also works for module instantiations.

// Constants are global, and must be declared outside a module
some_constant $10 := 100
some_constant2 := 100 // Can omit type if you prefer

// Blackbox module declaration, for external modules. Example: a global buffer from Lattice iCE40 FPGA.
SB_GB blackbox(
	USER_SIGNAL_TO_GLOBAL_BUFFER $1
) {
	// Outputs are declared inside module body
	out GLOBAL_BUFFER_OUTPUT $1
}
