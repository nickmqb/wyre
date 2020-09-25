# Wyre

Ergonomic hardware definition language that compiles to Verilog.

## Features

* Strongly typed
* Type inference
* Inline module instantiations
* Order independent declarations
* Minimalistic syntax
* Newline as statement separator
* Names before types and compact type notation
* C-style { block syntax }
* Compact bit literals
* `match` expressions
* Large data literals (data blocks)
* Builtin functions for transforming data (e.g. slice, swizzle)
* Structs
* Column accurate compile error reporting

[Learn more about these features](docs/feature_overview.md)

## Example

What does Wyre look like? Here is a basic example design with 2 modules, a clock input, a button and 3 leds.

	top(clk $1, button $1) {
		out reg leds $3

		posedge clk {
			if button {
				leds <= '111
			} else {
				leds <= inc(a: leds).o
			}
		}
	}

	inc(a $3) {
		out o := a + 1
	}

## Background

A while back I got a Lattice iCE40 FPGA to learn more about how computers work at a low level. I've had a lot of fun making designs for it in Verilog. During the process I kept track of gripes with the toolchain. I ended up with a list of mostly minor things, but nonetheless a list that had a fair amount of items. I felt that there were enough items to justify building some new tooling, and Wyre is the result.

Compared to Verilog, Wyre aims to cut down on verbosity, reduce errors (via strong typing) and improve design iteration speed. Wyre compiles to Verilog, so any design can be fed through an existing Verilog-based toolchain.

Some other hardware definition languages try to abstract away the hardware in some ways, with features like memory inference and various generative constructs. These higher level features are explicit non-goals for Wyre. Instead, Wyre aims to stay close to the hardware.

The Wyre compiler is written in the [Muon programming language](https://github.com/nickmqb/muon), of which I am the author and which is my main open source project. Syntactically, Wyre and Muon have a quite a bit in common. So if you like Wyre, be sure to [check out Muon](https://github.com/nickmqb/muon) too!

## Getting started

For a quick summary of features, check out [Feature overview](docs/feature_overview.md) (recommended).

For in-depth documentation, have a look at the [Language guide](docs/language_guide.md).

To learn more about how to install and use the compiler, go to [Getting started](docs/getting_started.md). Wyre works on Windows and Linux. I haven't tested it on macOS, but it will likely work.

You can also view [more examples](examples).

## Roadmap

Feedback is welcome! If you have ideas for new features or for improving existing features, let me know [by creating an issue](https://github.com/nickmqb/wyre/issues).

## Twitter

To stay up-to-date on Wyre, consider [following me on Twitter](https://twitter.com/nickmqb).

## License

[MIT](LICENSE)
