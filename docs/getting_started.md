# Getting started

## Building the compiler

You can choose between the following two options:

### From .mu source

1. [Install the Muon compiler](https://github.com/nickmqb/muon/blob/master/docs/getting_started.md).
1. Clone this repo
1. Go to the compiler directory (in this repo)
1. Compile .mu sources: `mu --args wyre.args`
1. Use a C compiler to compile wyre.c. E.g.:
	* GCC: `gcc -o wyre wyre.c`
	* MSVC: `cl /Zi wyre.c`
1. You now have a Wyre compiler!

### From .c

If you prefer to not install Muon, you can take a shortcut:

1. Clone this repo
1. Go to the dist directory
1. Compile wyre.c with a C compiler of your choice (see instructions above)
1. You now have a Wyre compiler!

## Running the compiler

The syntax for invoking the compiler is as follows:

	wyre [flag] [source_file.w] [flag] [source_file.w] ...

Source files and flags may be mixed and may be specified in any order.

Supported flags:

* `--output [path]`. The output of the compiler, a Verilog file.
* `--top [module]`. Name of the topmost module in the design.
* `--indent [n]`. Tab size, in spaces ([learn more about significant whitespace](language_tour.md)). Set to 0 to ignore.
* `--max-errors [n]`. Maximum number of compile errors to display.

To compile the [led](../examples/led.w) example:

1. Navigate to the `examples` directory.
2. Run: `wyre led.w --output led.v --top top --indent 4 --max-errors 20`

Tip: to avoid having to specify all arguments every time you invoke the compiler, it is recommendeded to create a short shell script (.sh) or batch file (.bat) that contains the command.
