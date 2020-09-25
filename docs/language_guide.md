# Language guide

## Syntax

Before we begin, a quick note on syntax.

In Wyre, whitespace before a line (i.e. indentation) is significant. Besides using { and } for blocks, Wyre expects the contents of each block to be indented (like in Python). This is done to improve compile error recovery and compile error message quality.

In order for this to work, Wyre needs to know your tab size. You must specify your tab size via the compiler command line flag `--indent [n]`.

If you don't want to use significant whitespace, simply pass `--indent 0` (but be warned that you might get lower quality compile error messages!).

## TODO

This section is still being written! For the time being:

* Check out the [feature overview](feature_overview.md) if you haven't already.
* See the [features example](../examples/features.w).
* View other [examples](../examples).
