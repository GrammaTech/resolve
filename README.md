Resolve
=======

TRL: 5

Software AST-based diff calculation, display, and automated resolution.

## Abstract
Resolve implements multi-lingual semantic differencing of software
source code and implements an automated technique of merge conflict
resolution.  This provides developers with more meaningful views of
software changes and it frees developers from the tedious manual task
of merge conflict resolution.

## Use Cases

### Improved difference calculation and view
Senior developers spend a significant portion of their time reviewing
the work products of other developers.  The primary mode of
representing these work products are as differences against the
existing code base.  By calculating syntactically aware differences
over software abstract syntax trees (ASTs) we provide more informative
and meaningful views of software changes.

### Automated merge conflict resolution
On project with multiple developers, a significant portion of time is
typically spent manually resolving conflicting changes between
parallel branches of development.  By automatically resolving merge
conflicts we are able to save projects a significant amount of
expensive developer time and free developers from the often tedious
job of manual conflict resolution.

## Tools
- command-line AST differencing
- web-based AST differencing
- test-suite guided merge conflict resolution

## Language Support
- JavaScript via ESTree ASTs (w/acorn)
- C/C++ via Clang ASTs
- Java via JavaParser ASTs
- Common-Lisp via Eclector

## License
Resolve, and any accompanying documentation or other materials are
GrammaTech, Inc. (GrammaTech) Proprietary Information.

See [LICENSE.txt](LICENSE.txt) for more information.
