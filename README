Atul's Mini-C Compiler
June 2, 2004

This is a compiler for a subset of the C programming language. It was
written in Python during the spring of 2004.

The lexer and parser were constructed using Dave Beazley's PLY (Python
Lex-Yacc), an open-source Python implementation of GNU
lex/yacc. Stages of compilation (symbol tree generation, type
checking, flow control checking, etc) are performed using an
object-oriented design pattern called a visitor (GoF 1995). The output
is annotated Intel 80x86 assembly, suitable for translation to machine
language using the GNU Assembler (GAS).

---------------------------------------------------------------
LANGUAGE FEATURES
---------------------------------------------------------------

The subset of the C language implemented here includes:

    * Functions, variables (local and global), and character and
      string literals.

    * Assignments (=, +=, etc), standard arithmetic binary and unary
      operators (+,-,*, etc), logical binary and unary operators (!,
      ==, <, etc).

    * Support for the C datatypes char and int, as well as implicit
      type conversion between the two (warnings are raised in
      situations of potential data loss). int variables are assumed to
      be signed, and char variables are assumed to be unsigned (this
      is not a violation of the ANSI C standard).

    * Control flow elements including while and for loops,
      if/then/else conditionals, and recursion.

    * Support for the C keywords extern for functions and variables,
      and static for functions.

    * Pointers, including pointer dereferencing (the * operator),
      multiple levels of indirection (double pointers, triple
      pointers, etc), array indexing notation, and the address-of (&)
      operator.

---------------------------------------------------------------
FILES AND DIRECTORIES
---------------------------------------------------------------

    lex.py       - Python Lex (this is part of PLY).
    yacc.py      - Python Yacc (this is part of PLY).
    clex.py      - Mini-C lexer.
    cparse.py    - Mini-C parser.  Contains yacc rules for Mini-C and
                   defines the classes that make up the AST.
    cvisitors.py - Mini-C visitors.  Defines the base visitor class,
                   and concrete visitor classes for printing the AST,
                   doing symbol table generation, type checking, and
                   flow control.
    cx86.py      - Intel 80x86 assembly code generator.  Defines a
                   virtual stack machine class and the code generator
                   visitor.
    c.py         - Front-end to the compiler.  This takes in command-
                   line options and runs the compiler on the filenames
                   you give it.
    samples/     - This directory contains foo.c and foo_lib.c, two
                   C files that can be compiled by the mini-c
                   compiler.  foo_lib.c is intended to be used as
                   a library that foo.c accesses, to show
                   that mini-c generates assembly that can be linked
                   with gcc.

---------------------------------------------------------------
USING THE COMPILER
---------------------------------------------------------------

The syntax for using the mini-c compiler is as follows:

    c.py <source-file-1> [[source-file-2] ...] [-ast] [-annotate]

Source files are the C files you want to compile into assembly (.s
files).

The '-ast' option generates a file with extension .ast that is a
printout of the abstract syntax tree for the source file, after
all stages of compilation occur.

The '-annotate' option generates annotated assembly.  That is,
assembly is generated with comments describing what each instruction
does, its relevance to the original C source code, and so forth.
Additional comments are inserted to delimit functions, control
structures, and so forth.

---------------------------------------------------------------
THE MAKEFILE
---------------------------------------------------------------

The makefile just compiles the two files in the samples/ directory and
outputs an executable called 'foo' into this directory (all other
output files are also placed here).

Note that while compiling this, you may receive a bunch of warnings
mentioning something about an "Illegal character: ''".  This is just
an artifact of newline translation differences between platforms and
should be ignored.
