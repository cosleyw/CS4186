# C89 Compiler for CS4186
## About
This is a (WIP) compiler from C to x86 for the CS4186 (studies in compiler construction) course.

This is intended to be a Self-hosting partial implementation of C89, mostly just excluding the preprocessor.

currently it can parse pretty much all of c89, notably excluding typedefs to make the grammar context free.
codegen is working for some integer operations, while loops, and functions (with a few issues around global scope).

# Building
To build this project run
```
make
```

# usage
at the moment you can run
```
mycc file.c
```

and it will print out some assembly that you can paste into `test/test.S`
and then to assemble you would run
```
cd test
make clean all
```
resulting in a `test` executable assuming you have `muslc` and `nasm` installed and the paths are correct
i'm using `muslc` so i can statically link against it with one big RWX section.



# Rough Plan
~~I intend to do a "one-pass" architecture to start with, and then add an optimizer after I get that working.~~

# Todo
- lexing
  - might merge into the parsing stage...
- parsing
  - ~~LR(1)~~
    - Current formalisms of LR(k) parser generators hurt my feelings, I feel that there is an elegant implementation that involves canonacalized graphs, but I don't have the time to find it.
  - recursive descent
- syntactic analysis
  - Ast
    - mostly good
  - Types
    - started code, not checked.
- codegen
   - for loops
   - other operators
   - optimizer

