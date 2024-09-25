# C89 Compiler for CS4186
## About
This is a (WIP) compiler from C to x86 for the CS4186 (studies in compiler construction) course.

This is intended to be a Self-hosting partial implementation of C89, mostly just excluding the preprocessor.

# Building
To build this project run
```
make
```

# Rough Plan
I intend to do a "one-pass" architecture to start with, and then add an optimizer after I get that working.

# Todo
- lexing
  - might merge into the parsing stage...
- parsing
  - ~~LR(1)~~
    - Current formalisms of LR(k) parser generators hurt my feelings, I feel that there is an elegant implementation that involves canonacalized graphs, but I don't have the time to find it.
  - recursive descent -- started
    - Maybe it works?
- syntactic analysis
  - Ast
    - Somewhat done...
- codegen

