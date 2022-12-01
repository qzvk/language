To convert an input file to a struct which can be executed by the interpreter, multiple transformations are performed:

# Lexing

Converts a string into a sequence of tokens.

# Parsing

Converts a sequence of tokens into an abstract syntax tree. This is broken into two sub-transformations:

## Parse tree generation

Converts the sequence of tokens into a parse tree.

## Abstraction

Converts the parse tree into an abstract syntax tree.

# IR generation

Finally, the AST is converted into the Internal Representation, which can be used for interpretation.
