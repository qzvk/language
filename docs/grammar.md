# Summary

A string in the language is parsed as a sequence of function definitions, separated by semicolons. The string may be terminated by a semicolon.

A function definition consists of a head and body, with an equals in between. A function head consists of an identifier - the name of the function - followed by zero or more additional identifiers - the function arguments. A function body is an expression. Accepted expressions are:
- Argument names (eg. `x`)
- Function names (eg. `map`)
- Integers (eg. `1234`)
- Basic operators (ie. `+`, `-`, `/`, `*`)
- Parentheses (eg. `(2 * 4)`)
- Application (eg. `f a b`)

# Examples

```
main = 4;
```

```
square n = n * n;
main = square 5
```

```
twice f x = f (f x);
double n = n * 2;
main = twice double 4;
```
