The grammar of the language described in McKeeman form:

```
start
    assignment-seq

assignment-seq
    assignment
    assignment ';' assignment-seq

assignment
    apply-expr '=' add-expr
    add-expr

add-expr
    add-expr add-op mul-expr
    mul-expr

mul-expr
    mul-expr mul-op apply-expr
    apply-expr

apply-expr
    apply-expr unary-expr
    unary-expr

unary-expr
    '(' add-expr ')'
    integer
    ident

add-op
    '+'
    '-'

mul-op
    '*'
    '/'
```
