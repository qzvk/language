The grammar of the language described in McKeeman form:

```
start
    add-expr
    assignment-seq add-expr

assignment-seq
    assignment
    assignment assignment-seq

assignment
    ident-seq '=' add-expr ';'

ident-seq
    ident
    ident ident-seq

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
