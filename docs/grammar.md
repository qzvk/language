The grammar of the language described in McKeeman form:

```
document
    mul-expr
    assignment-seq mul-expr

assignment-seq
    assignment
    assignment assignment-seq

assignment
    ident-seq '=' mul-expr ';'

ident-seq
    ident
    ident ident-seq

mul-expr
    mul-expr '*' add-expr
    mul-expr '/' add-expr
    add-expr

add-expr
    add-expr '+' apply-expr
    add-expr '-' apply-expr
    apply-expr

apply-expr
    apply-expr unary-expr
    unary-expr

unary-expr
    '(' mul-expr ')'
    integer
    ident
```
