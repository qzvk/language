use ir::{Error, Ir, Value};
use parser::Ast;

fn make_ast(input: &str) -> Ast {
    let tokens = lexer::lex(input);
    parser::parse(tokens).unwrap()
}

fn make_ir(input: &str) -> Ir {
    let ast = make_ast(input);
    Ir::new(ast).unwrap()
}

fn evaluate_str(input: &str) -> Result<Value, Error> {
    let ir = make_ir(input);
    ir::evaluate(ir)
}

#[test]
fn can_evaluate_integer() {
    let value = evaluate_str("main = 0;").unwrap();
    assert_eq!(Value::Integer(0), value);
}

#[test]
fn can_evaluate_operators() {
    let value = evaluate_str("main = 1200 + 34;").unwrap();
    assert_eq!(Value::Integer(1200 + 34), value);

    let value = evaluate_str("main = 999 - 321;").unwrap();
    assert_eq!(Value::Integer(999 - 321), value);

    let value = evaluate_str("main = 10 * 25;").unwrap();
    assert_eq!(Value::Integer(10 * 25), value);

    let value = evaluate_str("main = 130 / 4;").unwrap();
    assert_eq!(Value::Integer(130 / 4), value);

    let value = evaluate_str("main = ((2 * 5) + 4) * (5 - 1) / 2;").unwrap();
    assert_eq!(Value::Integer(((2 * 5) + 4) * (5 - 1) / 2), value);
}

#[test]
fn can_evaluate_simple_function() {
    let value = evaluate_str("value = 1234; main = value;").unwrap();
    assert_eq!(Value::Integer(1234), value);
}
