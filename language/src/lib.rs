#![warn(missing_docs)]
#![forbid(unsafe_code)]

//! Root crate for the project, containing definitions and structures common between crates.

mod nonterminal;
mod parse_table;
mod token_info;
mod token_kind;

use grammar::ParseTree;

pub use crate::{
    nonterminal::Nonterminal, parse_table::parse_table, token_info::TokenInfo,
    token_kind::TokenKind,
};

/// A `start` node.
#[derive(Debug)]
pub struct Start<'a> {
    assignments: AssignmentSeq<'a>,
}

impl<'a> Start<'a> {
    /// Create a new start node from an assignment sequence.
    pub fn new(assignments: AssignmentSeq<'a>) -> Self {
        Self { assignments }
    }
}

/// An `assignment-seq` node.
#[derive(Debug)]
pub struct AssignmentSeq<'a> {
    left: Assignment<'a>,
    right: Option<Box<Self>>,
}

impl<'a> AssignmentSeq<'a> {
    /// Create a new assignment sequence node from an an assignment and optional sequence.
    pub fn new(left: Assignment<'a>, right: Option<Box<Self>>) -> Self {
        Self { left, right }
    }
}

/// An `assignment` node.
#[derive(Debug)]
pub struct Assignment<'a> {
    left: Option<ApplyExpr<'a>>,
    right: AddExpr<'a>,
}

impl<'a> Assignment<'a> {
    /// Create an assignment from a body and optional head.
    pub fn new(left: Option<ApplyExpr<'a>>, right: AddExpr<'a>) -> Self {
        Self { left, right }
    }
}

/// An `add-expr` node.
#[derive(Debug)]
pub struct AddExpr<'a> {
    left: Option<(Box<AddExpr<'a>>, AddOp<'a>)>,
    right: MulExpr<'a>,
}

impl<'a> AddExpr<'a> {
    /// Create an addition expression from a right-hand side and optional left-hand.
    pub fn new(left: Option<(Box<AddExpr<'a>>, AddOp<'a>)>, right: MulExpr<'a>) -> Self {
        Self { left, right }
    }
}

/// A `mul-exr` node.
#[derive(Debug)]
pub struct MulExpr<'a> {
    left: Option<(Box<MulExpr<'a>>, MulOp<'a>)>,
    right: ApplyExpr<'a>,
}

impl<'a> MulExpr<'a> {
    /// Create a multiplication expression from a right-hand side and optional left-hand.
    pub fn new(left: Option<(Box<MulExpr<'a>>, MulOp<'a>)>, right: ApplyExpr<'a>) -> Self {
        Self { left, right }
    }
}

/// An `apply-exr` node.
#[derive(Debug)]
pub struct ApplyExpr<'a> {
    left: Option<Box<ApplyExpr<'a>>>,
    right: UnaryExpr<'a>,
}

impl<'a> ApplyExpr<'a> {
    /// Create an application expression from a right-hand side and optional left-hand.
    pub fn new(left: Option<Box<ApplyExpr<'a>>>, right: UnaryExpr<'a>) -> Self {
        Self { left, right }
    }
}

/// A `unary-exr` node.
#[derive(Debug)]
pub enum UnaryExpr<'a> {
    /// A parenthesized expression.
    AddExpr(Box<AddExpr<'a>>),

    /// An integer.
    Integer(TokenInfo<'a>),

    /// An identifier.
    Ident(TokenInfo<'a>),
}

/// An `add-op` node.
#[derive(Debug)]
pub enum AddOp<'a> {
    /// Addition
    Add(TokenInfo<'a>),

    /// Subtraction
    Subtract(TokenInfo<'a>),
}

/// A `mul-op` node.
#[derive(Debug)]
pub enum MulOp<'a> {
    /// Multiplication
    Multiply(TokenInfo<'a>),

    /// Division
    Divide(TokenInfo<'a>),
}

/// Any possible parse tree node.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Node<'a> {
    Start(Start<'a>),
    AssignmentSeq(AssignmentSeq<'a>),
    Assignment(Assignment<'a>),
    AddExpr(AddExpr<'a>),
    MulExpr(MulExpr<'a>),
    ApplyExpr(ApplyExpr<'a>),
    UnaryExpr(UnaryExpr<'a>),
    AddOp(AddOp<'a>),
    MulOp(MulOp<'a>),
}

/// Reduction operation as defined by the language grammar.
///
/// TODO: Serious cleanup.
pub fn reduce<'a>(
    head: Nonterminal,
    mut body: Vec<ParseTree<Node<'a>, TokenKind, TokenInfo<'a>>>,
) -> Node<'a> {
    match head {
        Nonterminal::Start => match body.pop() {
            Some(ParseTree::Nonterminal(Node::AssignmentSeq(assignment_seq))) => {
                Node::Start(Start::new(assignment_seq))
            }

            _ => panic!(),
        },

        Nonterminal::AssignmentSeq => {
            assert!((body.len() == 3) || (body.len() == 1));

            match body.pop() {
                Some(ParseTree::Nonterminal(Node::Assignment(assignment))) => {
                    Node::AssignmentSeq(AssignmentSeq::new(assignment, None))
                }

                Some(ParseTree::Nonterminal(Node::AssignmentSeq(assignment_seq))) => {
                    assert!(matches!(
                        body.pop(),
                        Some(ParseTree::Terminal(TokenKind::Semicolon, _))
                    ));

                    match body.pop() {
                        Some(ParseTree::Nonterminal(Node::Assignment(assignment))) => {
                            Node::AssignmentSeq(AssignmentSeq::new(
                                assignment,
                                Some(Box::new(assignment_seq)),
                            ))
                        }
                        _ => panic!(),
                    }
                }

                _ => panic!(),
            }
        }

        Nonterminal::Assignment => {
            assert!((body.len() == 3) || (body.len() == 1));

            let right = match body.pop() {
                Some(ParseTree::Nonterminal(Node::AddExpr(add_expr))) => add_expr,
                _ => panic!(),
            };

            match body.pop() {
                Some(ParseTree::Terminal(TokenKind::Equals, _)) => {
                    let left = match body.pop() {
                        Some(ParseTree::Nonterminal(Node::ApplyExpr(apply_expr))) => apply_expr,
                        _ => panic!(),
                    };

                    Node::Assignment(Assignment::new(Some(left), right))
                }

                None => Node::Assignment(Assignment::new(None, right)),

                _ => panic!(),
            }
        }

        Nonterminal::AddExpr => {
            assert!((body.len() == 3) || (body.len() == 1));

            let right = match body.pop() {
                Some(ParseTree::Nonterminal(Node::MulExpr(mul_expr))) => mul_expr,
                _ => panic!(),
            };

            match body.pop() {
                Some(ParseTree::Nonterminal(Node::AddOp(op))) => match body.pop() {
                    Some(ParseTree::Nonterminal(Node::AddExpr(left))) => {
                        Node::AddExpr(AddExpr::new(Some((Box::new(left), op)), right))
                    }

                    _ => panic!(),
                },

                None => Node::AddExpr(AddExpr::new(None, right)),

                _ => panic!(),
            }
        }

        Nonterminal::MulExpr => {
            assert!((body.len() == 3) || (body.len() == 1));

            let right = match body.pop() {
                Some(ParseTree::Nonterminal(Node::ApplyExpr(apply_expr))) => apply_expr,
                _ => panic!(),
            };

            match body.pop() {
                Some(ParseTree::Nonterminal(Node::MulOp(op))) => match body.pop() {
                    Some(ParseTree::Nonterminal(Node::MulExpr(left))) => {
                        Node::MulExpr(MulExpr::new(Some((Box::new(left), op)), right))
                    }
                    _ => panic!(),
                },
                None => Node::MulExpr(MulExpr::new(None, right)),
                _ => panic!(),
            }
        }

        Nonterminal::ApplyExpr => {
            assert!((body.len() == 2) || (body.len() == 1));

            let right = match body.pop() {
                Some(ParseTree::Nonterminal(Node::UnaryExpr(unary_expr))) => unary_expr,
                _ => panic!(),
            };

            match body.pop() {
                Some(ParseTree::Nonterminal(Node::ApplyExpr(left))) => {
                    Node::ApplyExpr(ApplyExpr::new(Some(Box::new(left)), right))
                }
                None => Node::ApplyExpr(ApplyExpr::new(None, right)),
                _ => panic!(),
            }
        }

        Nonterminal::UnaryExpr => {
            assert!((body.len() == 3) || (body.len() == 1));
            match body.pop() {
                Some(ParseTree::Terminal(TokenKind::Ident, info)) => {
                    Node::UnaryExpr(UnaryExpr::Ident(info))
                }

                Some(ParseTree::Terminal(TokenKind::Integer, info)) => {
                    Node::UnaryExpr(UnaryExpr::Integer(info))
                }

                Some(ParseTree::Terminal(TokenKind::CloseParen, _)) => match body.pop() {
                    Some(ParseTree::Nonterminal(Node::AddExpr(add_expr))) => {
                        Node::UnaryExpr(UnaryExpr::AddExpr(Box::new(add_expr)))
                    }

                    _ => panic!(),
                },

                _ => panic!(),
            }
        }

        Nonterminal::AddOp => {
            assert_eq!(body.len(), 1);
            match body.pop() {
                Some(ParseTree::Terminal(TokenKind::Plus, info)) => Node::AddOp(AddOp::Add(info)),
                Some(ParseTree::Terminal(TokenKind::Minus, info)) => {
                    Node::AddOp(AddOp::Subtract(info))
                }
                _ => panic!(),
            }
        }

        Nonterminal::MulOp => {
            assert_eq!(body.len(), 1);
            match body.pop() {
                Some(ParseTree::Terminal(TokenKind::Asterisk, info)) => {
                    Node::MulOp(MulOp::Multiply(info))
                }
                Some(ParseTree::Terminal(TokenKind::Slash, info)) => {
                    Node::MulOp(MulOp::Divide(info))
                }
                _ => panic!(),
            }
        }
    }
}
