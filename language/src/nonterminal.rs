#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// A nonterminal used during parse tree generation.
pub enum Nonterminal {
    /// The start symbol, representing an entire input.
    Start,

    /// A sequence of assignments.
    AssignmentSeq,

    /// A single assignment.
    Assignment,

    /// An additive expression.
    AddExpr,

    /// A multiplicative expression.
    MulExpr,

    /// A function application expression.
    ApplyExpr,

    /// A unary expression (parenthesized expression, identifier or literal).
    UnaryExpr,

    /// An additive operation (plus or minus).
    AddOp,

    /// A multiplicative operation (multiply or divide).
    MulOp,
}

impl grammar::Nonterminal for Nonterminal {
    type Iterator = std::array::IntoIter<Self, 9>;

    const COUNT: usize = 9;

    fn all() -> Self::Iterator {
        [
            Self::Start,
            Self::AssignmentSeq,
            Self::Assignment,
            Self::AddExpr,
            Self::MulExpr,
            Self::ApplyExpr,
            Self::UnaryExpr,
            Self::AddOp,
            Self::MulOp,
        ]
        .into_iter()
    }

    fn index(self) -> usize {
        self as usize
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::Start,
            1 => Self::AssignmentSeq,
            2 => Self::Assignment,
            3 => Self::AddExpr,
            4 => Self::MulExpr,
            5 => Self::ApplyExpr,
            6 => Self::UnaryExpr,
            7 => Self::AddOp,
            8 => Self::MulOp,
            _ => panic!(),
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            Self::Start => "start",
            Self::AssignmentSeq => "assignment-seq",
            Self::Assignment => "assignment",
            Self::AddExpr => "add-expr",
            Self::MulExpr => "mul-expr",
            Self::ApplyExpr => "apply-expr",
            Self::UnaryExpr => "unary-expr",
            Self::AddOp => "add-op",
            Self::MulOp => "mul-op",
        }
    }
}
