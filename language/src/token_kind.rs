/// A kind of token producable by lexical analysis.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    /// A plus (`+`)
    Plus,

    /// A minus (`-`)
    Minus,

    /// An asterisk (`*`)
    Asterisk,

    /// A slash (`/`)
    Slash,

    /// A semicolon (`;`)
    Semicolon,

    /// An equals (`=`)
    Equals,

    /// An open parenthesis (`(`)
    OpenParen,

    /// A close parenthesis (`)`)
    CloseParen,

    /// An integer (`/[0-9+]/`)
    Integer,

    /// An identifier (`/[a-zA-Z_][a-zA-Z0-9_]*/`)
    Ident,
}

impl grammar::Terminal for TokenKind {
    type Iterator = std::array::IntoIter<Self, 10>;

    const COUNT: usize = 10;

    fn all() -> Self::Iterator {
        [
            Self::Plus,
            Self::Minus,
            Self::Asterisk,
            Self::Slash,
            Self::Semicolon,
            Self::Equals,
            Self::OpenParen,
            Self::CloseParen,
            Self::Integer,
            Self::Ident,
        ]
        .into_iter()
    }

    fn index(self) -> usize {
        self as usize
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::Plus,
            1 => Self::Minus,
            2 => Self::Asterisk,
            3 => Self::Slash,
            4 => Self::Semicolon,
            5 => Self::Equals,
            6 => Self::OpenParen,
            7 => Self::CloseParen,
            8 => Self::Integer,
            9 => Self::Ident,
            _ => panic!(),
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            Self::Plus => "'+'",
            Self::Minus => "'-'",
            Self::Asterisk => "'*'",
            Self::Slash => "'/'",
            Self::Semicolon => "';'",
            Self::Equals => "'='",
            Self::OpenParen => "'('",
            Self::CloseParen => "')'",
            Self::Integer => "integer",
            Self::Ident => "ident",
        }
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenKind::Plus => "plus",
            TokenKind::Minus => "minus",
            TokenKind::Asterisk => "asterisk",
            TokenKind::Slash => "slash",
            TokenKind::Semicolon => "semicolon",
            TokenKind::Equals => "equals",
            TokenKind::OpenParen => "open-paren",
            TokenKind::CloseParen => "close-paren",
            TokenKind::Integer => "integer",
            TokenKind::Ident => "ident",
        };
        write!(f, "({string})")
    }
}
