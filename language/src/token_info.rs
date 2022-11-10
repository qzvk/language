/// Token (or lexical error) position information.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenInfo<'a> {
    line: u32,
    column: u32,
    source: &'a str,
}

impl<'a> TokenInfo<'a> {
    /// Create new token info, with the given `line`, `column`, and string of associated `source`.
    pub const fn new(line: u32, column: u32, source: &'a str) -> Self {
        Self {
            line,
            column,
            source,
        }
    }

    /// The line of a token.
    pub fn line(&self) -> u32 {
        self.line
    }

    /// The column the token begins on.
    pub fn column(&self) -> u32 {
        self.column
    }
}

impl<'a> std::fmt::Display for TokenInfo<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Line and column numbers are 1-indexed, since they're for humans.
        write!(f, "{}:{} {:?}", self.line + 1, self.column + 1, self.source)
    }
}
