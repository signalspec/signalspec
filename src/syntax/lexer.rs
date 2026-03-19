use std::str::Chars;

use crate::{FileSpan, syntax::FilePos};

#[derive(Debug)]
pub struct Tokens<'a> {
    pub input: &'a str,
    pub tokens: Vec<Token>,

    /// Positions of the first byte of each token, and the byte after the end of the token.
    ///
    /// Whitespace and comments are not included in tokens, so they may not be contiguous.
    pub positions: Vec<FileSpan>,
}

impl Tokens<'_> {
    pub fn get_span(&self, token_pos: usize) -> FileSpan {
        self.positions.get(token_pos).copied().unwrap_or(
            FileSpan::new(self.input.len(), self.input.len())
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Token {
    Unexpected = 0,
    /// End of line whose final token can end a statement (see `can_end_statement`).
    NewlineSep,
    Identifier,

    /// `#x`. First char is `#`.
    SymbolLiteral,
    NumberLiteral,

    /// `'0000'`. First and last chars are `'`.
    BareBitsLiteral,

    /// `b'0000'`. First char is `b`, second and last chars are `'`.
    BinaryBitsLiteral,

    /// h'0000'. First char is `h`, second and last chars are `'`.
    HexBitsLiteral,

    /// `"string"`. First and last char is `"`. May include escape sequences.
    StringLiteral,

    // Individual characters
    Bang = b'!',
    Star = b'*',
    Plus = b'+',
    Comma = b',',
    Minus = b'-',
    Dot = b'.',
    Slash = b'/',
    Colon = b':',
    Semicolon = b';',
    Equals = b'=',
    Lt = b'<',
    Gt = b'>',
    At = b'@',
    Pipe = b'|',
    Underscore = b'_',
    Question = b'?',
    Tilde = b'~',
    OpenParen = b'(',
    CloseParen = b')',
    OpenBrace = b'{',
    CloseBrace = b'}',
    OpenBracket = b'[',
    CloseBracket = b']',
    // Unused: $, %, &, ^, `

    // Compound symbols
    Arrow = 0x80, // `=>`
    DotDot, // ".."

    // Keywords
    KwAlt = 0x90,
    KwAny,
    KwBy,
    KwDef,
    KwFor,
    KwIs,
    KwLet,
    KwNew,
    KwOn,
    KwPrimitive,
    KwProtocol,
    KwRepeat,
    KwSeq,
    KwType,
    KwVar,
    KwWith,
}

impl Token {
    pub fn describe(self) -> &'static str {
        match self {
            Token::Unexpected => "",
            Token::NewlineSep => "newline",
            Token::Identifier => "identifier",
            Token::SymbolLiteral => "symbol literal",
            Token::NumberLiteral => "number literal",
            Token::BareBitsLiteral | Token::BinaryBitsLiteral | Token::HexBitsLiteral => "bits literal",
            Token::StringLiteral => "string literal",
            Token::Bang => "'!'",
            Token::Star => "'*'",
            Token::Plus => "'+'",
            Token::Comma => "','",
            Token::Minus => "'-'",
            Token::Dot => "'.'",
            Token::Slash => "'/'",
            Token::Colon => "':'",
            Token::Semicolon => "';'",
            Token::Equals => "'='",
            Token::Lt => "'<'",
            Token::Gt => "'>'",
            Token::At => "'@'",
            Token::Pipe => "'|'",
            Token::Underscore => "'_'",
            Token::Question => "'?'",
            Token::Tilde => "'~'",
            Token::OpenParen => "'('",
            Token::CloseParen => "')'",
            Token::OpenBrace => "'{'",
            Token::CloseBrace => "'}'",
            Token::OpenBracket => "'['",
            Token::CloseBracket => "']'",
            Token::Arrow => "'=>'",
            Token::DotDot => "'..'",
            Token::KwAlt => "alt",
            Token::KwAny => "any",
            Token::KwBy => "by",
            Token::KwDef => "def",
            Token::KwFor => "for",
            Token::KwIs => "is",
            Token::KwLet => "let",
            Token::KwNew => "new",
            Token::KwOn => "on",
            Token::KwPrimitive => "primitive",
            Token::KwProtocol => "protocol",
            Token::KwRepeat => "repeat",
            Token::KwSeq => "seq",
            Token::KwType => "type",
            Token::KwVar => "var",
            Token::KwWith => "with",
        }
    }

    /// True if an expression or action can end with this token, so a newline
    /// after it terminates the node.
    ///
    /// Lines ending in other tokens (operators, commas, open delimiters,
    /// keywords) continue onto the next line, and no newline token is emitted.
    fn can_end_statement(self) -> bool {
        matches!(self,
            Token::Identifier | Token::Underscore
            | Token::NumberLiteral | Token::SymbolLiteral | Token::StringLiteral
            | Token::BareBitsLiteral | Token::BinaryBitsLiteral | Token::HexBitsLiteral
            | Token::CloseParen | Token::CloseBracket | Token::CloseBrace
        )
    }
}

struct Cursor<'a> {
    input: &'a str,
    pos: u32,
    chars: Chars<'a>,
}

impl <'a> Cursor<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            chars: input.chars(),
        }
    }

    fn first(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn bump(&mut self) -> Option<char>{
        self.pos += 1;
        self.chars.next()
    }

    fn eat(&mut self, expected: char) -> bool {
        if self.first() == Some(expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_one(&mut self, f: impl Fn(char) -> bool) -> bool {
        if let Some(c) = self.first() && f(c) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_two(&mut self, f: impl Fn(char, char) -> bool) -> bool {
        let mut chars = self.chars.clone();
        if let Some(c1) = chars.next() && let Some(c2) = chars.next() && f(c1, c2) {
            self.bump();
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_while(&mut self, f: impl Fn(char) -> bool) {
        while let Some(c) = self.first() && f(c) {
            self.bump();
        }
    }

    fn pos(&self) -> u32 {
        self.pos
    }

    fn since(&self, pos: u32) -> &'a str {
        &self.input[pos as usize..self.pos as usize]
    }
}

fn is_whitespace(c: char) -> bool {
    matches!(c, '\t' | '\u{0B}' | '\u{0C}' | '\r' | ' ')
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

pub fn tokenize(input: &str) -> Tokens<'_> {
    let mut state = Cursor::new(input);

    let mut tokens: Vec<Token> = Vec::new();
    let mut positions = Vec::new();

    loop {
        let start = FilePos(state.pos());
        let Some(c) = state.bump() else {
            break;
        };

        let token = match c {
            c if is_whitespace(c) => {
                state.eat_while(is_whitespace);
                continue;
            },

            '/' if state.eat('/') => {
                state.eat_while(|c| c != '\n');
                continue;
            },


            // Golang-style separator insertion: if the previous token can end an expression or action,
            // this newline gets a token for the parser to use as a separator.
            '\n' if tokens.last().is_some_and(|t| t.can_end_statement()) => Token::NewlineSep,
            '\n' => continue,

            '#' if state.eat_one(is_ident_start) => {
                state.eat_while(is_ident_continue);
                Token::SymbolLiteral
            }

            c if c.is_ascii_digit() => { // Number literal
                state.eat_while(|c| c.is_ascii_digit());
                if state.eat_two(|c1, c2| c1 == '.' && c2.is_ascii_digit()) { // Need to not match `1..2`
                    state.eat_while(|c| c.is_ascii_digit());
                }
                Token::NumberLiteral
            }

            '\'' => { // Bare bits literal
                state.eat_while(|c| matches!(c, '0' | '1' | '_'));
                Token::BareBitsLiteral
            }

            'b' if state.eat('\'') => { // Binary bits literal
                state.eat_while(|c| matches!(c, '0' | '1' | '_'));
                Token::BinaryBitsLiteral
            }

            'h' if state.eat('\'') => { // Hex bits literal
                state.eat_while(|c| c.is_ascii_hexdigit() || c == '_');
                Token::HexBitsLiteral
            }

            '"' => { // String literal
                loop {
                    match state.bump() {
                        Some('\\') => { state.bump(); }, // Skip escaped char
                        Some('"') => break Token::StringLiteral, // End of string
                        Some('\n') | None => break Token::Unexpected, // Unterminated string
                        _ => {},
                    }
                }
            }

            '_' => Token::Underscore,

            c if is_ident_start(c) => {
                state.eat_while(is_ident_continue);
                match state.since(start.0) {
                    "alt" => Token::KwAlt,
                    "any" => Token::KwAny,
                    "by" => Token::KwBy,
                    "def" => Token::KwDef,
                    "for" => Token::KwFor,
                    "is" => Token::KwIs,
                    "let" => Token::KwLet,
                    "new" => Token::KwNew,
                    "on" => Token::KwOn,
                    "primitive" => Token::KwPrimitive,
                    "protocol" => Token::KwProtocol,
                    "repeat" => Token::KwRepeat,
                    "seq" => Token::KwSeq,
                    "type" => Token::KwType,
                    "var" => Token::KwVar,
                    "with" => Token::KwWith,
                    _ => Token::Identifier,
                }
            }

            '=' if state.eat('>') => Token::Arrow,
            '.' if state.eat('.') => Token::DotDot,

            '!' => Token::Bang,
            '*' => Token::Star,
            '+' => Token::Plus,
            ',' => Token::Comma,
            '-' => Token::Minus,
            '.' => Token::Dot,
            '/' => Token::Slash,
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            '=' => Token::Equals,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '@' => Token::At,
            '|' => Token::Pipe,
            '?' => Token::Question,
            '~' => Token::Tilde,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '[' => Token::OpenBracket,
            ']' => Token::CloseBracket,

            _ => Token::Unexpected,
        };

        let end = FilePos(state.pos());
        tokens.push(token);
        positions.push(FileSpan { start, end });
    }

    Tokens { input, tokens, positions }
}

impl peg::Parse for Tokens<'_> {
    type PositionRepr = usize;

    fn start<'input>(&'input self) -> usize {
        0
    }

    fn is_eof<'input>(&'input self, p: usize) -> bool {
        p >= self.tokens.len()
    }

    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr {
        p
    }
}

impl<'a> peg::ParseElem<'a> for Tokens<'a> {
    type Element = Token;

    fn parse_elem(&'a self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self.tokens.get(pos) {
            Some(&token) => peg::RuleResult::Matched(pos + 1, token),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'a> peg::ParseSlice<'a> for Tokens<'a> {
    type Slice = &'a str;

    fn parse_slice(&'a self, p1: usize, p2: usize) -> Self::Slice {
        let start = self.positions[p1].start.0 as usize;
        let end = self.positions[p2 - 1].end.0 as usize;
        &self.input[start..end]
    }
}


#[test]
fn test_tokens() {
    let t = tokenize("with Seq(type #a|#b, _) def test_constant_destructure() = seq {
     rxpeat aaa {
          aaa()
     }
     bbb()
  }
");
    for (i, (tok, span)) in t.tokens.iter().zip(t.positions).enumerate() {
        println!("{i:3} {span:3?} {tok:?} {:?}", &t.input[span.start.0 as usize..span.end.0 as usize]);
    }
}
