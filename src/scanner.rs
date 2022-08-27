pub struct Scanner<'a> {
    start: &'a str,
    current: &'a str,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            start: source,
            line: 1,
            current: source,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current.is_empty()
    }

    pub fn advance(&mut self) -> char {
        let (idx, char) = self.current.char_indices().next().unwrap();
        self.current = &self.current[(idx + char.len_utf8())..];
        return char;
    }

    fn peek(&self) -> Option<char> {
        self.current.chars().next()
    }
    fn peek_next(&self) -> Option<char> {
        self.current.chars().nth(1)
    }

    fn match_token(&mut self, expected: char) -> bool {
        let matches = self.current.starts_with(expected);
        if matches {
            self.advance();
        }
        matches
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                Some(' ' | '\r' | '\t') => {
                    self.advance();
                }
                Some('\n') => {
                    self.line += 1;
                    self.advance();
                    break;
                }
                Some('/') => {
                    if self.peek_next() == Some('/') {
                        while !matches!(self.peek(), Some('\n') | None) {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => break,
            }
        }
    }

    fn string<'b>(&'b mut self) -> Token<'a> {
        while !matches!(self.peek(), Some('"') | None) {
            if self.peek().unwrap() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance();

        self.make_token(TokenType::String)
    }

    fn number<'b>(&'b mut self) -> Token<'a> {
        fn is_digit(c: Option<char>) -> bool {
            match c {
                Some(c) if c.is_ascii_digit() => true,
                _ => false,
            }
        }

        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == Some('.') && is_digit(self.peek_next()) {
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier<'b>(&'b mut self) -> Token<'a> {
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
            self.advance();
        }

        let token_type = self.identifier_type();
        self.make_token(token_type)
    }

    fn identifier_type(&mut self) -> TokenType {
        match (
            self.start.chars().next().unwrap(),
            self.start.chars().nth(1),
        ) {
            ('a', _) => self.check_keyword(1, "nd", TokenType::And),
            ('c', _) => self.check_keyword(1, "lass", TokenType::Class),
            ('e', _) => self.check_keyword(1, "lse", TokenType::Else),
            ('i', _) => self.check_keyword(1, "f", TokenType::If),
            ('n', _) => self.check_keyword(1, "il", TokenType::Nil),
            ('o', _) => self.check_keyword(1, "r", TokenType::Or),
            ('p', _) => self.check_keyword(1, "rint", TokenType::Print),
            ('r', _) => self.check_keyword(1, "eturn", TokenType::Return),
            ('s', _) => self.check_keyword(1, "uper", TokenType::Super),
            ('v', _) => self.check_keyword(1, "ar", TokenType::Var),
            ('w', _) => self.check_keyword(1, "hile", TokenType::While),
            ('f', Some('a')) => self.check_keyword(2, "lse", TokenType::False),
            ('f', Some('o')) => self.check_keyword(2, "r", TokenType::For),
            ('f', Some('u')) => self.check_keyword(2, "n", TokenType::Fun),
            ('t', Some('r')) => self.check_keyword(2, "ue", TokenType::True),
            ('t', Some('h')) => self.check_keyword(2, "is", TokenType::This),
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(&self, start: usize, rest: &str, tt: TokenType) -> TokenType {
        if self.start.len() - self.current.len() == start + rest.len()
            && self.start[start..].starts_with( rest)
        {
            tt
        } else {
            TokenType::Identifier
        }
    }

    pub fn scan_token<'b>(&'b mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let token_type = if self.match_token('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.make_token(token_type)
            }
            '=' => {
                let token_type = if self.match_token('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.make_token(token_type)
            }
            '<' => {
                let token_type = if self.match_token('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.make_token(token_type)
            }
            '>' => {
                let token_type = if self.match_token('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.make_token(token_type)
            }
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => self.error_token("Unexected character."),
        }
    }

    fn make_token<'b>(&'b self, token_type: TokenType) -> Token<'a> {
        Token {
            token_type,
            source: &self.start[..(self.start.len() - self.current.len())],
            line: self.line,
        }
    }

    fn error_token<'b>(&'b self, message: &'a str) -> Token<'a> {
        Token {
            token_type: TokenType::Error,
            source: message,
            line: self.line,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub source: &'a str,
    pub line: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // one or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // literals.
    Identifier,
    String,
    Number,
    // keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

