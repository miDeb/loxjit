use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    chunk::{Chunk, OpCode},
    common::DEBUG_PRINT_CODE,
    interned_strings::StringInterner,
    scanner::{Scanner, Token, TokenType},
    value::Value,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

type ParseFn<'a, 'b> = fn(&mut Parser<'a, 'b>, can_assign: bool);

struct ParseRule<'a, 'b> {
    prefix: Option<ParseFn<'a, 'b>>,
    infix: Option<ParseFn<'a, 'b>>,
    precedence: Precedence,
}

impl<'a, 'b> ParseRule<'a, 'b> {
    fn get_rule(token_type: TokenType) -> ParseRule<'a, 'b> {
        match token_type {
            TokenType::LeftParen => ParseRule {
                prefix: Some(Parser::<'a, 'b>::grouping),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RightParen => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::LeftBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::RightBrace => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Comma => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Dot => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Minus => ParseRule {
                prefix: Some(Parser::<'a, 'b>::unary),
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Term,
            },
            TokenType::Semicolon => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Slash => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Factor,
            },
            TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Factor,
            },
            TokenType::Bang => ParseRule {
                prefix: Some(Parser::<'a, 'b>::unary),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Equal => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Greater => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::GreaterEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::Less => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::LessEqual => ParseRule {
                prefix: None,
                infix: Some(Parser::<'a, 'b>::binary),
                precedence: Precedence::Comparison,
            },
            TokenType::Identifier => ParseRule {
                prefix: Some(Parser::<'a, 'b>::variable),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::String => ParseRule {
                prefix: Some(Parser::<'a, 'b>::string),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Number => ParseRule {
                prefix: Some(Parser::<'a, 'b>::number),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::And => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Class => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Else => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::False => ParseRule {
                prefix: Some(Parser::<'a, 'b>::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::For => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Fun => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::If => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Nil => ParseRule {
                prefix: Some(Parser::<'a, 'b>::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Or => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Print => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Return => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Super => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::This => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::True => ParseRule {
                prefix: Some(Parser::<'a, 'b>::literal),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Var => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::While => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Error => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
}

pub struct Parser<'a, 'b> {
    current: Token<'a>,
    previous: Token<'a>,
    scanner: Scanner<'a>,
    chunk: &'b mut Chunk,
    had_error: bool,
    panic_mode: bool,
    interned_strings: &'b mut StringInterner,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(
        source: &'a str,
        chunk: &'b mut Chunk,
        interned_strings: &'b mut StringInterner,
    ) -> Self {
        Self {
            current: Token {
                token_type: TokenType::Error,
                source: "",
                line: 0,
            },
            previous: Token {
                token_type: TokenType::Error,
                source: "",
                line: 0,
            },
            scanner: Scanner::new(source),
            chunk,
            had_error: false,
            panic_mode: false,
            interned_strings,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;
        loop {
            self.current = self.scanner.scan_token();
            if !matches!(self.current.token_type, TokenType::Error) {
                break;
            }

            self.error_at_current(self.current.source);
        }
    }

    fn consume(&mut self, tt: TokenType, message: &str) {
        if self.current.token_type == tt {
            self.advance();
        } else {
            self.error_at_current(message)
        }
    }

    fn error(&mut self, message: &str) {
        self.error_at(self.previous, message)
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current, message)
    }

    fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        print!("[line {}] Error", token.line,);
        match token.token_type {
            TokenType::Eof => print!(" at end"),
            TokenType::Error => {}
            _ => print!(" at {}", token.source),
        }
        println!(": {}", message);
        self.had_error = true;
    }

    fn emit_byte(&mut self, byte: impl Into<u8>) {
        self.chunk.push(byte.into(), self.previous.line)
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if DEBUG_PRINT_CODE {
            if !self.had_error {
                self.chunk.disassemble("code")
            }
        }
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return)
    }

    fn emit_bytes(&mut self, byte1: impl Into<u8>, byte2: impl Into<u8>) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self, can_assign: bool) {
        let value = Value::Number(self.previous.source.parse().unwrap());
        self.emit_constant(value)
    }

    fn grouping(&mut self, can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, can_assign: bool) {
        let operator_type = self.previous.token_type;

        self.parse_precedence(Precedence::Unary);
        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::Not),
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => (),
        }
    }

    fn binary(&mut self, can_assign: bool) {
        let operator_type = self.previous.token_type;
        let rule = ParseRule::get_rule(operator_type);
        self.parse_precedence((Into::<u8>::into(rule.precedence) + 1).try_into().unwrap());

        match operator_type {
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Greater, OpCode::Not),
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Less, OpCode::Not),
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Sub),
            TokenType::Star => self.emit_byte(OpCode::Mul),
            TokenType::Slash => self.emit_byte(OpCode::Div),
            _ => unreachable!(),
        }
    }

    fn literal(&mut self, can_assign: bool) {
        match self.previous.token_type {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    fn string(&mut self, can_assign: bool) {
        let obj = self
            .interned_strings
            .put(self.previous.source[1..self.previous.source.len() - 1].to_string());
        self.emit_constant(Value::Obj(obj))
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous.source, can_assign);
    }

    fn named_variable(&mut self, name: &str, can_assign: bool) {
        let arg = self.identifier_constant(name);
        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.emit_bytes(OpCode::SetGlobal, arg);
        } else {
            self.emit_bytes(OpCode::GetGlobal, arg);
        }
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn identifier_constant(&mut self, name: &str) -> u8 {
        let obj = self.interned_strings.put(name.to_string());
        self.make_constant(Value::Obj(obj))
    }

    fn parse_variable(&mut self, message: &str) -> u8 {
        self.consume(TokenType::Identifier, message);
        self.identifier_constant(self.previous.source)
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");
        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn define_variable(&mut self, global: u8) {
        self.emit_bytes(OpCode::DefineGlobal, global);
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let Some(rule) = ParseRule::get_rule(self.previous.token_type).prefix else {
            self.error("Expect expression.");
            return;
        };

        let can_assign = precedence <= Precedence::Assignment;

        rule(self, can_assign);

        while precedence <= ParseRule::get_rule(self.current.token_type).precedence {
            self.advance();
            let infix_rule = ParseRule::get_rule(self.previous.token_type).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant, constant)
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        self.chunk.push_constant(value).unwrap_or_else(|_| {
            self.error("Too many constants in one chunk.");
            return 0;
        })
    }

    fn match_token(&mut self, tt: TokenType) -> bool {
        if self.check(tt) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&mut self, tt: TokenType) -> bool {
        self.current.token_type == tt
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.token_type != TokenType::Eof {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }
            match self.current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    pub fn compile(&mut self) -> Result<(), ()> {
        self.advance();

        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }

        self.end_compiler();
        if self.had_error {
            Err(())
        } else {
            Ok(())
        }
    }
}
