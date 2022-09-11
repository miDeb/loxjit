use std::fmt::Write;
use std::mem::MaybeUninit;

use num_enum::{IntoPrimitive, TryFromPrimitive};
use replace_with::replace_with_or_abort;

use crate::{
    chunk::{Chunk, OpCode},
    common::DEBUG_PRINT_CODE,
    errors::{CompileError, CompileResult},
    gc::{GarbageCollector, GcCell},
    object::{ObjFunction, ObjString},
    scanner::{Scanner, Token, TokenType},
    value::Value,
    vm::Vm,
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

type ParseFn<'a, 'b> = fn(&mut Parser<'a, 'b>, can_assign: bool) -> CompileResult<()>;

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
                infix: Some(Parser::<'a, 'b>::call),
                precedence: Precedence::Call,
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
                infix: Some(Parser::<'a, 'b>::and),
                precedence: Precedence::And,
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
                infix: Some(Parser::<'a, 'b>::or),
                precedence: Precedence::Or,
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

struct Local<'a> {
    name: &'a str,
    depth: i32,
    is_captured: bool,
}

#[derive(Clone, Copy)]
pub struct Upvalue {
    index: u8,
    is_local: bool,
}

#[derive(PartialEq, Eq)]
enum FunctionType {
    Function,
    Script,
}

struct Compiler<'a> {
    function: Box<ObjFunction>,
    fun_type: FunctionType,

    locals: Vec<Local<'a>>,
    scope_depth: i32,

    enclosing: Option<Box<Compiler<'a>>>,
    upvalues: [MaybeUninit<Upvalue>; u8::MAX as usize + 1],
}

impl<'a> Compiler<'a> {
    fn resolve_local(&mut self, name: &'a str) -> CompileResult<Option<u8>> {
        if let Some((i, local)) = self
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == name)
        {
            if local.depth == -1 {
                Err("Can't read local variable in its own initializer.".into())
            } else {
                Ok(Some(i as u8))
            }
        } else {
            Ok(None)
        }
    }

    fn resolve_upvalue(&mut self, name: &'a str) -> CompileResult<Option<u8>> {
        if let Some(enclosing) = &mut self.enclosing {
            if let Some(local) = enclosing.resolve_local(name)? {
                self.enclosing.as_mut().unwrap().locals[local as usize].is_captured = true;
                return self.add_upvalue(local, true).map(Some);
            }
            if let Some(upvalue) = enclosing.resolve_upvalue(name)? {
                return self.add_upvalue(upvalue, false).map(Some);
            }
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> CompileResult<u8> {
        let upvalue_count = self.function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = unsafe { self.upvalues[i].assume_init_ref() };
            if upvalue.index == index && upvalue.is_local == is_local {
                return Ok(i as u8);
            }
        }

        if upvalue_count == self.upvalues.len() {
            // TODO: fix error handling! This will not include line information.
            return Err("Too many closure variables in function.".into());
        }

        self.upvalues[upvalue_count].write(Upvalue { index, is_local });

        self.function.upvalue_count += 1;
        Ok(upvalue_count as u8)
    }
}

impl<'a> Compiler<'a> {
    fn new(
        fun_type: FunctionType,
        fun_name: Option<Box<str>>,
        enclosing: Option<Box<Compiler<'a>>>,
    ) -> Self {
        let mut compiler = Self {
            function: Box::new(ObjFunction::new(0, Chunk::new(), fun_name)),
            fun_type,
            locals: Vec::with_capacity(u8::MAX as usize + 1),
            scope_depth: 0,
            enclosing,
            upvalues: [MaybeUninit::uninit(); 256],
        };

        compiler.locals.push(Local {
            name: "",
            depth: 0,
            is_captured: false,
        });

        compiler
    }
}

pub struct Parser<'a, 'b> {
    current: Token<'a>,
    previous: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    compiler: Box<Compiler<'a>>,
    gc: &'b mut GarbageCollector<Vm, ObjString>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(source: &'a str, gc: &'b mut GarbageCollector<Vm, ObjString>) -> Self {
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
            had_error: false,
            compiler: Box::new(Compiler::new(FunctionType::Script, None, None)),
            gc,
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
    }

    fn advance(&mut self) -> CompileResult<()> {
        self.previous = self.current;
        loop {
            self.current = self.scanner.scan_token();
            if !matches!(self.current.token_type, TokenType::Error) {
                break;
            }

            return Err(self.error_at_current(self.current.source));
        }
        Ok(())
    }

    fn consume(&mut self, tt: TokenType, message: &str) -> CompileResult<()> {
        if self.current.token_type == tt {
            self.advance()?;
            Ok(())
        } else {
            Err(self.error_at_current(message))
        }
    }

    #[must_use]
    fn error(&mut self, message: &str) -> CompileError {
        self.error_at(self.previous, message)
    }

    #[must_use]
    fn error_at_current(&mut self, message: &str) -> CompileError {
        self.error_at(self.current, message)
    }

    #[must_use]
    fn error_at(&mut self, token: Token, message: &str) -> CompileError {
        self.had_error = true;
        let mut error = String::new();
        write!(&mut error, "[line {}] Error", token.line,).unwrap();
        match token.token_type {
            TokenType::Eof => write!(&mut error, " at end").unwrap(),
            TokenType::Error => {}
            _ => write!(&mut error, " at {}", token.source).unwrap(),
        }
        write!(&mut error, ": {}", message).unwrap();
        let mut error: CompileError = error.into();
        error.has_line_info = true;
        error
    }

    fn emit_byte(&mut self, byte: impl Into<u8>) {
        let line = self.previous.line;
        self.current_chunk().push(byte.into(), line)
    }

    fn emit_u16(&mut self, byte: impl Into<u16>) {
        let bytes = byte.into().to_ne_bytes();
        self.emit_bytes(bytes[0], bytes[1]);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if DEBUG_PRINT_CODE {
            if !self.had_error {
                let name = self
                    .compiler
                    .function
                    .name
                    .as_deref()
                    .unwrap_or("<script>")
                    .to_owned();
                self.current_chunk().disassemble(&name)
            }
        }
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while self
            .compiler
            .locals
            .last()
            .map(|local| local.depth > self.compiler.scope_depth)
            .unwrap_or(false)
        {
            let local = self.compiler.locals.last().unwrap();
            if local.is_captured {
                self.emit_byte(OpCode::CloseUpvalue);
            } else {
                self.emit_byte(OpCode::Pop);
            }
            self.compiler.locals.pop();
        }
    }

    fn emit_return(&mut self) {
        self.emit_bytes(OpCode::Nil, OpCode::Return)
    }

    fn emit_bytes(&mut self, byte1: impl Into<u8>, byte2: impl Into<u8>) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn expression(&mut self) -> CompileResult<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn block(&mut self) -> CompileResult<()> {
        while !(self.check(TokenType::RightBrace) || self.check(TokenType::Eof)) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")
    }

    fn number(&mut self, _can_assign: bool) -> CompileResult<()> {
        let value = Value::Number(self.previous.source.parse().unwrap());
        self.emit_constant(value)
    }

    fn grouping(&mut self, _can_assign: bool) -> CompileResult<()> {
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _can_assign: bool) -> CompileResult<()> {
        let operator_type = self.previous.token_type;

        self.parse_precedence(Precedence::Unary)?;
        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::Not),
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => (),
        }

        Ok(())
    }

    fn binary(&mut self, _can_assign: bool) -> CompileResult<()> {
        let operator_type = self.previous.token_type;
        let rule = ParseRule::get_rule(operator_type);
        self.parse_precedence((Into::<u8>::into(rule.precedence) + 1).try_into().unwrap())?;

        match operator_type {
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less, OpCode::Not),
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not),
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Sub),
            TokenType::Star => self.emit_byte(OpCode::Mul),
            TokenType::Slash => self.emit_byte(OpCode::Div),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn argument_list(&mut self) -> CompileResult<u8> {
        let mut count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression()?;
                if count == u8::MAX {
                    return Err(self.error("Can't have more than 255 arguments."));
                }
                count += 1;
                if !self.match_token(TokenType::Comma)? {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(count)
    }

    fn call(&mut self, _can_assign: bool) -> CompileResult<()> {
        let arg_count = self.argument_list()?;
        self.emit_bytes(OpCode::Call, arg_count);
        Ok(())
    }

    fn literal(&mut self, _can_assign: bool) -> CompileResult<()> {
        match self.previous.token_type {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn string(&mut self, _can_assign: bool) -> CompileResult<()> {
        let obj = self.gc.intern_string(ObjString::new(
            self.previous.source[1..self.previous.source.len() - 1].to_string(),
        ));
        self.emit_constant(Value::Obj(unsafe { obj.cast() }))
    }

    fn variable(&mut self, can_assign: bool) -> CompileResult<()> {
        self.named_variable(self.previous.source, can_assign)
    }

    fn named_variable(&mut self, name: &'a str, can_assign: bool) -> CompileResult<()> {
        let (arg, get_op, set_op) = if let Some(arg) = self.compiler.resolve_local(name)? {
            (arg, OpCode::GetLocal, OpCode::SetLocal)
        } else if let Some(arg) = self.compiler.resolve_upvalue(name)? {
            (arg, OpCode::GetUpvalue, OpCode::SetUpvalue)
        } else {
            (
                self.identifier_constant(name)?,
                OpCode::GetGlobal,
                OpCode::SetGlobal,
            )
        };
        if can_assign && self.match_token(TokenType::Equal)? {
            self.expression()?;
            self.emit_bytes(set_op, arg);
        } else {
            self.emit_bytes(get_op, arg);
        }
        Ok(())
    }

    fn declaration(&mut self) {
        let result = self
            .match_token(TokenType::Var)
            .map(|matched| {
                if matched {
                    self.var_declaration()
                } else {
                    self.statement()
                }
            })
            .flatten();

        if let Err(e) = result {
            if e.has_line_info {
                eprintln!("{}", e);
            } else {
                eprintln!("{}", self.error(&e.message));
            }
            self.synchronize();
        }
    }

    fn identifier_constant(&mut self, name: &str) -> CompileResult<u8> {
        let obj = self.gc.intern_string(ObjString::new(name.to_string()));
        self.make_constant(Value::Obj(unsafe { obj.cast() }))
    }

    fn add_local(&mut self, name: &'a str) -> CompileResult<()> {
        if self.compiler.locals.len() == u8::MAX as usize + 1 {
            return Err("Too many local variables in function.".into());
        }
        self.compiler.locals.push(Local {
            depth: -1,
            name,
            is_captured: false,
        });
        Ok(())
    }

    fn declare_variable(&mut self) -> CompileResult<()> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }

        let name = self.previous.source;

        for local in self.compiler.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }

            if local.name == name {
                return Err("Already a variable with this name in this scope.".into());
            }
        }

        self.add_local(name)
    }

    fn parse_variable(&mut self, message: &str) -> CompileResult<u8> {
        self.consume(TokenType::Identifier, message)?;

        self.declare_variable()?;
        if self.compiler.scope_depth > 0 {
            return Ok(0);
        }

        self.identifier_constant(self.previous.source)
    }

    fn var_declaration(&mut self) -> CompileResult<()> {
        let global = self.parse_variable("Expect variable name.")?;
        if self.match_token(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        self.define_variable(global);
        Ok(())
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal, global);
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
    }

    fn and(&mut self, _can_assign: bool) -> CompileResult<()> {
        let end_jmp = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_byte(OpCode::Pop);
        self.parse_precedence(Precedence::And)?;

        self.patch_jump(end_jmp)
    }

    fn or(&mut self, _can_assign: bool) -> CompileResult<()> {
        let end_jmp = self.emit_jump(OpCode::JumpIfTrue);

        self.emit_byte(OpCode::Pop);
        self.parse_precedence(Precedence::Or)?;

        self.patch_jump(end_jmp)
    }

    fn statement(&mut self) -> CompileResult<()> {
        if self.match_token(TokenType::Print)? {
            self.print_statement()?;
        } else if self.match_token(TokenType::LeftBrace)? {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else if self.match_token(TokenType::If)? {
            self.if_statement()?;
        } else if self.match_token(TokenType::While)? {
            self.while_statement()?;
        } else if self.match_token(TokenType::For)? {
            self.for_statement()?;
        } else if self.match_token(TokenType::Fun)? {
            self.fun_declaration()?;
        } else if self.match_token(TokenType::Return)? {
            self.return_statement()?;
        } else {
            self.expression_statement()?;
        }

        Ok(())
    }

    fn return_statement(&mut self) -> CompileResult<()> {
        if self.compiler.fun_type == FunctionType::Script {
            return Err(self.error("Can't return from top-level code."));
        }
        if self.match_token(TokenType::Semicolon)? {
            self.emit_return();
        } else {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
            self.emit_byte(OpCode::Return);
        }
        Ok(())
    }

    fn if_statement(&mut self) -> CompileResult<()> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let then_jmp = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.statement()?;

        if self.match_token(TokenType::Else)? {
            let else_jmp = self.emit_jump(OpCode::Jump);
            self.patch_jump(then_jmp)?;
            self.emit_byte(OpCode::Pop);

            self.statement()?;
            self.patch_jump(else_jmp)?;
        } else {
            self.patch_jump(then_jmp)?;
            self.emit_byte(OpCode::Pop);
        }
        Ok(())
    }

    fn fun_declaration(&mut self) -> CompileResult<()> {
        let global = self.parse_variable("Expect function name.")?;
        self.mark_initialized();
        self.function()?;
        self.define_variable(global);
        Ok(())
    }

    fn function(&mut self) -> CompileResult<()> {
        replace_with_or_abort(&mut self.compiler, |compiler| {
            Box::new(Compiler::new(
                FunctionType::Function,
                Some(self.previous.source.to_owned().into_boxed_str()),
                Some(compiler),
            ))
        });

        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;

        if !self.check(TokenType::RightParen) {
            loop {
                if self.compiler.function.arity == u8::MAX {
                    return Err(self.error_at_current("Can't have more than 255 parameters."));
                }
                self.compiler.function.arity += 1;
                let constant = self.parse_variable("Expect parameter name.")?;
                self.define_variable(constant);
                if !self.match_token(TokenType::Comma)? {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;
        self.block()?;
        self.end_compiler();

        let enclosing = self.compiler.enclosing.take();
        let compiler = std::mem::replace(&mut self.compiler, enclosing.unwrap());
        let upvalue_count = compiler.function.upvalue_count;

        let value = Value::Obj(unsafe { GcCell::new(*compiler.function, &mut self.gc).cast() });
        let byte2 = self.make_constant(value)?;

        self.emit_bytes(OpCode::Closure, byte2);

        for i in 0..upvalue_count {
            let upvalue = unsafe { compiler.upvalues[i].assume_init_ref() };
            self.emit_bytes(if upvalue.is_local { 1 } else { 0 }, upvalue.index);
        }

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> CompileResult<()> {
        self.emit_byte(OpCode::JumpUp);
        let Ok(offset) : Result<u16, _> = (self.current_chunk().code.len() - loop_start + 2).try_into() else {
           return Err(self.error("Loop body too large."));
        };

        self.emit_u16(offset);
        Ok(())
    }

    fn while_statement(&mut self) -> CompileResult<()> {
        let loop_start = self.current_chunk().code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let exit_jmp = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.statement()?;

        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jmp)?;
        self.emit_byte(OpCode::Pop);
        Ok(())
    }

    fn for_statement(&mut self) -> CompileResult<()> {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        if self.match_token(TokenType::Semicolon)? {
            // empty initializer
        } else if self.match_token(TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.expression()?;
        }

        let mut loop_start = self.current_chunk().code.len();

        let exit_jump;

        if !self.match_token(TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_byte(OpCode::Pop);
        } else {
            exit_jump = None;
        }

        if !self.match_token(TokenType::RightParen)? {
            let body_jmp = self.emit_jump(OpCode::Jump);
            let increment_start = self.current_chunk().code.len();
            self.expression()?;
            self.emit_byte(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

            self.emit_loop(loop_start)?;
            loop_start = increment_start;
            self.patch_jump(body_jmp)?;
        }

        self.statement()?;

        self.emit_loop(loop_start)?;

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
            self.emit_byte(OpCode::Pop);
        }

        self.end_scope();

        Ok(())
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.emit_u16(u16::MAX);
        self.current_chunk().code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) -> CompileResult<()> {
        let Ok(jump): Result<i16, _> = (self.current_chunk().code.len() - offset -2).try_into() else {
            return Err(self.error("Too much code to jump over."));
        };

        self.current_chunk().code[offset..=offset + 1].copy_from_slice(&jump.to_ne_bytes());

        Ok(())
    }

    fn print_statement(&mut self) -> CompileResult<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        self.emit_byte(OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> CompileResult<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        self.emit_byte(OpCode::Pop);
        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> CompileResult<()> {
        self.advance()?;
        let Some(rule) = ParseRule::get_rule(self.previous.token_type).prefix else {
           return Err(self.error("Expect expression."));
        };

        let can_assign = precedence <= Precedence::Assignment;

        rule(self, can_assign)?;

        while precedence <= ParseRule::get_rule(self.current.token_type).precedence {
            self.advance()?;
            let infix_rule = ParseRule::get_rule(self.previous.token_type).infix.unwrap();
            infix_rule(self, can_assign)?;
        }

        if can_assign && self.match_token(TokenType::Equal)? {
            Err(self.error("Invalid assignment target."))
        } else {
            Ok(())
        }
    }

    fn emit_constant(&mut self, value: Value) -> CompileResult<()> {
        let constant = self.make_constant(value)?;
        self.emit_bytes(OpCode::Constant, constant);
        Ok(())
    }

    fn make_constant(&mut self, value: Value) -> CompileResult<u8> {
        self.current_chunk()
            .push_constant(value)
            .map_err(|_| "Too many constants in one chunk.".into())
    }

    fn match_token(&mut self, tt: TokenType) -> CompileResult<bool> {
        Ok(if self.check(tt) {
            self.advance()?;
            true
        } else {
            false
        })
    }

    fn check(&mut self, tt: TokenType) -> bool {
        self.current.token_type == tt
    }

    fn synchronize(&mut self) {
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
            _ = self.advance().ok();
        }
    }

    pub fn compile(mut self) -> Result<Box<ObjFunction>, ()> {
        let result: CompileResult<()> = (|| {
            self.advance()?;
            while !self.match_token(TokenType::Eof)? {
                self.declaration();
            }

            self.end_compiler();
            Ok(())
        })();

        match result {
            Ok(_) => {
                if self.had_error {
                    Err(())
                } else {
                    Ok(self.compiler.function)
                }
            }
            Err(e) => {
                eprintln!("{}", e);

                Err(())
            }
        }
    }
}
