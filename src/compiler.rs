use std::collections::HashMap;
use std::mem::MaybeUninit;

use num_enum::{IntoPrimitive, TryFromPrimitive};
use replace_with::replace_with_or_abort;

use crate::common::LOX_LOX_EXTENSIONS;
use crate::emitter::GlobalVarIndex;
use crate::gc::intern_const_string;
use crate::{
    emitter::Emitter,
    gc::GcCell,
    object::{ObjFunction, ObjString},
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
                infix: Some(Parser::<'a, 'b>::dot),
                precedence: Precedence::Call,
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
                prefix: Some(Parser::<'a, 'b>::super_),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::This => ParseRule {
                prefix: Some(Parser::<'a, 'b>::this),
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
    index: usize,
    is_local: bool,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum FunctionType {
    Function,
    Initializer,
    Method,
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
    fn resolve_local(&mut self, name: &'a str) -> Result<Option<usize>, &'static str> {
        if let Some((i, local)) = self
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == name)
        {
            if local.depth == -1 {
                Err("Can't read local variable in its own initializer.")
            } else {
                Ok(Some(i))
            }
        } else {
            Ok(None)
        }
    }

    fn resolve_upvalue(&mut self, name: &'a str) -> Result<Option<usize>, &'static str> {
        if let Some(enclosing) = &mut self.enclosing {
            if let Some(local) = enclosing.resolve_local(name)? {
                self.enclosing.as_mut().unwrap().locals[local].is_captured = true;
                return self.add_upvalue(local, true).map(Some);
            }
            if let Some(upvalue) = enclosing.resolve_upvalue(name)? {
                return self.add_upvalue(upvalue, false).map(Some);
            }
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> Result<usize, &'static str> {
        let upvalue_count = self.function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = unsafe { self.upvalues[i].assume_init_ref() };
            if upvalue.index == index && upvalue.is_local == is_local {
                return Ok(i);
            }
        }

        if upvalue_count == self.upvalues.len() {
            return Err("Too many closure variables in function.");
        }

        self.upvalues[upvalue_count].write(Upvalue { index, is_local });

        self.function.upvalue_count += 1;
        Ok(upvalue_count)
    }
}

impl<'a> Compiler<'a> {
    fn new(
        fun_type: FunctionType,
        fun_name: Option<GcCell<ObjString>>,
        enclosing: Option<Box<Compiler<'a>>>,
    ) -> Self {
        let mut compiler = Self {
            function: Box::new(ObjFunction::new(0, fun_name, None)),
            fun_type,
            locals: Vec::with_capacity(u8::MAX as usize + 1),
            scope_depth: 0,
            enclosing,
            upvalues: [MaybeUninit::uninit(); 256],
        };

        compiler.locals.push(Local {
            name: if fun_type == FunctionType::Function {
                ""
            } else {
                "this"
            },
            depth: 0,
            is_captured: false,
        });
        compiler
    }
}

struct ClassCompiler {
    enclosing: Option<Box<ClassCompiler>>,
    has_superclass: bool,
}

pub struct Parser<'a, 'b> {
    current: Token<'a>,
    previous: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    compiler: Box<Compiler<'a>>,
    class_compiler: Option<Box<ClassCompiler>>,
    emitter: &'b mut Emitter,
    globals: &'b mut HashMap<GcCell<ObjString>, GlobalVarIndex>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(
        source: &'a str,
        emitter: &'b mut Emitter,
        globals: &'b mut HashMap<GcCell<ObjString>, GlobalVarIndex>,
    ) -> Self {
        /*if globals.is_empty() {
            let name = intern_const_string("clock".to_string());
            let index = emitter.add_global(name);
            emitter.builtin_fn_0(clock);
            emitter.define_global(index);
            globals.insert(name, index);

            if LOX_LOX_EXTENSIONS {
                // getc
                let name = intern_const_string("getc".to_string());
                let index = emitter.add_global(name);
                emitter.builtin_fn_0(getc);
                emitter.define_global(index);
                globals.insert(name, index);

                // chr(ch)
                let name = intern_const_string("chr".to_string());
                let index = emitter.add_global(name);
                emitter.builtin_fn_1(chr);
                emitter.define_global(index);
                globals.insert(name, index);

                // exit(status)
                let name = intern_const_string("exit".to_string());
                let index = emitter.add_global(name);
                emitter.builtin_fn_1(exit);
                emitter.define_global(index);
                globals.insert(name, index);

                // print_error(message)
                let name = intern_const_string("print_error".to_string());
                let index = emitter.add_global(name);
                emitter.builtin_fn_1(print_error);
                emitter.define_global(index);
                globals.insert(name, index);
            }
        } else {
            // We need to re-set the builtins because the instruction buffer might have been
            // reallocated. TODO: Maybe use a different buffer for builtins?
            emitter.builtin_fn_0(clock);
            emitter.define_global(
                *globals
                    .get(&intern_const_string("clock".to_string()))
                    .unwrap(),
            );
            if LOX_LOX_EXTENSIONS {
                emitter.builtin_fn_0(getc);
                emitter.define_global(
                    *globals
                        .get(&intern_const_string("getc".to_string()))
                        .unwrap(),
                );
                emitter.builtin_fn_1(chr);
                emitter.define_global(
                    *globals
                        .get(&intern_const_string("chr".to_string()))
                        .unwrap(),
                );
                emitter.builtin_fn_1(exit);
                emitter.define_global(
                    *globals
                        .get(&intern_const_string("exit".to_string()))
                        .unwrap(),
                );
                emitter.builtin_fn_1(print_error);
                emitter.define_global(
                    *globals
                        .get(&intern_const_string("print_error".to_string()))
                        .unwrap(),
                );
            }
        }*/
        //emitter.enter_function_scope(None, 0);
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
            panic_mode: false,
            compiler: Box::new(Compiler::new(FunctionType::Script, None, None)),
            class_compiler: None,
            emitter,
            globals,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;
        self.emitter.set_line(self.previous.line);
        loop {
            self.current = self.scanner.scan_token();
            if !matches!(self.current.token_type, TokenType::Error) {
                break;
            }
            self.error_at_current(self.current.source)
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
        self.had_error = true;
        eprint!("[line {}] Error", token.line,);
        match token.token_type {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => {}
            _ => eprint!(" at '{}'", token.source),
        }
        eprintln!(": {}", message);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
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
                todo!(); //self.emitter.close_upvalue();
            } else {
                self.emitter.pop();
            }
            self.compiler.locals.pop();
        }
    }

    fn emit_return(&mut self) {
        match self.compiler.fun_type {
            FunctionType::Initializer => todo!(), //self.emitter.get_local(0),
            FunctionType::Function | FunctionType::Method => todo!(), //self.emitter.nil(),
            FunctionType::Script => return,
        }

        if let Some(fn_info) = self.compiler.function.fn_info {
            todo!(); //self.emitter.ret(fn_info);
        } else {
            assert!(self.had_error);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    fn block(&mut self) {
        while !(self.check(TokenType::RightBrace) || self.check(TokenType::Eof)) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")
    }

    fn number(&mut self, _can_assign: bool) {
        let value: f64 = self.previous.source.parse().unwrap();
        self.emitter.push(value.to_bits());
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.token_type;

        self.parse_precedence(Precedence::Unary);
        match operator_type {
            TokenType::Bang => self.emitter.not(),
            TokenType::Minus => self.emitter.negate(),
            _ => (),
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        let operator_type = self.previous.token_type;
        let rule = ParseRule::get_rule(operator_type);
        self.parse_precedence((Into::<u8>::into(rule.precedence) + 1).try_into().unwrap());

        match operator_type {
            TokenType::BangEqual => todo!(),    //self.emitter.ne(),
            TokenType::EqualEqual => todo!(),   //self.emitter.eq(),
            TokenType::Greater => todo!(),      //self.emitter.gt(),
            TokenType::GreaterEqual => todo!(), //self.emitter.ge(),
            TokenType::Less => todo!(),         //self.emitter.lt(),
            TokenType::LessEqual => todo!(),    //self.emitter.le(),
            TokenType::Plus => self.emitter.add(),
            TokenType::Minus => self.emitter.sub(),
            TokenType::Star => self.emitter.mul(),
            TokenType::Slash => self.emitter.div(),
            _ => unreachable!(),
        }
    }

    fn argument_list(&mut self) -> u8 {
        let mut count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if count == u8::MAX {
                    self.error("Can't have more than 255 arguments.");
                }
                count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        count
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        todo!(); //self.emitter.call(arg_count);
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.identifier_constant(self.previous.source);

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            todo!(); //self.emitter.set_property(name)
        } else if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            todo!(); //self.emitter.invoke(name, arg_count)
        } else {
            todo!(); //self.emitter.get_property(name)
        }
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous.token_type {
            TokenType::False => self.emitter.false_(),
            TokenType::True => self.emitter.true_(),
            TokenType::Nil => self.emitter.nil(),
            _ => unreachable!(),
        }
    }

    fn this(&mut self, _can_assign: bool) {
        if self.class_compiler.is_none() {
            self.error("Can't use 'this' outside of a class.");
            return;
        }
        self.variable(false);
    }

    fn string(&mut self, _can_assign: bool) {
        let obj = intern_const_string(
            self.previous.source[1..self.previous.source.len() - 1].to_string(),
        );
        self.emitter.push(Value::from_obj(unsafe { obj.cast() }).to_bits());
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous.source, can_assign)
    }

    fn super_(&mut self, _can_assign: bool) {
        if let Some(class_compiler) = &self.class_compiler {
            if !class_compiler.has_superclass {
                self.error("Can't use 'super' in a class with no superclass.");
            }
        } else {
            self.error("Can't use 'super' outside of a class.");
        }
        self.consume(TokenType::Dot, "Expect '.' after 'super'.");
        self.consume(TokenType::Identifier, "Expect superclass method name.");
        let name = self.identifier_constant(self.previous.source);

        self.named_variable("this", false);
        if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable("super", false);
            todo!(); //self.emitter.invoke_super(name, arg_count);
        } else {
            self.named_variable("super", false);
            todo!(); //self.emitter.get_super(name)
        }
    }

    fn get_global_var_index(&mut self, string: GcCell<ObjString>) -> GlobalVarIndex {
        if let Some(value) = self.globals.get(&string) {
            *value
        } else {
            let index = self.emitter.add_global(string);
            self.globals.insert(string, index);
            index
        }
    }

    fn named_variable(&mut self, name: &'a str, can_assign: bool) {
        if let Some(index) = self
            .compiler
            .resolve_local(name)
            .map_err(|e| self.error(e))
            .ok()
            .flatten()
        {
            if can_assign && self.match_token(TokenType::Equal) {
                self.expression();
                self.emitter.set_local(index);
            } else {
                self.emitter.get_local(index);
            }
        } else if let Some(index) = self
            .compiler
            .resolve_upvalue(name)
            .map_err(|e| self.error(e))
            .ok()
            .flatten()
        {
            if can_assign && self.match_token(TokenType::Equal) {
                self.expression();
                todo!(); //self.emitter.set_upvalue(index);
            } else {
                todo!(); //self.emitter.get_upvalue(index);
            }
        } else {
            let constant = self.identifier_constant(name);
            let index = self.get_global_var_index(constant);
            if can_assign && self.match_token(TokenType::Equal) {
                self.expression();
                self.emitter.set_global(index);
            } else {
                self.emitter.get_global(index);
            }
        }
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Class) {
            self.class_declaration()
        } else if self.match_token(TokenType::Var) {
            self.var_declaration()
        } else if self.match_token(TokenType::Fun) {
            self.fun_declaration()
        } else {
            self.statement()
        }

        if self.panic_mode {
            self.synchronize()
        }
    }

    fn identifier_constant(&mut self, name: &str) -> GcCell<ObjString> {
        intern_const_string(name.to_string())
    }

    fn add_local(&mut self, name: &'a str) {
        if self.compiler.locals.len() == u8::MAX as usize + 4 {
            self.error("Too many local variables in function.");
            return;
        }
        self.compiler.locals.push(Local {
            depth: -1,
            name,
            is_captured: false,
        });
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous.source;

        for local in self.compiler.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }

            if local.name == name {
                self.error("Already a variable with this name in this scope.");
                return;
            }
        }

        self.add_local(name)
    }

    fn parse_variable(&mut self, message: &str) -> Option<GcCell<ObjString>> {
        self.consume(TokenType::Identifier, message);

        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return None;
        }

        Some(self.identifier_constant(self.previous.source))
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");
        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emitter.nil();
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn define_variable(&mut self, global: Option<GcCell<ObjString>>) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
        } else {
            let var = self.get_global_var_index(global.unwrap());
            self.emitter.define_global(var);
        }
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jmp = todo!(); //self.emitter.get_new_label();
        todo!(); //self.emitter.jump_if_false(end_jmp);

        todo!(); //self.emitter.pop();
        self.parse_precedence(Precedence::And);

        todo!(); //self.emitter.set_jump_target(end_jmp);
    }

    fn or(&mut self, _can_assign: bool) {
        let end_jmp = todo!(); //self.emitter.get_new_label();
        todo!(); //self.emitter.jump_if_true(end_jmp);

        todo!(); //self.emitter.pop();
        self.parse_precedence(Precedence::Or);

        todo!(); //self.emitter.set_jump_target(end_jmp);
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
        } else {
            self.expression_statement();
        }
    }

    fn return_statement(&mut self) {
        if self.compiler.fun_type == FunctionType::Script {
            self.error("Can't return from top-level code.")
        } else if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else if self.compiler.fun_type == FunctionType::Initializer {
            self.error("Can't return a value from an initializer.")
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            todo!(); //self.emitter.ret(self.compiler.function.fn_info.unwrap());
        }
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jmp = todo!(); //self.emitter.get_new_label();
        todo!(); //self.emitter.jump_if_false(then_jmp);
        todo!(); //self.emitter.pop();
        self.statement();

        //let else_jmp = todo!();//self.emitter.get_new_label();
        todo!(); //self.emitter.jump(else_jmp);

        todo!(); //self.emitter.set_jump_target(then_jmp);
        todo!(); //self.emitter.pop();

        if self.match_token(TokenType::Else) {
            self.statement();
        }

        todo!(); //self.emitter.set_jump_target(else_jmp);
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn function(&mut self, function_type: FunctionType) {
        todo!(); /*
                 let jmp = todo!();//self.emitter.get_new_label();
                 todo!();//self.emitter.jump(jmp);

                 let fn_name = intern_const_string(self.previous.source.to_owned());

                 replace_with_or_abort(&mut self.compiler, |compiler| {
                     Box::new(Compiler::new(function_type, Some(fn_name), Some(compiler)))
                 });

                 self.begin_scope();

                 self.consume(TokenType::LeftParen, "Expect '(' after function name.");

                 if !self.check(TokenType::RightParen) {
                     loop {
                         if self.compiler.function.arity == u8::MAX {
                             return self.error_at_current("Can't have more than 255 parameters.");
                         }
                         self.compiler.function.arity += 1;
                         let constant = self.parse_variable("Expect parameter name.");
                         self.define_variable(constant);
                         if !self.match_token(TokenType::Comma) {
                             break;
                         }
                     }
                 }
                 self.emitter
                     .enter_function_scope(self.compiler.function.name, self.compiler.function.arity);

                 let fn_info = todo!();//self.emitter.start_fn(self.compiler.function.arity);
                 self.compiler.function.fn_info = Some(fn_info);

                 // JIT needs to preserve rsi, rip and rbp here. Don't use those spots.
                 // See emitter.rs for the documentation of the internal calling convention.
                 self.compiler.locals.push(Local {
                     name: "",
                     depth: -1,
                     is_captured: false,
                 });
                 self.compiler.locals.push(Local {
                     name: "",
                     depth: -1,
                     is_captured: false,
                 });
                 self.compiler.locals.push(Local {
                     name: "",
                     depth: -1,
                     is_captured: false,
                 });

                 self.consume(TokenType::RightParen, "Expect ')' after parameters.");
                 self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
                 self.block();
                 self.end_compiler();

                 let enclosing = self.compiler.enclosing.take();
                 let compiler = std::mem::replace(&mut self.compiler, enclosing.unwrap());
                 let upvalue_count = compiler.function.upvalue_count;

                 todo!();//self.emitter.fn_epilogue(fn_info);

                 todo!();//self.emitter.set_jump_target(jmp);

                 self.emitter
                     .enter_function_scope(self.compiler.function.name, self.compiler.function.arity);

                 todo!();//self.emitter.end_fn(
                     fn_info,
                     fn_name,
                     (0..upvalue_count).map(|i| {
                         let upvalue = unsafe { compiler.upvalues[i].assume_init_ref() };
                         (upvalue.is_local, upvalue.index)
                     }),
                 );
                 */
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");

        let class_name = self.previous.source;

        let name_constant = self.identifier_constant(self.previous.source);
        self.declare_variable();

        todo!(); //self.emitter.push_class(name_constant);
        self.define_variable(Some(name_constant));

        self.class_compiler = Some(Box::new(ClassCompiler {
            enclosing: self.class_compiler.take(),
            has_superclass: false,
        }));

        if self.match_token(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            self.variable(false);

            if class_name == self.previous.source {
                self.error("A class can't inherit from itself.");
            }

            self.named_variable(class_name, false);
            todo!(); //self.emitter.inherit();
            self.class_compiler.as_mut().unwrap().has_superclass = true;

            self.begin_scope();
            self.add_local("super");
            self.define_variable(None);
        }

        self.named_variable(class_name, false);
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        todo!(); //self.emitter.pop();

        if self.class_compiler.as_ref().unwrap().has_superclass {
            self.end_scope();
        }

        self.class_compiler = self.class_compiler.take().unwrap().enclosing;
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let name = self.identifier_constant(self.previous.source);

        self.function(if self.previous.source == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        });

        todo!(); //self.emitter.add_method(name)
    }

    fn while_statement(&mut self) {
        let loop_start = todo!(); //self.emitter.get_new_label();
        todo!(); //self.emitter.set_jump_target(loop_start);

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jmp = todo!(); //self.emitter.get_new_label();
        todo!(); //self.emitter.jump_if_false(exit_jmp);
        todo!(); //self.emitter.pop();
        self.statement();

        todo!(); //self.emitter.jump(loop_start);

        todo!(); //self.emitter.set_jump_target(exit_jmp);
        todo!(); //self.emitter.pop();
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        if self.match_token(TokenType::Semicolon) {
            // empty initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        //let mut loop_start = todo!();//self.emitter.get_new_label();
        todo!(); //self.emitter.set_jump_target(loop_start);

        //let exit_jump = todo!();//self.emitter.get_new_label();

        if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            todo!(); //self.emitter.jump_if_false(exit_jump);
            todo!(); //self.emitter.pop();
        }

        if !self.match_token(TokenType::RightParen) {
            let body_jmp = todo!(); //self.emitter.get_new_label();
            todo!(); //self.emitter.jump(body_jmp);

            //let increment_start = todo!();//self.emitter.get_new_label();
            todo!(); //self.emitter.set_jump_target(increment_start);
            self.expression();
            todo!(); //self.emitter.pop();
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            todo!(); //self.emitter.jump(loop_start);
                     //loop_start = increment_start;
            todo!(); //self.emitter.set_jump_target(body_jmp);
        }

        self.statement();

        todo!(); //self.emitter.jump(loop_start);

        todo!(); //self.emitter.set_jump_target(exit_jump);
        todo!(); //self.emitter.pop();

        self.end_scope();
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emitter.print();
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emitter.pop();
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
            self.error("Invalid assignment target.")
        }
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

    pub fn compile(mut self) -> Result<extern "win64" fn(*mut Value, *const Emitter) -> u8, ()> {
        let entry_point = self.emitter.create_entrypoint();
        self.advance();
        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }

        self.end_compiler();
        match self.had_error {
            true => Err(()),
            false => Ok(self.emitter.finish(entry_point)),
        }
    }
}
