use std::{borrow::Cow, fmt::Display};

pub type CompileResult<T> = Result<T, CompileError>;

pub struct CompileError {
    pub message: Cow<'static, str>,
    pub has_line_info: bool,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl CompileError {
    pub fn new(message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
            has_line_info: false,
        }
    }
}

impl<T> From<T> for CompileError
where
    T: Into<Cow<'static, str>>,
{
    fn from(t: T) -> Self {
        Self::new(t)
    }
}
