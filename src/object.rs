use std::fmt::Display;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Obj {
    pub contents: ObjContents
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum ObjContents {
    String(Box<str>),
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.contents {
            ObjContents::String(s) => write!(f, "{}",  &*s),
        }
    }
}

impl From<String> for Obj {
    fn from(s: String) -> Self {
        Self {
            contents: ObjContents::String(s.into_boxed_str())
        }
    }
}