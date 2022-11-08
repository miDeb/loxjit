use dynasmrt::AssemblyOffset;

use crate::{gc::GcCell, object::ObjString};

pub struct SourceMapping {
    fn_infos: Vec<(AssemblyOffset, FnSourceInfo)>,
    line_infos: Vec<(AssemblyOffset, LineSourceInfo)>,
}

impl SourceMapping {
    pub const fn new() -> Self {
        Self {
            fn_infos: Vec::new(),
            line_infos: Vec::new(),
        }
    }

    pub fn set_line(&mut self, offset: AssemblyOffset, line: usize) {
        if let Some((last_offset, last_line)) = self.line_infos.last_mut() {
            if *last_offset == offset {
                last_line.line = line;
                return;
            }
            if last_line.line == line {
                return;
            }
        }
        self.line_infos.push((offset, LineSourceInfo { line }))
    }

    pub fn begin_function(&mut self, offset: AssemblyOffset, function: FnSourceInfo) {
        self.fn_infos.push((offset, function));
    }

    pub fn print_stacktrace(&self, base_offset: *const u8, mut ip: *const u8, mut bp: *const u8) {
        loop {
            let offset = AssemblyOffset(unsafe { ip.sub_ptr(base_offset) });

            let function = self.find_function_for_offset(offset);
            let line = self.find_line_for_offset(offset);
            eprint!("[line {}] ", line.line);
            if let Some(name) = function.name {
                eprintln!("in {name}()");
                ip = unsafe { *bp.sub(function.arg_count as usize * 0x8 + 0x8).cast() };
                bp = unsafe { *bp.sub(function.arg_count as usize * 0x8 + 0x10).cast() };
            } else {
                eprintln!("in script");
                break;
            }
        }
    }

    fn find_function_for_offset(&self, offset: AssemblyOffset) -> FnSourceInfo {
        let index = match self.fn_infos.binary_search_by(|a| a.0.cmp(&offset)) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        self.fn_infos[index].1.clone()
    }

    fn find_line_for_offset(&self, offset: AssemblyOffset) -> LineSourceInfo {
        let index = match self.line_infos.binary_search_by(|a| a.0.cmp(&offset)) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        self.line_infos[index].1
    }
}

#[derive(Clone)]
pub struct FnSourceInfo {
    name: Option<GcCell<ObjString>>,
    arg_count: u8,
}

impl FnSourceInfo {
    pub fn new(name: Option<GcCell<ObjString>>, arg_count: u8) -> Self {
        Self { name, arg_count }
    }
}

#[derive(Clone, Copy)]
struct LineSourceInfo {
    line: usize,
}
