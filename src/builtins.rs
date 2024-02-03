use crate::value::Value;

pub extern "win64" fn print(value: Value) {
    print!("{}", value)
}
