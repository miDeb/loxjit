# loxjit

This is a simple lox JIT compiler targeting x64. It is essentially a port of clox, but generating x64 assembly instead of bytecode for the interpreter.
Under the hood [dynasm-rs](https://crates.io/crates/dynasm) is used to assemble the instructions.

loxjit uses inline caches (ICs) to speed up property lookups and method calls.

Another optimization that has a noticeable effect is the removal of hash lookups for accessing global variables. Instead, global variables are addressed by an index assigned by the compiler.

## Running LoxLox

loxjit supports running [LoxLox](https://github.com/benhoyt/loxlox), a Lox interpreter written in Lox. Support is enabled using the `LOX_LOX_EXTENSIONS` constant in `lib/common.rs` (defaults to true).
To run a lox script in LoxLox, run `cargo run --release lox.lox < script.lox`.

### Running LoxLox inside of LoxLox

To enable LoxLox to run itself the `getc` extension considers a null byte as EOF. Therefore, when running
```bash
(cat lox.lox; printf "\0"; cat script.lox) | cargo r --release lox.lox
```
first, loxjit will run the first LoxLox instance, which will read stdin until the null byte is encountered.
Then it will start to run the second LoxLox instance, which will continue to read stdin and execute `script.lox`.
