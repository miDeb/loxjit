# loxjit

This is a simple lox JIT compiler targeting x64. It is essentially a port of clox, but generating x64 assembly instead of bytecode for the interpreter.
Under the hood [dynasm-rs](https://crates.io/crates/dynasm) is used to assemble the instructions.

The goal is to implement inline caches (ICs) to make property access and method calls faster. Currently however only the IC for getting properties is implemented (which already causes a noticeable performance improvement).

A non-jit optimization that has a noticeable effect is the removal of hash lookups for accessing global variables. Instead, global variables are addressed by an index assigned by the compiler.