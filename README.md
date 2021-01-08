# cloxrs

An interpreter for [Lox](http://craftinginterpreters.com). Written in Rust.
It follows the 3rd part of the book, and is similar in structure to clox.
It also replaces some parts of clox by leveraging Rust's features.

### Progress

cloxrs is currently up to chapter 27.

### Notable user-facing differences to clox

- Stack can be bigger than 256
- Strings can be concatenated with anything
- Missing class fields simply produce `nil` instead of a runtime error

### Additional native functions

- `readfile`: Takes 1 path/string parameter, returns file content as string or nil
if file could not be read
- `writefile`: Takes 1 path/string parameter and one file content parameter, 
returns success (bool)

### Implementation differences

- Opcodes are simply an enum, which often simply contains the opcode arguments.

- Regular Rust HashMaps are used instead of a custom implementation.

- Due to not wanting to use unsafe Rust, upvalues are implemented entirely
different and use `Rc<Cell<...>>`. This mostly removes the need for GC, which is why cloxrs
GC only has to infreqently collect upvalues.

### Build/Run

``` bash
# Run as REPL
cargo run 

# Execute $file
cargo run $file
```

### Code style

Follow the style used by rustfmt.
