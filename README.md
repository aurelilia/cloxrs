# cloxrs

An interpreter for [Lox](http://craftinginterpreters.com). Written in Rust.
It follows the 3rd part of the book, and is similar in structure to clox.
It also replaces some parts of clox by leveraging Rust's features.

It is a fully-featured Lox VM, and all features are implemented, however the code is still
mostly unoptimized and probably slower than clox.

### Notable user-facing differences to clox

- Stack can be bigger than 256
- Strings can be concatenated with anything
- Missing class fields simply produce `nil` instead of a runtime error

### Additional native functions

- `readfile`: Takes 1 path parameter, returns file content as string or nil
if file could not be read
- `writefile`: Takes 1 path parameter and one file content parameter, 
returns success (bool)

### Implementation differences

- Opcodes are simply an enum, which often simply contains the opcode arguments.
- There is no constant table; args are embedded into opcodes (TODO: Maybe do make a
table for strings, having them in the opcode is probably really inefficient)
- rustc HashMaps are used instead of a custom implementation.
- Due to not wanting to use unsafe Rust, upvalues are implemented entirely
different and use `Rc<Cell<...>>`. This mostly removes the need for GC, which is why cloxrs
GC only has to infreqently collect upvalues.
- Methods are implemented by adding an `EndClass` opcode instead of `Method`, and simply pushing each method onto the
stack above the class. `EndClass` then simply pops methods off the stack until it reaches the class,
where it then inserts them.

### Build/Run

``` bash
# Run as REPL
cargo run 

# Execute $file
cargo run $file
```

### Code style

Follow the style used by rustfmt.
