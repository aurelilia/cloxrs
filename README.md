# cloxrs

An interpreter for [Lox](http://craftinginterpreters.com). Written in Rust.
It follows the 3rd part of the book, and is similar in structure to clox.
It also replaces some parts of clox by leveraging Rust's features.

It is a fully-featured Lox VM, and all features are implemented, however it's about
4-7x slower (depending on the benchmark) than clox due to different
design decisions as well as a few limitations of Rust's memory model, 

### Notable user-facing differences to clox

- Stack can be bigger than 256
- Strings can be concatenated with anything
- Missing class fields simply produce `nil` instead of a runtime error

### Additional native functions

- `readfile`: Takes 1 path parameter, returns file content as string or nil
if file could not be read
- `writefile`: Takes 1 path parameter and one file content parameter, 
returns success (bool)
- `input`: Waits for user to input one line, returns input. Newline at the end is stripped.

### Implementation differences

- Opcodes are simply an enum, which contains the opcode arguments.
- Instead of a per-chunk constant table, constant strings are inside a global interner,
with all other constants embedded into the instruction.
- The table/map implementation uses the interner key for faster hashing instead of
storing the hash of every string.
- Frames are stored implicitly in the VM callstack, with `run` recursing with every call.
This is faster, but also means that the VM does not display stack traces. 
- Due to not wanting to use unsafe Rust, upvalues are implemented entirely
different and use `Rc<Cell<...>>`. This mostly removes the need for GC, which is why cloxrs
GC only has to infreqently collect upvalues. This does make them slower though.
- Methods are implemented by adding an `EndClass` opcode instead of `Method`, and pushing each method onto the
stack above the class. `EndClass` then pops methods off the stack until it reaches the class,
where it then inserts them.

### Build/Run

`cloxrs` requires a nightly version of Rust. If you use `rustup`, `rustup default nightly`
will automatically install the newest nightly version.

``` bash
# Build release, output in ./target/release/loxrs
cargo build --release

# Run as REPL
./target/release/loxrs 

# Execute $file
./target/release/loxrs  $file
```

### Code style

Follow the style used by rustfmt.
