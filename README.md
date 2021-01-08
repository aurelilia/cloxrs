# cloxrs

An interpreter for [Lox](http://craftinginterpreters.com). Written in Rust.
It follows the 3rd part of the book, and is similar in structure to clox.
It also replaces some parts of clox by leveraging Rust's features.

### Progress

cloxrs is currently up to chapter 25.

### Notable differences to clox

- Stack can be bigger than 256
- Strings can be concatenated with anything

### Additional native functions

- `readfile`: Takes 1 path/string parameter, returns file content as string or nil
if file could not be read
- `writefile`: Takes 1 path/string parameter and one file content parameter, 
returns success (bool)

### Build/Run

``` bash
# Run as REPL
cargo run 

# Execute $file
cargo run $file
```

### Code style

Follow the style used by rustfmt.
Max line length is 120.