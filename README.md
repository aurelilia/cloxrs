# loxrs

An interpreter for [Lox](http://craftinginterpreters.com). Written in Rust.
It follows the 3rd part of the book, and is similar in structure to clox.
It also replaces some parts of clox by leveraging Rust's features.

### Notable differences to clox

- Stack can be bigger than 256
- Strings can be concatenated with anything

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