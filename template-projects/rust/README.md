# Rust Template

This is the Rust template for the ID2202 course. Like the other templates,
building happens with `make`, running with `./hello`. The project is built using
[`cargo`](https://crates.io/), which you thus must have installed on your
machine.

To use this template for an assignment, you may need to rename the
executable. To do so, edit `Makefile` (also remember to ignore the new
file in `.gitignore`):

```make
# Before
all:
	cargo build --release --offline
	cp target/release/hello hello

# After
all:
	cargo build --release --offline
	cp target/release/hello new-fancy-name
```

Put all your source files in `src`. You add external dependencies in `Cargo.toml`
under `dependencies`.
