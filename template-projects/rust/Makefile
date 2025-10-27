.PHONY: all clean
all:
	cargo build --release --offline
	cp target/release/hello hello

clean:
	cargo clean
	$(RM) hello
