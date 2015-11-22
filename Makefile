KIND=debug

.PHONY: all run name target/debug/pwb_runner target/build/pwb_runner clean

all: target/$(KIND)/pwb_runner

clean:
	@cargo clean -p pwb_ws_15

test: clean
	@cargo test

rebuild: clean all

run:
	@target/$(KIND)/pwb_runner

name:
	@echo "Wedel-Konstrukto.rs"

target/debug/pwb_runner: clean
	@cargo build

target/release/pwb_runner: clean
	@cargo build --release
