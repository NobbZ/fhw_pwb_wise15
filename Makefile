ifeq ($(LOGNAME), nmelzer)
KIND=debug
else
KIND=release
endif

all: target/$(KIND)/pwb_runner
	@echo $(KIND)

run:
	target/$(KIND)/pwb_runner

name:
	@echo "Wedel-Konstrukto.rs"

target/debug/pwb_runner:
	@cargo build

target/release/pwb_runner:
	@cargo build --release
