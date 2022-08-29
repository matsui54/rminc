all : test_no_cap

test :
	cargo test

test_no_cap:
	cargo test -- --nocapture

run :
	cargo run

.PHONY: test run
