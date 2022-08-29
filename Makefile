all : test_print

test :
	cargo test
	./test.sh

test_print:
	cargo test -- --nocapture

run :
	cargo run

clean:
	rm -f tmp*

.PHONY: test test_print run clean
