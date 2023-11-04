alias b := benchmark
benchmark:
	cargo test -r  benchmark -- --include-ignored --nocapture
