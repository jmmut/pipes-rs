alias b := benchmark
benchmark:
    cargo test -r  benchmark -- --include-ignored --nocapture

alias w := web
web:
    ./web/run_local.sh