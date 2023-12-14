#!/bin/bash

# Before running this script, you'll need to:
# - add the WebAssembly target in the rust compiler:
#   rustup target add wasm32-unknown-unknown
# - install wasm-pack to use wasm-bindgen
#   cargo install wasm-pack
# - install the local web server:
#   cargo install basic-http-server
# see the readme.md for more info

./recompile_web.sh

basic-http-server built_html/
