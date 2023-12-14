#!/bin/bash

# Don't run this manually, run local_build_and_run.sh instead to run the project locally.
# This script exists just to make sure the CI and the local building does the same.

wasm-pack build --target web
mkdir -p built_html
# the folder export_html contains the html wrapper so that the wasm can be used
cp -r export_html/* built_html/
cp -r pkg/ built_html/
