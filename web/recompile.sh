#!/bin/bash
set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "${SCRIPT_DIR}/.."

# Don't run this manually, run ./web/run_local.sh from the root instead to run the project locally.
# This script exists just to make sure the CI and the local building does the same.

mkdir -p ./web/built_html
wasm-pack build interpreter --target web --out-dir ../web/built_html/pkg/

cp ./web/index.html ./web/built_html/
