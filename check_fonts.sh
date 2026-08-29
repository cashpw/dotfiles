#!/usr/bin/env bash
# Compatibility wrapper pointing to python dependency resolver
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
python3 "${script_dir}/scripts/setup/check_deps.py" "$@"
