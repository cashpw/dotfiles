#!/bin/bash
# Check for expected programs. The user is expected to manually install missing programs themselves.

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

for program_with_extension in ${script_dir}/expected-programs/*; do
  program=$(basename -- "${program_with_extension}")
  program="${program%.*}"
  if [[ ! -x $(command -v "${program}") ]]; then
    echo "Missing program: ${program}"
    sed 's/^/  /' "${script_dir}/expected-programs/${program}.md"
  fi
done
