#!/bin/bash
# Check for expected programs. The user is expected to manually install missing programs themselves.

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# Mapping from expected-programs file stem to possible binary names
declare -A ALIASES=(
  ["i3status-rs"]="i3status-rs i3status-rust"
  ["rofi"]="rofi rofi-wayland"
  ["nvm"]="nvm node"
)

is_installed() {
  local program="$1"
  local binaries="${ALIASES[$program]:-$program}"
  for bin in $binaries; do
    if [[ -x $(command -v "$bin") ]]; then
      return 0
    fi
  done
  return 1
}

for program_with_extension in ${script_dir}/expected-programs/*; do
  program=$(basename -- "${program_with_extension}")
  program="${program%.*}"
  if ! is_installed "${program}"; then
    echo "Missing program: ${program}"
    sed 's/^/  /' "${script_dir}/expected-programs/${program}.md"
    echo ""
  fi
done
