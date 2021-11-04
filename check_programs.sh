#!/bin/bash
# Check for expected programs. The user is expected to manually install missing programs themselves.

missing_programs=()
found_programs=()

while read program; do
  if [[ ! -x $(command -v "${program}") ]]; then
    missing_programs+=("${program}")
  else
    found_programs+=("${program}")
  fi
done <expected_programs.txt

if [[ "${#missing_programs[@]}" != "0" ]]; then
  echo "Found ${#found_programs[@]}/$(wc -l < expected_programs.txt) programs."
  echo ""
  echo "Missing programs:"
  for missing_program in "${missing_programs[@]}"; do
    echo "${missing_program}"
  done
  echo ""
  echo "Try this: sudo apt install ${missing_programs[*]}"
else
  echo "Found all programs."
fi
