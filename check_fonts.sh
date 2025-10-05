#!/bin/bash
# Check for expected fonts. The user is expected to manually install missing fonts themselves.

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

for font_with_extension in ${script_dir}/expected-fonts/*; do
  font=$(basename -- "${font_with_extension}")
  font="${font%.*}"
  echo ${font}
  fc-list | grep -q "*${font}*"
  if [[ ! $(fc-list | grep -q "${font}") ]]; then
    echo "Missing font: ${font}"
    sed 's/^/  /' "${script_dir}/expected-fonts/${font}.md"
  fi
done
