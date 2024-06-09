#!/usr/bin/env sh
# Syncronize notes across machines
#
# This is an alternative to Syncthing after it was banned on corporate devices.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source "${SCRIPT_DIR}/config.sh"

if [[ ! -x $(command -v "unison") ]]; then
  echo "[Sync notes] Error: unison not installed. Please install unison (e.g. sudo apt install unison)."
fi

unison \
  -auto \
  -batch \
  -ignore "Name {org-roam.db}" \
  -ignore "Regex .unison.*" \
  /home/cashweaver/proj/notes \
  "socket://localhost:${UNISON_SYNC_PORT}//usr/local/google/home/cashweaver/proj/notes"
