#!/usr/bin/env sh
# Syncronize notes across machines
#
# This is an alternative to Syncthing after it was banned on corporate devices.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source "${SCRIPT_DIR}/config.sh"

unison \
  -auto \
  -batch \
  -ignore "Regex .unison.*" \
  /home/cashweaver/proj/notes \
  "socket://localhost:${UNISON_SYNC_PORT}//usr/local/google/home/cashweaver/proj/notes"
