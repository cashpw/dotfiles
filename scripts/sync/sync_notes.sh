#!/usr/bin/env sh
# Syncronize notes across machines
#
# This is an alternative to Syncthing after it was banned on corporate devices.

source ~/.scripts/identify_device/identify_device.sh

if [[ ! -x $(command -v "unison") ]]; then
  echo "[Sync notes] Error: unison not installed. Please install unison (e.g. sudo apt install unison)."
fi

if [[ -x $(command -v "gcert") ]]; then
  gcertstatus --quiet || gcert
fi
unison -auto -batch /home/cashweaver/proj/notes ssh://cashweaver.c.googlers.com/proj/notes
