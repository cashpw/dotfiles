#!/usr/bin/env sh
#
# To be run on remote machine to syncronize between local and cloud.
#
# This is an alternative to Syncthing after it was banned on corporate devices.

if [[ ! -x $(command -v "unison") ]]; then
  echo "[Sync] Error: unison not installed. Please install unison (e.g. sudo apt install unison)."
  exit 1
fi

gcertstatus --check_ssh=false > /dev/null || (echo "Renewing gcert:" && gcert)

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source "${SCRIPT_DIR}/config.sh"

trap "exit" INT TERM ERR
trap "kill 0" EXIT

unison -socket "${UNISON_SYNC_PORT}" &

while inotifywait -r -e modify,create,delete,move ~/proj/notes; do
  echo "anything, this is just to 'ping'" | socat - TCP:localhost:55320
done &

wait
