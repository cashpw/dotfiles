#!/usr/bin/env sh
#
# To be run on remote machine to syncronize between local and cloud.
#
# This is an alternative to Syncthing after it was banned on corporate devices.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source "${SCRIPT_DIR}/config.sh"

unison -socket "${UNISON_SYNC_PORT}" &
while inotifywait -r -e modify,create,delete,move ~/proj/notes; do
  echo "anything, this is just to 'ping'" | socat - TCP:localhost:55320
done &
