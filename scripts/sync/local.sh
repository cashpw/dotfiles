#!/usr/bin/env sh
#
# To be run on local machine to syncronize between local and cloud.
#
# This is an alternative to Syncthing after it was banned on corporate devices.

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source "${SCRIPT_DIR}/config.sh"

ssh -R 55320:localhost:55320 -L 55321:localhost:55321 cashweaver.c.googlers.com &

while inotifywait -r -e modify,create,delete,move ~/proj/notes; do
  bash "${SCRIPT_DIR}/sync.sh"
done &

while true; do
  socat TCP-LISTEN:55320 -> /tmp/unison_with_cloudtop
  bash "${SCRIPT_DIR}/sync.sh"
done &
