#!/usr/bin/env sh
#
# To be run on local machine to syncronize between local and cloud.
#
# This is an alternative to Syncthing after it was banned on corporate devices.

trap "exit" INT TERM ERR
trap "kill 0" EXIT

if [[ ! -x $(command -v "unison") ]]; then
  echo "[Sync] Error: unison not installed. Please install unison (e.g. sudo apt install unison)."
  exit 1
fi

gcertstatus --check_ssh=false > /dev/null || (echo "Renewing gcert:" && gcert)

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source "${SCRIPT_DIR}/config.sh"

if [[ $(nc -z localhost "${UNISON_NOTIFY_PORT}") ]]; then
  echo "[Sync] Error: Port ${UNISON_NOTIFY_PORT} is in use. Stopping."
  exit 1
fi
if [[ $(nc -z localhost "${UNISON_SYNC_PORT}") ]]; then
  echo "[Sync] Error: Port ${UNISON_SYNC_PORT} is in use. Stopping."
  exit 1
fi

echo "[Sync] SSH-ing ..."
ssh -R "${UNISON_NOTIFY_PORT}:localhost:${UNISON_NOTIFY_PORT}" -L "${UNISON_SYNC_PORT}:localhost:${UNISON_NOTIFY_PORT}" cashweaver.c.googlers.com &

echo "[Sync] Setting up watches ..."
while inotifywait -r -e modify,create,delete,move ~/proj/notes; do
  bash "${SCRIPT_DIR}/sync.sh"
done &

echo "[Sync] Setting up TCP listener ..."
while true; do
  socat TCP-LISTEN:55320 -> /tmp/unison_with_cloudtop
  bash "${SCRIPT_DIR}/sync.sh"
done &

wait
