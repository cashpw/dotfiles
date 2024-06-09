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
echo "[Sync] Setting up watches ..."

if [[ $(nc -z localhost "${UNISON_NOTIFY_PORT}") ]]; then
  echo "[Sync] Error: Port ${UNISON_NOTIFY_PORT} is in use. Stopping."
  exit 1
fi
if [[ $(nc -z localhost "${UNISON_SYNC_PORT}") ]]; then
  echo "[Sync] Error: Port ${UNISON_SYNC_PORT} is in use. Stopping."
  exit 1
fi

while inotifywait -r -e modify,create,delete,move ~/proj/notes; do
  echo "[Sync] Synchronizing due to local change ..."
  bash "${SCRIPT_DIR}/sync.sh"
  echo "[Sync] Synchronizing DONE"
done &

echo "[Sync] Setting up TCP listener ..."
while true; do socat "TCP-LISTEN:${UNISON_NOTIFY_PORT}" -> /tmp/unison_with_cloudtop;
  echo "[Sync] Synchronizing due to remote change ..."
  bash "${SCRIPT_DIR}/sync.sh"
  echo "[Sync] Synchronizing DONE"
done

echo "[Sync] SSH-ing ..."
ssh \
  -R "${UNISON_NOTIFY_PORT}:localhost:${UNISON_NOTIFY_PORT}" \
  -L "${UNISON_SYNC_PORT}:localhost:${UNISON_SYNC_PORT}" \
  cashweaver.c.googlers.com &

#wait
