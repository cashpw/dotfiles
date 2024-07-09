#!/bin/bash
#
# Syncronize local lieer-managed email

set -e

MAIL_DIR_NAME="${1}"
SECONDS=0
MAIL_DIR_PATH="/home/cashweaver/mail/${MAIL_DIR_NAME}"
LOG_FILE="/tmp/gmi-sync-log-${MAIL_DIR_NAME}.txt"
LOCK_FILE="/tmp/gmi-sync-${MAIL_DIR_NAME}.lock"

function cleanup() {
  rm "${LOCK_FILE}"
}
trap cleanup EXIT

if [ -f "${LOCK_FILE}" ]; then
  echo "Another sync is already in progress."
  exit 0
fi
touch "${LOCK_FILE}"

echo "[$(date) Sync started]" >> $LOG_FILE

cd "${MAIL_DIR_PATH}"
gmi sync >> $LOG_FILE

duration=$SECONDS
echo "[$(date) Sync ended ($(($duration / 60))min $(($duration % 60))sec)]" >> $LOG_FILE
