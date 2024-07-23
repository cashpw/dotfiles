#!/bin/bash
#
# Syncronize local lieer-managed email

set -e

MAIL_DIR_NAME="${1}"
SECONDS=0
MAIL_DIR_PATH="/home/cashweaver/mail/${MAIL_DIR_NAME}"
# Store the last time we updated local mail within the mail directory
LAST_UPDATE_PATH="${MAIL_DIR_PATH}/last-update.txt"
LOG_FILE="/tmp/gmi-sync-log-${MAIL_DIR_NAME}.txt"
LOCK_FILE="/tmp/gmi-sync-${MAIL_DIR_NAME}.lock"

function cleanup() {
  rm "${LOCK_FILE}"
}
trap cleanup EXIT

if [ ! -f "${LAST_UPDATE_PATH}" ]; then
  echo "Last update path ($LAST_UPDATE_PATH) missing. Creating it." >> $LOG_FILE
  echo "$(date +%s)" > $LAST_UPDATE_PATH
fi

if [ -f "${LOCK_FILE}" ]; then
echo "Another sync is already in progress." >> $LOG_FILE
  exit 0
fi
touch "${LOCK_FILE}"

LAST_UPDATE=$(cat "${LAST_UPDATE_PATH}")
NOW=$(date +%s)
echo "[$(date) Sync started]" >> $LOG_FILE

# Avoid pushing out-of-date email data.
#
# `gmi sync` runs `gmi push` then `gmi pull`. If we're out of date, we may push
# out-of-date data (for example: Move archived emails back into the inbox).
# `gmi push` is supposed to avoid this, but I've had it happen once or twice.
SECONDS_SINCE_LAST_UPDATE="$(($NOW - $LAST_UPDATE))"
cd "${MAIL_DIR_PATH}"
echo "seconds since last update: $SECONDS_SINCE_LAST_UPDATE"
if [[ $SECONDS_SINCE_LAST_UPDATE > 600 ]]; then
  echo "More than 10 minutes since last update ($SECONDS_SINCE_LAST_UPDATE). Pulling; no push." >> $LOG_FILE
  gmi pull >> $LOG_FILE
else
  gmi sync >> $LOG_FILE
fi
echo "$(date +%s)" > $LAST_UPDATE_PATH

duration=$SECONDS
echo "[$(date) Sync ended ($(($duration / 60))min $(($duration % 60))sec)]" >> $LOG_FILE
