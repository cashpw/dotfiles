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
STATUS_FILE="/tmp/gmi-sync-${MAIL_DIR_NAME}-status.txt"

function log() {
  local message="$1"
  local file="$2"

  echo "[$(date)] $message" >> "$file"
}

function sync() {
  LAST_UPDATE=$(cat "${LAST_UPDATE_PATH}")
  NOW=$(date +%s)
  log "Sync started" $LOG_FILE

  # Avoid pushing out-of-date email data.
  #
  # `gmi sync` runs `gmi push` then `gmi pull`. If we're out of date, we may push
  # out-of-date data (for example: Move archived emails back into the inbox).
  # `gmi push` is supposed to avoid this, but I've had it happen once or twice.
  SECONDS_SINCE_LAST_UPDATE="$(($NOW - $LAST_UPDATE))"
  cd "${MAIL_DIR_PATH}"
  log "$SECONDS_SINCE_LAST_UPDATE seconds since last update" $LOG_FILE
  if [[ $SECONDS_SINCE_LAST_UPDATE > 600 ]]; then
    log "Pull: Begin" $LOG_FILE
    gmi pull >> $LOG_FILE
    log "Pull: Complete" $LOG_FILE
  else
    log "Sync: Begin" $LOG_FILE
    gmi sync >> $LOG_FILE
    log "Sync: Complete" $LOG_FILE
  fi

  echo "$(date +%s)" > $LAST_UPDATE_PATH

  duration=$SECONDS
  log "Ran for $(($duration / 60))min $(($duration % 60))sec" $LOG_FILE
}

function exit_check() {
  rm "${LOCK_FILE}"

  if [[ $? -ne 0 ]]; then
    notify-send "Error syncing email."
  fi
}

#---

trap exit_check EXIT

if [ ! -f "${LAST_UPDATE_PATH}" ]; then
  log "Last update path ($LAST_UPDATE_PATH) missing. Creating it." $LOG_FILE
  echo "$(date +%s)" > $LAST_UPDATE_PATH
fi

if [ -f "${LOCK_FILE}" ]; then
log "Another sync is already in progress." $LOG_FILE
  exit 0
fi
touch "${LOCK_FILE}"

#echo "⏳" > $STATUS_FILE
sync
#echo "✅" > $STATUS_FILE
