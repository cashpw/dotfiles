#!/bin/bash

SECONDS=0
MAIL_DIR_PATH="/home/cashweaver/mail/cashbweaver.gmail"
LOG_FILE="/tmp/gmi-sync-log-cashbweaver.gmail.txt"
LIEER_DIR_PATH="/home/cashweaver/third_party/lieer"

echo "[$(date) Sync started]" >> $LOG_FILE

if [[ $(ps aux | grep gmi | grep sync | grep -V tail | wc -l) == 1 ]]; then
  echo "Another sync is already in progress."
  exit 0
fi

source "${LIEER_DIR_PATH}/.venv/bin/activate"

cd "${MAIL_DIR_PATH}"
"${LIEER_DIR_PATH}/gmi" sync >> $LOG_FILE

deactivate

duration=$SECONDS
echo "[$(date) Sync ended ($(($duration / 60))min $(($duration % 60))sec)]" >> $LOG_FILE
