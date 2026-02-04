#!/bin/bash

SECONDS=0
MAIL_DIR_PATH="/home/cashpw/mail/cash.cashpw"
LOG_FILE="/tmp/gmi-sync-log-cash.cashpw.txt"
GMI="/home/cashpw/.local/bin/gmi"

echo "[$(date +"%F %T%Z") cash@cashpw.com: Sync started]" >> $LOG_FILE

if [[ $(ps aux | grep gmi | grep sync | grep -v tail | wc -l) == 1 ]]; then
  echo "Another sync is already in progress." >> $LOG_FILE
else
  cd "${MAIL_DIR_PATH}"
  $GMI sync >> $LOG_FILE 2>&1
fi

duration=$SECONDS
echo "[$(date +"%F %T%Z") cash@cashpw.com: Sync ended after ($(($duration / 60))min $(($duration % 60))sec)]" >> $LOG_FILE
