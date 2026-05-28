#!/bin/bash

HOST="cashweaver.c.googlers.com"
TIMEOUT=2

# Run remote command to get memory and CPU.
# Use BatchMode to fail fast without prompting for credentials.
# Use ConnectTimeout to fail fast if offline.
stats=$(ssh -o ConnectTimeout=$TIMEOUT -o BatchMode=yes $HOST "free | grep Mem | awk '{print int(\$3/\$2 * 100)}'; vmstat 1 2 | tail -n 1 | awk '{print 100 - \$15}'" 2>/dev/null)

if [ $? -eq 0 ] && [ -n "$stats" ]; then
  ram=$(echo "$stats" | sed -n '1p')
  cpu=$(echo "$stats" | sed -n '2p')
  
  # Print formatted output with Material Design icons and text
  # U+F0150 (󰅐) for Cloud
  printf "\U000f0150 RAM %s%% CPU %s%%\n" "$ram" "$cpu"
else
  # U+F0150 (󰅐) for Cloud
  printf "\U000f0150 offline\n"
fi
