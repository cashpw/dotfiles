#!/bin/bash

HOST="cashweaver.c.googlers.com"
TIMEOUT=2

# Run remote command to get memory, CPU, and disk.
# Use BatchMode to fail fast without prompting for credentials.
# Use ConnectTimeout to fail fast if offline.
stats=$(ssh -o ConnectTimeout=$TIMEOUT -o BatchMode=yes $HOST "free | grep Mem | awk '{print int(\$3/\$2 * 100)}'; vmstat 1 2 | tail -n 1 | awk '{print 100 - \$15}'; df / | awk 'NR==2 {print int(\$3/\$2 * 100)}'" 2>/dev/null)

if [ $? -eq 0 ] && [ -n "$stats" ]; then
  ram=$(echo "$stats" | sed -n '1p')
  cpu=$(echo "$stats" | sed -n '2p')
  disk=$(echo "$stats" | sed -n '3p')
  
  printf "󰅟 RAM %s%% 󰅟 CPU %s%% 󰅟 DISK %s%%\n" "$ram" "$cpu" "$disk"
else
  printf "󰅟 offline\n"
fi
