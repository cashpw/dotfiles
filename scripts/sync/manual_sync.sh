#!/usr/bin/env sh
# Syncronize notes across machines
#
# This is an alternative to Syncthing after it was banned on corporate devices.

unison \
  -auto \
  -batch \
  -ignore "Regex .unison.*" \
  /home/cashweaver/proj/notes \
  "ssh://cashweaver.c.googlers.com//usr/local/google/home/cashweaver/proj/notes"
