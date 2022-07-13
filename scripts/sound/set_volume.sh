#!/bin/bash
# Sets the volume for a set of outputs
#
# I've had some audio issues in the past. Hopefully this brute-force approach solves them.

outputs=(
  "Master"
  "Headphone"
  "Front"
)
for output in "${outputs[@]}"; do
  amixer set "$output" $1
done
