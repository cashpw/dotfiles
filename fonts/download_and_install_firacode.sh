#!/usr/bin/env sh

if dpkg -l | grep "fonts-firacode"; then
  echo "Fira Code has already been installed."
else
  sudo apt install fonts-firacode
fi
