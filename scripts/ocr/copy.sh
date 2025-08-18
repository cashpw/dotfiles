#!/usr/bin/env sh

grim -g "$(slurp)" /tmp/ocr-in.png
tesseract /tmp/ocr-in.png /tmp/ocr-out
cat /tmp/ocr-out.txt | wl-copy
