# wlprop

`wlprop` is a script for inspecting Wayland window properties on Sway/wlroots compositors (similar to `xprop` on X11).

## Dependencies

Requires the following packages:
* `sway` (`swaymsg`)
* `jq`
* `slurp`
* `gawk` / `awk`

## Installation

Install from the local repository script at `~/third_party/wlprop/wlprop.sh`:

```bash
mkdir -p ~/.local/bin
cp ~/third_party/wlprop/wlprop.sh ~/.local/bin/wlprop
chmod +x ~/.local/bin/wlprop
```
