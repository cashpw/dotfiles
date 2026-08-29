# wlprop

`wlprop` is a script for inspecting Wayland window properties on Sway/wlroots compositors (similar to `xprop` on X11).

## Dependencies

Requires the following packages:
* `sway` (`swaymsg`)
* `jq`
* `slurp`
* `gawk` / `awk`

## Installation

The script is tracked directly inside this repository under `scripts/utility/wlprop.sh`. To make it accessible in your `$PATH`:

```bash
mkdir -p ~/.local/bin
ln -sf ~/.config/dotfiles/scripts/utility/wlprop.sh ~/.local/bin/wlprop
```
