xrdb ~/.Xresources

source ~/.profile

exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) i3
