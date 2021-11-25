# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

function is_ssh_session() {
  [[ -n "$SSH_CLIENT" ]] || [[ -n "$SSH_TTY" ]]
}

eval $(dircolors $HOME/.config/dircolors/dircolors-solarized/dircolors.256dark)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

if ! is_ssh_session; then
  preferred_screenlayout_filepath="$HOME/.screenlayout/preferred.sh"
  if [[ -f "${preferred_screenlayout_filepath}" ]]; then
    source "${preferred_screenlayout_filepath}"
  fi
fi
