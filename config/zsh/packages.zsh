#!/usr/bin/env zsh

source "$HOME/.config/antigen/antigen.zsh"

antigen bundle jeffreytse/zsh-vi-mode

antigen bundle zsh-users/zsh-completions

antigen bundle zsh-users/zsh-autosuggestions

antigen bundle zsh-users/zsh-syntax-highlighting

antigen bundle zsh-users/zsh-history-substring-search
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

antigen bundle MichaelAquilina/zsh-you-should-use

export NVM_DIR="$HOME/.config/nvm"
antigen bundle lukechilds/zsh-nvm

antigen apply
