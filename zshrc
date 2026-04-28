if [[ -f /usr/local/google/home/cashweaver/is-cloudtop || -f /usr/local/google/home/cashweaver/is-work-laptop ]]; then
    IS_WORK=1
else
    IS_WORK=0
fi

if [[ $IS_WORK -eq 0 ]]; then
    eval $(keychain --eval --quiet cashbweaver)
    eval $(keychain --eval --quiet ultracc)
fi

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line
bindkey -M vicmd 'v' edit-command-line

source "$HOME/.config/zsh/config.zsh"
source "$HOME/.config/zsh/packages.zsh"
source "$HOME/.config/zsh/path.zsh"
source "$HOME/.config/zsh/aliases.zsh"
source "$HOME/.config/zsh/environment.zsh"

if [[ $IS_WORK -eq 1 ]]; then
    [[ -f "$HOME/.config/zsh/work.zsh" ]] && source "$HOME/.config/zsh/work.zsh"
    [[ -f "$HOME/.config/zsh-work.zsh" ]] && source "$HOME/.config/zsh-work.zsh"
else
    [[ -f "$HOME/.config/zsh/personal.zsh" ]] && source "$HOME/.config/zsh/personal.zsh"
fi

eval "$(starship init zsh)"

