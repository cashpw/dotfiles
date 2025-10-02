eval $(keychain --eval --quiet cashbweaver)
eval $(keychain --eval --quiet ultracc)

source "$HOME/.config/zsh/packages.zsh"
source "$HOME/.config/zsh/path.zsh"
source "$HOME/.config/zsh/aliases.zsh"
source "$HOME/.config/zsh/environment.zsh"

source "$HOME/.config/zsh/nvm.zsh"
source "$HOME/.config/zsh/pyenv.zsh"

eval "$(starship init zsh)"
