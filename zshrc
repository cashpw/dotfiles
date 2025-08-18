eval $(keychain --eval --quiet cashbweaver)

source "$HOME/.config/zsh/packages.zsh"
source "$HOME/.config/zsh/path.zsh"
source "$HOME/.config/zsh/aliases.zsh"
source "$HOME/.config/zsh/nvm.zsh"
source "$HOME/.config/zsh/environment.zsh"

eval "$(starship init zsh)"
