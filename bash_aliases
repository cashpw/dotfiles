# Basic
# ==============================================================================

alias c="clear"

alias l="ls"
alias la="ls -a"
alias ll="ls -la"

alias ..="cd ../"
alias ..2="cd ../../"
alias ..3="cd ../../../"
alias ..4="cd ../../../../"
alias ..5="cd ../../../../../"

alias sba='source ~/.bash_aliases'
alias sp='source ~/.profile'

alias v='nvim'

# Convenience
alias pn='bash ~/.scripts/sync/pull_push_notes.sh'

# Editor
alias e='emacsclient -t -a ""'

# Applications
## Fabric
alias fabric='~/third_party/fabric/fabric'
alias transcript='fabric --transcript --youtube '
## Redshift
alias rson='redshift -O 3700 -l 37.63:-122.41 -b 1.0:0.7'
alias rsdark='redshift -O 3700 -l 37.63:-122.41 -b 0.7:0.7'
alias rsoff='redshift -x'
## Zotra
alias zotra='node ~/.local/share/zotra-server/bin/index.js'
