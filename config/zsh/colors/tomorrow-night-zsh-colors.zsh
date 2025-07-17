#!/usr/bin/env zsh
#
# Paste these contents into your ~/.zshrc before activating zsh-syntax-highlighting.
# Based on the Tomorrow Night color palette and the structure of the Catppuccin
# theme's zsh-syntax-highlighting.

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main cursor)
typeset -gA ZSH_HIGHLIGHT_STYLES

# Main highlighter styling: https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md

# Comments
ZSH_HIGHLIGHT_STYLES[comment]='fg=#969896'

# Functions/Commands
ZSH_HIGHLIGHT_STYLES[alias]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[global-alias]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[function]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[command]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=#b5bd68,italic'
ZSH_HIGHLIGHT_STYLES[autodirectory]='fg=#de935f,italic'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=#de935f'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=#de935f'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=#b294bb'

# Built-ins
ZSH_HIGHLIGHT_STYLES[builtin]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=#b5bd68'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=#b5bd68'

# Punctuation & Delimiters
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-unquoted]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[process-substitution-delimiter]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-delimiter]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[back-dollar-quoted-argument]='fg=#cc6666'

# Strings
ZSH_HIGHLIGHT_STYLES[command-substitution-quoted]='fg=#f0c674'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-quoted]='fg=#f0c674'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=#f0c674'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument-unclosed]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=#f0c674'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument-unclosed]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[rc-quote]='fg=#f0c674'

# Variables & Parameters
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument-unclosed]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[assign]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[named-fd]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[numeric-fd]='fg=#c5c8c6'

# Other
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[path]='fg=#c5c8c6,underline'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=#cc6666,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=#c5c8c6,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg=#cc6666,underline'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=#b294bb'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-unclosed]='fg=#cc6666'
ZSH_HIGHLIGHT_STYLES[redirection]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[arg0]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[default]='fg=#c5c8c6'
ZSH_HIGHLIGHT_STYLES[cursor]='fg=#c5c8c6'
