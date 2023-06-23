#!/bin/s
eval "$(/opt/homebrew/bin/brew shellenv)"

export HISTFILE="$ZDOTDIR/.zhistory"
HISTSIZE=1000000
SAVEHIST=1000000

export EDITOR="nvim"
export TERMINAL="kitty"
export BROWSER="safari"
export PATH="$HOME/.local/bin":$PATH
export MANPAGER='nvim +Man!'
export MANWIDTH=999

# export PATH="$HOME/.local/cargo/bin":$PATH
# export PATH="$HOME/.local/rustup/toolchains/nightly-aarch64-apple-darwin/bin":$PATH
# eval "$(thefuck --alias)"
# eval "$(fnm env)"
eval "$(zoxide init zsh)"
