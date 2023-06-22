#!/bin/sh
export https_proxy=http://127.0.0.1:6152 http_proxy=http://127.0.0.1:6152 all_proxy=socks5://127.0.0.1:6153

eval "$(/opt/homebrew/bin/brew shellenv)"

export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
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
