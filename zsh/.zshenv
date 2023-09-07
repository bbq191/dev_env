#!/bin/zsh

export https_proxy=http://127.0.0.1:6152 http_proxy=http://127.0.0.1:6152 all_proxy=socks5://127.0.0.1:6153

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
[ -f "$XDG_CONFIG_HOME/fzf.zsh" ] && source "XDG_CONFIG_HOME/fzf.zsh"

export LANG="en_US.UTF-8"
export DEFAULT_USER=$USER
export EDITOR="emacs -a """
export TERMINAL="kitty"
export BROWSER="safari"
export MANPAGER='nvim +Man!'
export MANWIDTH=999
export LESSHISTFILE=-
# pip user command
#export PIP_EXEC_USER="/Users/vincixu/Library/Python/3.11"

# prettierd
#export PRETTIERD_DEFAULT_CONFIG="$XDG_CONFIG_HOME/prettierd"

# postgresql
#export POSTGRES="/opt/homebrew/opt/postgresql@15/bin"

# llvm config
#export LLVM_PATH="/opt/homebrew/opt/llvm/bin"

# helix editor
export HELIX_RUNTIME="$HOME/Workspace/helix/runtime"
# rust home
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# npm config
#export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"

# path append
export PATH="$CARGO_HOME/bin":$PATH

