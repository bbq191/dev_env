#!/bin/zsh

export https_proxy=http://127.0.0.1:6152 http_proxy=http://127.0.0.1:6152 all_proxy=socks5://127.0.0.1:6153

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# homebrew
export HOMEBREW_PREFIX="/opt/homebrew";
export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
export HOMEBREW_REPOSITORY="/opt/homebrew";
#export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
#export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
#export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";

[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
[ -f "$XDG_CONFIG_HOME/fzf.zsh" ] && source "XDG_CONFIG_HOME/fzf.zsh"

export LANG="en_US.UTF-8"
export DEFAULT_USER=$USER
export EDITOR="emacs -a """
export MANWIDTH=999
export LESSHISTFILE=-
# pip user command
#export PIP_EXEC_USER="/Users/vincixu/Library/Python/3.11"

# prettierd
#export PRETTIERD_DEFAULT_CONFIG="$XDG_CONFIG_HOME/prettierd"

# postgresql
#export POSTGRES="/opt/homebrew/opt/postgresql@15/bin"

# llvm config
export LLVM_HOME="/opt/homebrew/opt/llvm"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

# rust home
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# npm config
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"

# path append
export PATH=/opt/homebrew/bin:/opt/homebrew/sbin:/Users/afu/.local/share/cargo/bin:/Users/afu/.local/share/rustup/toolchains/stable-aarch64-apple-darwin/bin:/opt/homebrew/opt/llvm/bin:$PATH
