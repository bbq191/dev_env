#!/bin/zsh
# proxy
export HTTPS_PROXY=http://127.0.0.1:7001
export HTTP_PROXY=http://127.0.0.1:7001
export ALL_PROXY=socks5://127.0.0.1:7001

# general
export LANG="en_US.UTF-8"
export DEFAULT_USER=$USER
export EDITOR="emacs -a """
export MANWIDTH=999
# export TERMINAL="kitty"
export TERM="xterm-256color"
export LESSHISTFILE=-

# XDG Path
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# USER Bin
export USER_BIN="$HOME/.local/bin"

# homebrew
export HOMEBREW_PREFIX="/opt/homebrew";
export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
export HOMEBREW_REPOSITORY="/opt/homebrew";
#export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
#export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
#export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";

# zap
[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
[ -f "$XDG_CONFIG_HOME/fzf.zsh" ] && source "$XDG_CONFIG_HOME/fzf.zsh"

# java config
export SDKMAN_DIR="$XDG_DATA_HOME/sdkman"
[[ -s "/Users/afu/.local/share/sdkman/bin/sdkman-init.sh" ]] && source "$SDKMAN_DIR/bin/sdkman-init.sh"

# rust home
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# npm config
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"

# path append
export PATH="$HOMEBREW_REPOSITORY/bin":"$HOMEBREW_REPOSITORY/sbin":"$CARGO_HOME/bin":"$LLVM_HOME/bin":"$GOPATH/bin":"$PATH"
