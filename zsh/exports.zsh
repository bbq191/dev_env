#!/bin/s
eval "$(/opt/homebrew/bin/brew shellenv)"

export HISTFILE="$ZDOTDIR/.zhistory"
HISTSIZE=1000000
SAVEHIST=1000000

export EDITOR="nvim"
export TERMINAL="kitty"
export BROWSER="safari"
export MANPAGER='nvim +Man!'
export MANWIDTH=999

export PRETTIERD_DEFAULT_CONFIG="$XDG_DATA_HOME/prettierd"
export POSTGRES="/opt/homebrew/opt/postgresql@15/bin"
# llvm config
export LLVM_PATH="/opt/homebrew/opt/llvm/bin"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++"

# rust config
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# npm config
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"

# path append
export PATH="$CARGO_HOME/bin:$LLVM_PATH:$LDFLAGS:$POSTGRES:$PRETTIERD_DEFAULT_CONFIG:$PATH"
# export PATH="$HOME/.local/bin":$PATH
# eval "$(thefuck --alias)"
eval "$(fnm env)"
eval "$(zoxide init zsh)"
