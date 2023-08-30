export https_proxy=http://127.0.0.1:6152 http_proxy=http://127.0.0.1:6152 all_proxy=socks5://127.0.0.1:6153

eval "$(/opt/homebrew/bin/brew shellenv)"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export LANG="en_US.UTF-8"
export DEFAULT_USER=$USER
export EDITOR="emacs -a """
export TERMINAL="kitty"
export BROWSER="safari"
export MANPAGER='nvim +Man!'
export MANWIDTH=999
# pip user command
export PIP_EXEC_USER="/Users/vincixu/Library/Python/3.11"
# prettierd
export PRETTIERD_DEFAULT_CONFIG="$XDG_CONFIG_HOME/prettierd"
# postgresql
export POSTGRES="/opt/homebrew/opt/postgresql@15/bin"
# llvm config
export LLVM_PATH="/opt/homebrew/opt/llvm/bin"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++"
# rust config
# export CARGO_HOME="$XDG_DATA_HOME/cargo"
# xport RUSTUP_HOME="$XDG_DATA_HOME/rustup"
# npm config
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
# path append
HISTSIZE=1000000
SAVEHIST=1000000
export PATH="$PIP_EXEC_USER/bin":$LLVM_PATH:$LDFLAGS:$POSTGRES:$PRETTIERD_DEFAULT_CONFIG:$PATH
###########################################################################################################
export PATH="$HOME/.cargo/bin:$PATH"

