###########################################################################################################
# First of all
export https_proxy=http://127.0.0.1:6152 http_proxy=http://127.0.0.1:6152 all_proxy=socks5://127.0.0.1:6153

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
# export HISTFILE="$ZDOTDIR/.zhistory"
export EDITOR="nvim"
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
# Created by Zap installer
# [ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
source "$XDG_DATA_HOME/zap/zap.zsh"

# source
# plug "$XDG_CONFIG_HOME/zsh/aliases.zsh"
# plug "$XDG_CONFIG_HOME/zsh/exports.zsh"

# plugins
plug "zap-zsh/supercharge"
plug "esc/conda-zsh-completion"
plug "hlissner/zsh-autopair"
plug "zap-zsh/fzf"
plug "zap-zsh/exa"
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/zap-prompt"
plug "zsh-users/zsh-syntax-highlighting"
plug "wintermi/zsh-brew"
plug "Aloxaf/fzf-tab"

# Load and initialise completion system
autoload -Uz compinit
compinit
############################################################################################################
# eval "$(thefuck --alias)"
eval "$(/opt/homebrew/bin/brew shellenv)"
eval "$(fnm env)"
eval "$(zoxide init zsh)"
############################################################################################################
# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# easier to read disk
alias df='df -h'     # human-readable sizes
alias free='free -m' # show sizes in MB

# get top process eating memory
alias psmem='ps aux | sort -nr -k 4 | head -5'

# get top process eating cpu ##
alias pscpu='ps aux | sort -nr -k 3 | head -5'

alias gcm="git checkout master"
alias gcs="git checkout stable"

if [[ $TERM == "xterm-kitty" ]]; then
  alias ssh="kitty +kitten ssh"
fi

