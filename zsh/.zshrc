#!/bin/zsh

# 环境变量设定
eval "$(/opt/homebrew/bin/brew shellenv)"

# proxy
# export HTTPS_PROXY=http://127.0.0.1:6153
# export HTTP_PROXY=http://127.0.0.1:6153
# export ALL_PROXY=socks5://127.0.0.1:6153

# general
export LANG="en_US.UTF-8"
export DEFAULT_USER=$USER
export EDITOR="nvim"
export MANWIDTH=999
# export TERMINAL="kitty"
export TERM="xterm-256color"
export LESSHISTFILE=-

# XDG Path
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR="$HOME/.local/run"

HISTFILE="$XDG_STATE_HOME"/zsh/history
# Completion files: Use XDG dirs
[ -d "$XDG_CACHE_HOME"/zsh ] || mkdir -p "$XDG_CACHE_HOME"/zsh
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME"/zsh/zcompcache

# 加载补全系统
autoload -Uz compinit
compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-$ZSH_VERSION

# USER Bin
export USER_BIN="$HOME/.local/bin"

# zap
if [ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ]; then
  source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"
fi

if [ -f "$XDG_CONFIG_HOME/fzf.zsh" ]; then
  source "$XDG_CONFIG_HOME/fzf.zsh"
fi

# ADB platform
export ANDROID_USER_HOME="$XDG_DATA_HOME/android"

# go home
export GOPATH="$XDG_DATA_HOME/go"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"

# java config
export SDKMAN_DIR="$XDG_DATA_HOME/sdkman"
if [ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]; then
  source "$SDKMAN_DIR/bin/sdkman-init.sh"
fi

# rust home
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# python home
export PYTHON_HOME="/opt/homebrew/opt/python/libexec/bin"

# npm config
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"

# maven repository
export MAVEN_OPTS=-Dmaven.repo.local="$XDG_DATA_HOME"/maven/repository

# system tools
export curl_path="/opt/homebrew/opt/curl/bin"
export make_path="/opt/homebrew/opt/make/libexec/gnubin"

# Added by Toolbox App
export TOOLBOX="$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# path append
export PATH="$GNUPGHOME:$CARGO_HOME/bin:$PYTHON_HOME:$GOPATH:$GOMODCACHE:$ANDROID_USER_HOME:$make_path:$curl_path:$TOOLBOX:$PATH"

# 个人习惯设定
HISTSIZE=999
SAVEHIST=1000
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify

# completion using arrow keys (based on history)
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward
# plugins
plug "zap-zsh/supercharge"
plug "zap-zsh/exa"
plug "esc/conda-zsh-completion"
plug "hlissner/zsh-autopair"
plug "MichaelAquilina/zsh-you-should-use"
plug "zap-zsh/fzf"
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/zap-prompt"
plug "zsh-users/zsh-syntax-highlighting"
plug "wintermi/zsh-brew"
plug "Aloxaf/fzf-tab"

# Additional configurations
eval "$(thefuck --alias)"
eval "$(gh copilot alias -- zsh)"
eval "$(fnm env --use-on-cd)"
eval "$(zoxide init zsh)"

# -- Use fd instead of fzf --
export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"

# Use fd for listing path candidates
_fzf_compgen_path() {
  fd --hidden --exclude .git . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type=d --hidden --exclude .git . "$1"
}

# --- setup fzf theme ---
fg="#CBE0F0"
bg="#011628"
bg_highlight="#143652"
purple="#B388FF"
blue="#06BCE4"
cyan="#2CF9ED"

export FZF_DEFAULT_OPTS="--color=fg:${fg},bg:${bg},hl:${purple},fg+:${fg},bg+:${bg_highlight},hl+:${purple},info:${blue},prompt:${cyan},pointer:${cyan},marker:${cyan},spinner:${cyan},header:${cyan}"

# Setup fzf previews
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

# Advanced customization of fzf options via _fzf_comprun function
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'eza --tree --color=always {} | head -200' "$@" ;;
    export|unset) fzf --preview "eval 'echo $'{}"         "$@" ;;
    ssh)          fzf --preview 'dig {}'                   "$@" ;;
    *)            fzf --preview "bat -n --color=always --line-range :500 {}" "$@" ;;
  esac
}

# Setup fzf-git
source ~/Developer/fzf-git.sh/fzf-git.sh

# confirm before overwriting something
alias cd="z"
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# easier to read disk
alias df='df -h'     # human-readable sizes
alias free='free -m' # show sizes in MB

# get top process eating memory
alias psmem='ps aux | sort -nr -k 4 | head -5'

# get top process eating cpu
alias pscpu='ps aux | sort -nr -k 3 | head -5'

alias gcm="git checkout master"
alias gcs="git checkout stable"

# subversion
alias svn="svn --config-dir \"$XDG_CONFIG_HOME\"/subversion"

# vscode
alias code='code-insiders --extensions-dir "$VSCODE_PORTABLE/extensions"'

# yarn
alias yarn='yarn --use-yarnrc "$XDG_CONFIG_HOME/yarn/config"'

# brew
alias brewuac='brew update -v && brew upgrade -v && brew cleanup --prune=all -v'
alias brewun='brew uninstall "$@"'
alias brewin='brew install "$@"'
