#!/bin/zsh

#eval "$(/opt/homebrew/bin/brew shellenv)"

HISTSIZE=1000000
SAVEHIST=1000000

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

# Load and initialise completion system
autoload -Uz compinit
compinit

############################################################################################################
# eval "$(thefuck --alias)"
eval "$(fnm env --use-on-cd)"
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
