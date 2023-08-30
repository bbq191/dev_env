# add plugins ##########################################################################################
[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"

# plugins
plug "zap-zsh/supercharge"
plug "zap-zsh/exa"
plug "esc/conda-zsh-completion"
plug "hlissner/zsh-autopair"

plug "zap-zsh/fzf"
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/zap-prompt"
plug "zsh-users/zsh-syntax-highlighting"
plug "wintermi/zsh-brew"
plug "Aloxaf/fzf-tab"

# Created by Zap installer

# Load and initialise completion system
autoload -Uz compinit
 compinit

############################################################################################################
# eval "$(thefuck --alias)"
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

