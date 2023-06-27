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
