# First of all
export https_proxy=http://127.0.0.1:6152 http_proxy=http://127.0.0.1:6152 all_proxy=socks5://127.0.0.1:6153

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Created by Zap installer
[ -f "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh" ] && source "${XDG_DATA_HOME:-$HOME/.local/share}/zap/zap.zsh"

# source
plug "$XDG_CONFIG_HOME/zsh/aliases.zsh"
plug "$XDG_CONFIG_HOME/zsh/exports.zsh"

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
# autoload -Uz compinit
# compinit
