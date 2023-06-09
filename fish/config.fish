# proxy
set -x https_proxy http://127.0.0.1:6152
set -x http_proxy http://127.0.0.1:6152
set -x all_proxy socks5://127.0.0.1:6152

set -x XDG_CONFIG_HOME $HOME/.config
set fish_greeting ""
# starship initial
starship init fish | source
# fnm node env
fnm env --use-on-cd | source
# zoxide
zoxide init fish | source
# lesshst
set -x LESSHISTFILE -

# customer setting #############################################################
# Rust config
set -x CARGO_HOME $HOME/.local/cargo
set -x RUSTUP_HOME $HOME/.local/rustup

# neovim
set -x NEOVIM_HOME $HOME/.local/neovim

# starship
set -x STARSHIP_CONFIG $HOME/.config/starship/starship.toml
# end customer setting #########################################################

# subshell
if status is-interactive
  printf '\eP$f{"hook": "SourcedRcFileForWarp", "value": { "shell": "fish"}}\x9c'
end

# some useful shortcut
abbr --add gcm git commit -m
abbr --add gaa git add -A
abbr --add gpu git push
abbr --add gpl git pull

