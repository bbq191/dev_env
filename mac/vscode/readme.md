# vscode config

```css
body {
  background-size: cover;
  background-repeat: no-repeat;
  background-position: center;
  opacity: 0.85;
  background-image: url('data:image/webp;base64,yourcode');
}
```

# run from automator

```sh
export XDG_DATA_HOME="$HOME/.local/share"

# vscode
export VSCODE_PORTABLE="$XDG_DATA_HOME/vscode"

# path append
export PATH="$VSCODE_PORTABLE:$PATH"

/usr/local/bin/code-insiders --extensions-dir "$VSCODE_PORTABLE/extensions" "$@"
```
