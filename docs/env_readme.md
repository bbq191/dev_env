> Base on [www.chrisatmachine.com](https://www.chrisatmachine.com/posts/01-macos-developer-setup)

First of all I deleted some parts that is not useful for me.

## System Settings

These are some settings I immediately set:

* bump key repeat up by one notch
* set turn display off after 20 mins while on battery 30 mins while charging
* turn on night shift

## Finder

There are a few tweaks to Finder that I think are necessary for it to be useable at all for a developer.

Check out this video at the timestamp provided for all of the updates: [video](https://youtu.be/2_ZbslLnshw?t=3017)

Not adapted for everyone, tweak as you wish.

### HostName

I usually like change my hostname and local hostname.

```shell
sudo scutil --set HostName xxx
sudo scutil --set LocalHostName xxx
```

## Command Tool

```shell
xcode-select --install
```

## Improving the Launcher

Spotlight sucks. Alfred sucks less. Raycast is actually pretty good.

```shell
brew install raycast
```

## Install brew

I like to install all of my software via a package manager and brew is the best way to do that on a mac imo.

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

(echo; echo 'eval "$(/opt/homebrew/bin/brew shellenv)"') >> /Users/vinci/.zshrc
eval "$(/opt/homebrew/bin/brew shellenv)"

brew analytics off
```

## Install a terminal

I know that iterm2 is a popular choice but it doesn’t have fontfallback which makes it garbage, kitty is the superior choice:

```shell
brew install kitty
```

### Update environment

Add `/etc/zshrc`

```shell
if [[ -z "$XDG_CONFIG_HOME" ]]
then
    export XDG_CONFIG_HOME="$HOME/.config"
fi

if [[ -z "$XDG_DATA_HOME" ]]
then
    export XDG_DATA_HOME="$HOME/.local/share"
fi

if [[ -z "$XDG_CACHE_HOME" ]]
then
    export XDG_CACHE_HOME="$HOME/.cache"
fi

if [[ -d "$XDG_CONFIG_HOME/zsh" ]]
then
    export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
fi
```

Using your own dotfile, this is my. To your zsh PATH and run:

```shell
ln -s $(pwd) ~/.config/
```

### Nerd fonts

```
brew install fontconfig
```

### Install getNF

It's a easy way to install nerd fonts.

```shell
git clone https://github.com/ronniedroid/getnf.git
cd getnf
./install.sh
```

run `getnf` from the terminal and it will represent you with a list of NerdFonts with fzf, select the ones you want, and let it do it's work.

By default the downloaded archives are removed, But if you give `getnf` the `-k` flag, it will not remove the Archives from the download directory

## Install a browser

I’m not familiar with **safari** as a browser, coming from Linux I usually use **brave** or **librewolf**.

In this case I’ll install **brave** and pin it to the dock and remove **safari**

```
brew install brave-browser
brew install librewolf

```

## CLI utilities

These are a few CLI utilities I can’t live without:

```
brew install tree    # allows you to see the outline of a directory 
brew install zoxide  # jump anywhere within your filesystem with z <foldername>
brew install ripgrep # blazingly fast grep
brew install fd      # blazingly fast find

```

### FZF

`fzf` gets its own section because of how useful it is

```shell
brew install fzf
$(brew --prefix)/opt/fzf/install

```

After installation you will be able to press `control-r` to interactively search history

Also you can pipe any output in to `fzf` and fuzzy search over it for example:

```shell
brew list | fzf

```

## Terminal System Monitors

```shell
brew install htop
brew install glances
brew install lazygit
```

## Web Tools

Some tools I use for interacting with the web.

```shell
brew install insomnia
brew install wget
brew install httpie
brew install jq
brew install ngrok
npm -g live-server
```

## Documentation

It’s nice to have examples for commands

```shell
cargo install tealdeer
```

## Programming Languages

Here is how I install and setup various programming languages I use.

### Python

btw **python 2** isn’t even included in MacOs anymore.

```shell
echo "alias python=/opt/homebrew/share/man/man1/python3.11.1" >> ~/.zshrc
echo "alias pip=/opt/homebrew/bin/pip3.11" >> ~/.zshrc
```

Install miniforge for apple silicon:

```shell
wget https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-MacOSX-arm64.sh -O ~/miniforge.sh

sh ~/miniforge.sh -b -f -p  $HOME/.miniforge

rm ~/miniforge.sh
```

Add the following in your `.zshrc` file:

```shell
if [ -f "$HOME/.miniforge/etc/profile.d/conda.sh" ]; then
      . "$HOME/.miniforge/etc/profile.d/conda.sh"
  else
      export PATH="$HOME/.miniforge/bin:$PATH"
fi

```

Open up a new terminal and the `conda` command should be available, if you don’t want to activate the `base` environment run the following:

```shell
conda config --set auto_activate_base false
```

### Java

I use **sdkman** to manage **java** and all things JVM based.

```shell
curl -s "https://get.sdkman.io" | bash

sdk install java
```

### Node

**fnm** is better than **nvm**, both are better than just installing node and running into endless permission issues.

```shell
brew install fnm

echo '"$(fnm env --use-on-cd)"' >> /Users/vinci/.zshrc

fnm install 18
```

### Rust

This should be all you need to install rust.

```shell
curl --proto '=https' --tlsv1.2 https://sh.rustup.rs -sSf | sh

```

### Go

This should be all you need to install go.

```shell
brew install go
```

I hate that they put a `go` directory right in my home directory. I personally change the `GOPATH` like this:

```shell
export GOPATH=$HOME/.local/share/go
export PATH=$HOME/.local/share/go/bin:$PATH
```

then remove the other one:

```shell
sudo rm -rf ~/go
```

## Install NeoVim 

As new developer you can watch this [video](https://www.youtube.com/watch?v=ctH-a-1eUME&list=PLhoH5vyxr6Qq41NFL4GvhFp-WLd5xzIzZ) first, learning these videos will help you to use lunarvim.  [[NeoVim IDE Setup]]

Neovim is my text editor of choice

I install Neovim from source you can probably just:

```
brew install neovim --HEAD
```

## Github CLI

Very convenient CLI utility if you use Github

```shell
brew install gh
gh auth login
```

