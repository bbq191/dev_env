# About
This share for my KiTTY terminal and LunarVIM config.

# KiTTY Terminal

```
# background set
background_opacity                1.8
background_image_layout           scaled
background_tint                   0.8
background_image                  ./image.png
# when scaled use this
background_image_linear           no

font_family                       TerminessTTF Nerd Font Mono
font_size                         18.0

```
Using tokyo night theme， CMD like this

```
kitty +kitten themes

```

# LunarVIM

My plugins choice, only vim-kitty is not theme, it can syntax highlighting for kitty conf file.

```
lvim.plugins = {
  {"folke/tokyonight.nvim"},
  {"projekt0n/github-nvim-theme"},
  {"fladson/vim-kitty"},
  {"tanvirtin/monokai.nvim"},
}

```

And I set colorscheme and transparent window.

```
lvim.colorscheme = "tokyonight"
lvim.transparent_window = true
````
It's perfect for me.

![screenshot]{./Screenshot 2023-02-22 at 11.17.58 AM.png}

Thanks!
