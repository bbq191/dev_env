-- short cut
local o = vim.opt -- for conciseness

o.guicursor = ""
-- line numbers
o.nu = true
o.relativenumber = true

-- tabs & indentation
o.tabstop = 4
o.softtabstop = 4
o.shiftwidth = 4
o.expandtab = true -- expand tab to spaces
o.smartindent = true

-- line wrapping
o.wrap = true

-- system performance
o.swapfile = false
o.backup = false
o.clipboard = "unnamed"

-- searching
o.hlsearch = false
o.incsearch = true

o.termguicolors = true

o.scrolloff = 8
o.signcolumn = "yes"
o.isfname:append("@-@")

o.updatetime = 50

o.colorcolumn = "80"

-- cursor line
o.cursorline = true -- highlight the current cursor line
