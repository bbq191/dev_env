local o = vim.opt -- for conciseness

-- line numbers
o.relativenumber = true -- show relative line numbers
o.number = true -- shows absolute line number on cursor line (when relative number is on)

-- tabs & indentation
o.tabstop = 2 -- 2 spaces for tabs (prettier default)
o.shiftwidth = 2 -- 2 spaces for indent width
o.expandtab = true -- expand tab to spaces
o.autoindent = true -- copy indent from current line when starting new one

-- line wrapping
o.wrap = false -- disable line wrapping

-- search settings
o.ignorecase = true -- ignore case when searching
o.smartcase = true -- if you include mixed case in your search, assumes you want case-sensitive

-- cursor line
o.cursorline = true -- highlight the current cursor line

-- appearance

-- turn on termguicolors for nightfly colorscheme to work
-- (have to use iterm2 or any other true color terminal)
o.termguicolors = true
o.background = "dark" -- colorschemes that can be light or dark will be made dark
o.signcolumn = "yes" -- show sign column so that text doesn't shift
o.scrolloff = 8 -- scroll more line
o.colorcolumn = "80"

-- backspace
o.backspace = "indent,eol,start" -- allow backspace on indent, end of line or insert mode start position
o.isfname:append("@-@")

-- clipboard
o.clipboard:append("unnamedplus") -- use system clipboard as default register

-- split windows
o.splitright = true -- split vertical window to the right
o.splitbelow = true -- split horizontal window to the bottom

-- turn off swapfile
o.swapfile = false
o.backup = false

-- all for gui
-- Put anything you want to happen only in Neovide here
if vim.g.neovide then
	o.guifont = "Iosevka Fixed:h15"

	-- Helper function for transparency formatting
	local alpha = function()
		return string.format("%x", math.floor(255 * vim.g.transparency or 0.9))
	end
	-- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
	vim.g.neovide_transparency = 0.0
	vim.g.transparency = 0.9
	vim.g.neovide_background_color = "#0f1117" .. alpha()
end
