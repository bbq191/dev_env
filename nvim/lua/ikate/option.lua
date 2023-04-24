-- i never use
local o = vim.opt -- for conciseness

vim.opt.guicursor = ""

-- line numbers
vim.opt.nu = true
vim.opt.relativenumber = true

-- tabs & indentation
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true -- expand tab to spaces
vim.opt.smartindent = true

-- line wrapping
vim.opt.wrap = false

-- system performance
vim.opt.swapfile = false
vim.opt.backup = false

vim.opt.undodir = os.getenv("XDG_CONFIG_HOME") .. "/nvim/undodir"
vim.opt.directory = os.getenv("XDG_CONFIG_HOME") .. "/nvim/tmp"
vim.opt.undofile = true

-- searching
vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.opt.colorcolumn = "100"

-- cursor line
vim.opt.cursorline = true -- highlight the current cursor line

-- clipboard
-- vim.opt.clipboard:append("unnamedplus") -- use system clipboard as default register

-- neovide config
if vim.g.neovide then
    vim.g.neovide_padding_top = 0
    vim.g.neovide_padding_bottom = 0
    vim.g.neovide_padding_right = 0
    vim.g.neovide_padding_left = 0

    vim.o.guifont = "CaskaydiaCove NFM:h16" -- text below applies for VimScript
    vim.g.neovide_remember_window_size = true
    -- Helper function for transparency formatting
    local alpha = function()
        local result = 255 * vim.g.transparency
        return string.format("%x", math.floor(result or 0.8))
    end
    -- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
    vim.g.neovide_transparency = 0.0
    vim.g.transparency = 0.4
    vim.g.neovide_background_color = "#0f1117" .. alpha()
end
