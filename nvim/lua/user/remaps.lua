-- keymap
vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pe", vim.cmd.Ex) -- project exploer
vim.keymap.set("n", "<leader>w", ":w<cr>")    -- save current buffer
vim.keymap.set("n", "<leader>!", ":q!<cr>")   -- save and quit current buffer
vim.keymap.set("n", "<leader>q", ":q<CR>")    -- source current buffer

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "<C-d>", "<C-d>zz") -- zz means center
vim.keymap.set("n", "<C-u>", "<C-u>zz") -- always scoll to center
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- window management
vim.keymap.set("n", "<leader>sv", "<C-w>v")         -- split window vertically
vim.keymap.set("n", "<leader>sh", "<C-w>s")         -- split window horizontally
vim.keymap.set("n", "<leader>se", "<C-w>=")         -- make split windows equal width & height
vim.keymap.set("n", "<leader>sx", ":close<CR>")     -- close current split window

vim.keymap.set("n", "<leader>to", ":tabnew<CR>")    -- open new tab
vim.keymap.set("n", "<leader>tx", ":tabclose<CR>")  -- close current tab
vim.keymap.set("n", "<leader>tn", ":tabn<CR>")      -- go to next tab
vim.keymap.set("n", "<leader>tp", ":tabp<CR>")      -- go to previous tab

vim.keymap.set("n", "<leader>bd", ":bd<CR>")        -- close current buffer
vim.keymap.set("n", "<S-l>", ":bnext<CR>")          -- go to next buffer
vim.keymap.set("n", "<S-h>", ":bprevious<CR>")      -- go to previous buffer
vim.keymap.set("n", "<leader>bl", ":ls<CR>")        -- go to previous buffer

vim.keymap.set("n", "<C-t>", ":terminal<CR>")       -- open  terminal window

-- replays with copy
vim.keymap.set("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])
-- delete single character without copying into register
vim.keymap.set("n", "x", '"_x')

-- This is going to get me cancelled
vim.keymap.set("i", "<C-c>", "<Esc>")

vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)

vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

vim.keymap.set("n", "<leader>mr", "<cmd>CellularAutomaton make_it_rain<CR>");

vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)
