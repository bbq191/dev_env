-- set leader key to space
vim.g.mapleader = " "
local k = vim.keymap -- for conciseness

-- General Keymaps -------------------
k.set("n", "Q", "<nop>")
-- k.set("n", "J", "mzJ`z")
k.set("v", "J", ":m '>+1<CR>gv=gv")
k.set("v", "K", ":m '<-2<CR>gv=gv")

k.set("n", "<C-d>", "<C-d>zz")
k.set("n", "<C-u>", "<C-u>zz")
k.set("n", "n", "nzzzv")
k.set("n", "N", "Nzzzv")

-- use jk to exit insert mode
k.set("i", "jk", "<ESC>", { desc = "Exit insert mode with jk" })

-- clear search highlights
k.set("n", "<leader>nh", ":nohl<CR>", { desc = "Clear search highlights" })

k.set("n", "<leader>to", "<cmd>tabnew<CR>", { desc = "Open new tab" }) -- open new tab
k.set("n", "<leader>tx", "<cmd>tabclose<CR>", { desc = "Close current tab" }) -- close current tab
k.set("n", "<leader>tn", "<cmd>tabn<CR>", { desc = "Go to next tab" }) --  go to next tab
k.set("n", "<leader>tp", "<cmd>tabp<CR>", { desc = "Go to previous tab" }) --  go to previous tab
k.set("n", "<leader>tf", "<cmd>tabnew %<CR>", { desc = "Open current buffer in new tab" }) --  move current buffer to new tab

-- greatest remap ever
k.set("n", "x", '"_x')
-- k.set("x", "<leader>p", [["_dP]])
-- k.set({"n", "v"}, "<leader>y", [["+y]])
-- k.set("n", "<leader>Y", [["+Y]])
-- k.set({"n", "v"}, "<leader>d", [["_d]])

-- next greatest remap ever : asbjornHaland
k.set("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

