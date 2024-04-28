-- set leader key to space
vim.g.mapleader = " "
local k = vim.keymap -- for conciseness

k.set("n", "<leader>pv", vim.cmd.Ex)
-- General Keymaps -------------------
k.set("n", "Q", "<nop>")
k.set("v", "K", ":m '<-2<CR>gv=gv")

k.set("n", "<C-d>", "<C-d>zz")
k.set("n", "<C-u>", "<C-u>zz")
k.set("n", "n", "nzzzv")
k.set("n", "N", "Nzzzv")

-- use jk to exit insert mode
k.set("i", "jk", "<ESC>", { desc = "Exit insert mode with jk" })

-- k.set("n", "<leader>bx", "<cmd>close<CR>", { desc = "Close current split" }) -- close current split window
-- k.set("n", "<leader>to", "<cmd>tabnew<CR>", { desc = "Open new tab" }) -- open new tab
-- k.set("n", "<leader>tx", "<cmd>tabclose<CR>", { desc = "Close current tab" }) -- close current tab
-- k.set("n", "<leader>tn", "<cmd>tabn<CR>", { desc = "Go to next tab" }) --  go to next tab
-- k.set("n", "<leader>tp", "<cmd>tabp<CR>", { desc = "Go to previous tab" }) --  go to previous tab
-- k.set("n", "<leader>tf", "<cmd>tabnew %<CR>", { desc = "Open current buffer in new tab" }) --  move current buffer to new tab

-- greatest remap ever
k.set("n", "x", '"_x')
k.set("x", "<leader>p", [["_dP]])
k.set({ "n", "v" }, "<leader>y", [["+y]])
k.set("n", "<leader>Y", [["+Y]])
k.set({ "n", "v" }, "<leader>d", [["_d]])

k.set("n", "<leader>vwm", function()
	require("vim-with-me").StartVimWithMe()
end)
k.set("n", "<leader>svwm", function()
	require("vim-with-me").StopVimWithMe()
end)
k.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")

k.set("n", "<C-k>", "<cmd>cnext<CR>zz")
k.set("n", "<C-j>", "<cmd>cprev<CR>zz")
k.set("n", "<leader>k", "<cmd>lnext<CR>zz")
k.set("n", "<leader>j", "<cmd>lprev<CR>zz")

k.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
k.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

k.set("n", "<leader>ee", "oif err != nil {<CR>}<Esc>Oreturn err<Esc>")

k.set("n", "<leader>mr", "<cmd>CellularAutomaton make_it_rain<CR>")

k.set("n", "<leader><leader>", function()
	vim.cmd("so")
end)
