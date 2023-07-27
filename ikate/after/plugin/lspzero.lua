local lsp = require("lsp-zero")

lsp.preset("recommended")

lsp.ensure_installed({
	"cmake",
	"rust_analyzer",
	"clangd",
})

-- Fix Undefined global 'vim'
lsp.nvim_workspace()

local cmp = require("cmp")
local cmp_select = { behavior = cmp.SelectBehavior.Select }
local cmp_mappings = lsp.defaults.cmp_mappings({
	["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
	["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
	["<C-y>"] = cmp.mapping.confirm({ select = true }),
	["<C-Space>"] = cmp.mapping.complete(),
})

cmp_mappings["<Tab>"] = nil
cmp_mappings["<S-Tab>"] = nil

lsp.setup_nvim_cmp({
	mapping = cmp_mappings,
})

lsp.set_preferences({
	suggest_lsp_servers = false,
	sign_icons = {
		error = "E",
		warn = "W",
		hint = "H",
		info = "I",
	},
})

lsp.on_attach(function(client, bufnr)
	local option = { buffer = bufnr, remap = false }

	vim.keymap.set("n", "gd", function()
		vim.lsp.buf.definition()
	end, option)
	vim.keymap.set("n", "K", function()
		vim.lsp.buf.hover()
	end, option)
	vim.keymap.set("n", "<leader>vws", function()
		vim.lsp.buf.workspace_symbol()
	end, option)
	vim.keymap.set("n", "<leader>vd", function()
		vim.diagnostic.open_float()
	end, option)
	vim.keymap.set("n", "[d", function()
		vim.diagnostic.goto_next()
	end, option)
	vim.keymap.set("n", "]d", function()
		vim.diagnostic.goto_prev()
	end, option)
	vim.keymap.set("n", "<leader>vca", function()
		vim.lsp.buf.code_action()
	end, option)
	vim.keymap.set("n", "<leader>vrr", function()
		vim.lsp.buf.references()
	end, option)
	vim.keymap.set("n", "<leader>vrn", function()
		vim.lsp.buf.rename()
	end, option)
	vim.keymap.set("i", "<C-h>", function()
		vim.lsp.buf.signature_help()
	end, option)
end)

lsp.format_mapping("gq", {
	format_opts = {
		async = false,
		timeout_ms = 10000,
	},
	servers = {
		["null-ls"] = { "yaml", "toml", "json" },
	},
})

lsp.setup()

-- null_ls cconfig
local null_ls = require("null-ls")

null_ls.setup({
	sources = {
		-- Replace these with the tools you want to install
		-- make sure the source name is supported by null-ls
		-- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
		null_ls.builtins.formatting.prettierd,
		null_ls.builtins.formatting.stylua,
	},
})

-- See mason-null-ls.nvim's documentation for more details:
-- https://github.com/jay-babu/mason-null-ls.nvim#setup
require("mason-null-ls").setup({
	ensure_installed = { "prettierd", "stylua" },
	automatic_installation = true,
})

vim.diagnostic.config({
	virtual_text = true,
})
