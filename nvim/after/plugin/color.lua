-- Set up default color scheme
function ColorMyNVIM(color)
	color = color or "rose-pine-dawn"
	vim.cmd.colorscheme(color)

	vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

ColorMyNVIM()
