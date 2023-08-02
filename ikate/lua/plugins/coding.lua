return {
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.2",
		-- or                              , branch = '0.1.x',
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	{
		"folke/trouble.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			icons = false,
		},
	},
	{ "nvim-treesitter/playground" },
	{ "theprimeagen/harpoon" },
	{ "theprimeagen/refactoring.nvim" },
	{ "nvim-treesitter/nvim-treesitter-context" },
}
