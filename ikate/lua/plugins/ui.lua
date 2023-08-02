return {
  { "theprimeagen/harpoon" },
  { "theprimeagen/refactoring.nvim" },
  { "folke/zen-mode.nvim" },
  { "mbbill/undotree" },
  { "tpope/vim-fugitive" },
  { "echasnovski/mini.indentscope", version = false },
  { "nvim-lualine/lualine.nvim", event = "VeryLazy" },
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = {
      { "MunifTanjim/nui.nvim" },
      {
        "rcarriga/nvim-notify",
        opts = {
          stages = "slide",
          timeout = 5000,
          render = "compact",
        },
      },
    },
  },
  { "akinsho/bufferline.nvim" },
  { "echasnovski/mini.animate", version = false },
}
