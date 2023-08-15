return {
  { "mbbill/undotree" },
  { "tpope/vim-fugitive" },
  { "lukas-reineke/indent-blankline.nvim" },
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
  {
    "SmiteshP/nvim-navic",
    lazy = true,
    init = function()
      vim.g.navic_silence = true
      require("user.util").on_attach(function(client, buffer)
        if client.server_capabilities.documentSymbolProvider then
          require("nvim-navic").attach(client, buffer)
        end
      end)
    end,
    opts = function()
      return {
        separator = " ",
        highlight = true,
        depth_limit = 5,
        icons = {
          Array = " ",
          Boolean = " ",
          Class = " ",
          Color = " ",
          Constant = " ",
          Constructor = " ",
          Copilot = " ",
          Enum = " ",
          EnumMember = " ",
          Event = " ",
          Field = " ",
          File = " ",
          Folder = " ",
          Function = " ",
          Interface = " ",
          Key = " ",
          Keyword = " ",
          Method = " ",
          Module = " ",
          Namespace = " ",
          Null = " ",
          Number = " ",
          Object = " ",
          Operator = " ",
          Package = " ",
          Property = " ",
          Reference = " ",
          Snippet = " ",
          String = " ",
          Struct = " ",
          Text = " ",
          TypeParameter = " ",
          Unit = " ",
          Value = " ",
          Variable = " ",
        },
      }
    end,
  },
}
