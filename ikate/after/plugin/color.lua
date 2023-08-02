require("rose-pine").setup({
  --- @usage 'auto'|'main'|'moon'|'dawn'
  variant = "auto",
  --- @usage 'main'|'moon'|'dawn'
  dark_variant = "main",
  bold_vert_split = false,
  dim_nc_background = false,
  disable_background = true,
  disable_float_background = false,
  disable_italics = false,

  --- @usage string hex value or named color from rosepinetheme.com/palette
  groups = {
    background = "base",
    background_nc = "_experimental_nc",
    panel = "surface",
    panel_nc = "base",
    border = "highlight_med",
    comment = "muted",
    link = "iris",
    punctuation = "subtle",

    error = "love",
    hint = "iris",
    info = "foam",
    warn = "gold",

    headings = {
      h1 = "iris",
      h2 = "foam",
      h3 = "rose",
      h4 = "gold",
      h5 = "pine",
      h6 = "foam",
    },
    -- or set all headings at once
    -- headings = 'subtle'
  },

  -- Change specific vim highlight groups
  -- https://github.com/rose-pine/neovim/wiki/Recipes
  highlight_groups = {
    ColorColumn = { bg = "rose" },

    -- Blend colours against the "base" background
    CursorLine = { bg = "foam", blend = 10 },
    StatusLine = { fg = "love", bg = "love", blend = 10 },

    -- By default each group adds to the existing config.
    -- If you only want to set what is written in this config exactly,
    -- you can set the inherit option:
    Search = { bg = "gold", inherit = false },
  },
})

require("tokyonight").setup({
  style = "moon", -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
  light_style = "day", -- The theme is used when the background is set to light
  transparent = true, -- Enable this to disable setting the background color
  sidebars = { "qf", "vista_kind", "terminal", "packer" },
  hide_inactive_statusline = true, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
  dim_inactive = true, -- dims inactive windows
  lualine_bold = true, -- When `true`, section headers in the lualine theme will be bold

  on_colors = function(colors)
    colors.hint = colors.orange
    colors.error = "#ff0000"
  end,
})

function ColorMyPencils(color)
  color = "rose-pine" or "tokyonight"
  vim.cmd.colorscheme(color)

  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

ColorMyPencils()
