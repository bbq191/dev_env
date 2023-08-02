-- Lualine
require("lualine").setup({
    options = {
        -- ... your lualine config
        -- theme = "tokyonight-moon",
        theme = "auto",
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
        globalstatus = true,
        -- ... your lualine config
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch" },
        lualine_c = {
            {
                "diagnostics",
                symbols = { error = "E", warn = "W", info = "I", hint = "H" },
            },
            { "filetype", icon_only = true, separator = "", padding = { left = 1, right = 0 } },
            { "filename", path = 1, symbols = { modified = "  ", readonly = "", unnamed = "" } },
            -- stylua: ignore
            {
                function() return require("nvim-navic").get_location() end,
                cond = function() return package.loaded["nvim-navic"] and require("nvim-navic").is_available() end,
            },
        },
        lualine_x = {
            -- stylua: ignore
            {
                function() return require("noice").api.status.command.get() end,
                cond = function() return package.loaded["noice"] and require("noice").api.status.command.has() end,
            },
            -- stylua: ignore
            {
                function() return require("noice").api.status.mode.get() end,
                cond = function() return package.loaded["noice"] and require("noice").api.status.mode.has() end,
            },
            -- stylua: ignore
            {
                function() return "  " .. require("dap").status() end,
                cond = function() return package.loaded["dap"] and require("dap").status() ~= "" end,
            },
            { require("lazy.status").updates, cond = require("lazy.status").has_updates },
            {
                "diff",
                symbols = { added = "+", modified = "~", removed = "-" },
            },
        },
        lualine_y = {
            { "progress", separator = " ",                  padding = { left = 1, right = 0 } },
            { "location", padding = { left = 0, right = 1 } },
        },
        lualine_z = {
            function()
                return " " .. os.date("%R")
            end,
        },
    },
})

-- add mini indenet line
require("mini.indentscope").setup({
    -- Options which control scope computation
    options = {
        try_as_border = true,
    },
    symbol = "│",
})

require("dressing").setup({
    select = {
        get_config = function(opts)
            if opts.kind == "codeaction" then
                return {
                    backend = "nui",
                    nui = {
                        relative = "cursor",
                        max_width = 40,
                    },
                }
            end
        end,
    },
    input = {
        winhighlight = "NormalFloat:DiagnosticError",
    },
})

local navic = require("nvim-navic")

require("lspconfig").clangd.setup({
    on_attach = function(client, bufnr)
        navic.attach(client, bufnr)
    end,
})
