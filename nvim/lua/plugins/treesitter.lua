return { -- Using treesitter
    {
        'nvim-treesitter/nvim-treesitter',
        build = function()
            local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
            ts_update()
        end,
        opts = function(_, opts)
            if type(opts.ensure_installed) == "table" then
                vim.list_extend(opts.ensure_installed, {
                    "comment",
                    "diff",
                    "dot",
                    "git_rebase",
                    "gitattributes",
                    "gitcommit",
                    "gitignore",
                    "http",
                    "jq",
                    "rust",
                    "sql"
                })
            end
        end
    },
    { "nvim-treesitter/nvim-treesitter-context" },
    { 'nvim-treesitter/playground' },
}
