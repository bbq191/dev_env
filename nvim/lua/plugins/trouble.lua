return {
    {
        "folke/trouble.nvim",
        dependencies = "nvim-tree/nvim-web-devicons", -- if you do not nees icond comment here
        config = function()
            require("trouble").setup {
                -- icons = false, -- if you do not need icons open here
            }
        end
    }
}
