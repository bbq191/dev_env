# By default we assume the configuration is located at a variable
#     called "exports".
# You can change this with `-n any_name` in the CLI.

from mw2fcitx.tweaks.moegirl import tweaks

exports = {
    # Source configurations.
    "source": {
        # MediaWiki api.php path, if to fetch titles from online.
        # 【⬇️api_path这一项，指定百科网站的api.php⬇️】
        # "api_path": "https://wiki.biligame.com/ys/api.php",
        # "api_path": "https://wiki.biligame.com/sr/api.php",
        # "api_path": "https://wiki.biligame.com/zzz/api.php",
        "api_path": "https://wiki.biligame.com/wukong/api.php",
        # Title file path, if to fetch titles from local file. (optional)
        # Only works if api_path is absent.
        # 【⬇️file_path这一项，指定用于暂存获取内容的文件名⬇️】
        "file_path": "titles.txt",
        "kwargs": {
            # Title number limit for online fetching. (optional)
            # Only works if api_path is provided.
            #"title_limit": 120,
            # Title list export path. (optional)
            # 【⬇️output这一项是可选的，指定词条列表的输出路径。⬇️】
            #"output": "titles.txt"
        }
    },
    # Tweaks configurations as an list.
    # Every tweak function accepts a list of titles and return
    #     a list of title.
    "tweaks":
        tweaks,
    # Converter configurations.
    "converter": {
        # opencc is a built-in converter.
        # For custom converter functions, just give the function itself.
        "use": "opencc",
        "kwargs": {}
    },
    # Generator configurations.
    "generator": {
        # rime is a built-in generator.
        # For custom generator functions, just give the function itself.
        # 【⬇️MW2Fcitx也可以为其他输入法生成词库。默认为RIME生成⬇️】
        "use": "rime",
        "kwargs": {
            # Destination dictionary filename. (optional)
            # 【⬇️output这一项，指定输出的词库文件名，建议以YAML为扩展名】
            "output": "wukong.dict.yaml"
        }
    }
}
