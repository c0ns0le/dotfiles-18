rm %home%\.atom\init.coffee
mklink /H %home%\.atom\init.coffee %home%\dotfiles\atom\init.coffee

rm %home%\.atom\config.cson
mklink /H %home%\.atom\config.cson %home%\dotfiles\atom\config.cson

rm %home%\.atom\keymap.cson
mklink /H %home%\.atom\keymap.cson %home%\dotfiles\atom\keymap.cson

rm %home%\.atom\snippets.cson
mklink /H %home%\.atom\snippets.cson %home%\dotfiles\atom\snippets.cson

rm %home%\.atom\styles.less
mklink /H %home%\.atom\styles.less %home%\dotfiles\atom\styles.less
