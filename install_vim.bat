rm %home%\_vimrc
rm -rf %home%\vimfiles
mklink /H %home%\_vimrc %home%\dotfiles\.vimrc
mklink /J %home%\vimfiles %home%\dotfiles\vimfiles
