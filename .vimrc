"set nocompatible
filetype plugin indent on
syntax on
set background=light
colo solarized8_light

set nobackup
set noswapfile
set history=1000
set number
set laststatus=2
set modelines=5
set ts=2 sts=2 sw=2 expandtab
set incsearch
set nojoinspaces
set display+=lastline
set backspace=indent,eol,start

if has('gui_running')
  set guifont=Hack:h11
  set guioptions-=T "toolbar
  set guioptions-=m "menubar
  set guioptions-=r "right scroll
  set guioptions-=L "left scroll
endif

packadd minpac
call minpac#init()

call minpac#add('lifepillar/vim-solarized8')
call minpac#add('tpope/vim-fugitive')

command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()
