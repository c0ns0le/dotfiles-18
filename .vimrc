"set nocompatible
filetype plugin indent on
syntax on
set background=light
colo solarized8_light

set nojoinspaces
set signcolumn=yes
set wildmenu
set nobackup
set noswapfile
set clipboard=unnamed
set shortmess=a
set encoding=utf-8
set hidden
set history=1000
set laststatus=2
set history=1000
set relativenumber
set backspace=indent,eol,start
set tabstop=2
set shiftwidth=2
set expandtab
set incsearch
set ignorecase
set smartcase
set hlsearch
set autoread
set autowrite
set visualbell
set lazyredraw
set scrolloff=2

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
call minpac#add('leafgarland/typescript-vim')

command! PackUpdate call minpac#update()
command! PackClean call minpac#clean()

let mapleader = ' '

nnoremap <leader>g :Gstatus<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>s :source %<CR>
