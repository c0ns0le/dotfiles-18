set nocompatible

" Plugins
call plug#begin()
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/async.vim'
Plug 'ryanolsonx/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'leafgarland/typescript-vim'
Plug 'lifepillar/vim-gruvbox8'
Plug 'tpope/vim-fugitive'
call plug#end()

" Plugin Settings
" --

"  LSP
let g:lsp_log_verbose = 1
let g:lsp_log_file = expand('~/vim-lsp.log')
let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
let g:asyncomplete_log_file = expand('~/asyncomplete.log')

imap <c-space> <Plug>(asyncomplete_force_refresh)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" TODO: make github repo called vim-lsp-python
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

" TODO: make github repo called vim-lsp-typescript (or vim-lsp-typescript-javascript)
if executable('typescript-language-server')
    " npm install -g typescript typescript-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'typescript-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'typescript-language-server', '--stdio']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'tsconfig.json'))},
        \ 'whitelist': ['typescript'],
        \ })
endif

" Settings
" --
set number
set hidden
syntax on
filetype plugin indent on
set clipboard=unnamed
set backspace=indent,eol,start
set tabstop=2
set shiftwidth=2
set expandtab
set wildmenu
set signcolumn=yes
set hlsearch
set incsearch
set nobackup
set noswapfile

" Color theme
" --
set background=dark
colo gruvbox8

let mapleader = ' '
nnoremap <leader>x :source %<cr>
nnoremap <leader>s :w<cr>
nnoremap <leader>b :ls<cr>
nnoremap <leader>p :PlugInstall<cr>
nnoremap <leader><leader> :set nohlsearch<cr>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
"vnoremap <leader>2 "xy:echo @x<cr>
inoremap jj <esc>
command! ProjectSearchVim -nargs=1 vimgrep /<args>/gj ./**/*.vim
command! ProjectSearchTs -nargs=1 vimgrep /<args>/gj ./**/*.ts
