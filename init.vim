set nocompatible

" Plugins
call plug#begin()

" Navigation / File Exploration
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ryanolsonx/ctrlp-projects.vim'

" LSP
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/async.vim'
Plug 'ryanolsonx/vim-lsp'
Plug 'ryanolsonx/vim-lsp-typescript'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

" Color schemes
Plug 'lifepillar/vim-gruvbox8'
Plug 'lifepillar/vim-solarized8'
Plug 'arcticicestudio/nord-vim'
Plug 'NLKNguyen/papercolor-theme'

" Status Bar
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'itchyny/lightline.vim'

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter', { 'commit': '932ffac' }

" Utils
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'

" Programming
"Plug 'SirVer/ultisnips'
"Plug 'honza/vim-snippets' 
Plug 'mattn/emmet-vim'
Plug 'sbdchd/neoformat'
"Plug 'editorconfig/editorconfig-vim'

" Languages
Plug 'leafgarland/typescript-vim'

call plug#end()

" Plugin Settings
" --

"  LSP
"let g:lsp_log_verbose = 1
"let g:lsp_log_file = expand('~/vim-lsp.log')
"let g:asyncomplete_log_file = expand('~/asyncomplete.log')
let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
let g:asynccomplete_auto_popup = 1
set completeopt+=preview

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

" Airline
let g:airline_skip_empty_sections = 1
let g:airline_section_y = ''
let g:airline_powerline_fonts = 1
let airline#extensions#whitespace#enabled = 0

" Lightline
let g:lightline = {
  \ 'colorscheme': 'PaperColor'
  \ }

" CTRL P
let g:ctrlp_max_height = 30
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v[\/](.*\.egg-info|venv|emacs|sublime|node_modules|__pycache__|target|dist)|(\.(swp|ico|git|svn))$'
let g:ctrlp_extensions = ['projects']

" Settings
" --
set number
set hidden
syntax on
filetype plugin indent on
"set clipboard=unnamed
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
set laststatus=2

" Color theme
" --
set background=light
colo PaperColor

function! s:customize_colorscheme()
  if g:colors_name == 'PaperColor'
    hi! StatusLine guifg='#444444'
  endif
endfunction

augroup customize_colorscheme_group
  autocmd!
  autocmd ColorScheme * call <SID>customize_colorscheme()
  autocmd ColorScheme * call lightline#update()
augroup END

call <SID>customize_colorscheme()

" Mappings
" --
let mapleader = ' '
nnoremap <leader>x :source %<cr>
nnoremap <leader>s :w<cr>
nnoremap <leader>b :ls<cr>
nnoremap <leader>p :Neoformat<cr>
nnoremap <leader>n :NERDTreeToggle<cr>
nnoremap <leader>f :CtrlP<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>c :CtrlPProjects<cr>
nnoremap <leader>gg :Gstatus<cr>
nnoremap <leader>gs :Gwrite<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader><leader> :set nohlsearch<cr>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
"vnoremap <leader>2 "xy:echo @x<cr>
inoremap jj <esc>

" Language Mappings
" --

augroup language_mappings
  autocmd!

  " TypeScript
  autocmd FileType typescript nnoremap <leader>h :LspHover<cr>
  autocmd FileType typescript nnoremap <f2> :LspRename<cr>
  autocmd FileType typescript nnoremap <f8> :LspDocumentDiagnostics<cr>
  autocmd FileType typescript nnoremap <f10> :LspDocumentSymbol<cr>
  autocmd FileType typescript nnoremap <f11> :LspReferences<cr>
  autocmd FileType typescript nnoremap <f12> :LspDefinition<cr>
  autocmd FileType typescript command! ProjectSearch -nargs=1 vimgrep /<args>/gj ./**/*.ts<cr>

  " Vim
  autocmd FileType vim command! ProjectSearch -nargs=1 vimgrep /<args>/gj ./**/*.vim<cr>
augroup END
