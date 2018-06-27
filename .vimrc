set nocompatible

" Settings
" --

filetype plugin indent on
syntax on
set noshowmode
set number
set hidden
set clipboard=unnamed
set backspace=indent,eol,start
set tabstop=2
set shiftwidth=2
set expandtab
set wildmenu
set signcolumn=yes
set hlsearch
set ignorecase
set incsearch
set nobackup
set noswapfile
set laststatus=2

" Plugins
" --

call plug#begin()

" Color themes
Plug 'chriskempson/base16-vim'
""Plug 'lifepillar/vim-solarized8'

" Language completion, linting, etc
Plug 'w0rp/ale'

" Visual file manager
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" File navigation in project
Plug 'ctrlpvim/ctrlp.vim'

" Git

" - git gutter is broken is newer versions :/
Plug 'airblade/vim-gitgutter', { 'commit': '932ffac' }
Plug 'tpope/vim-fugitive'

" Syntax highlighting
Plug 'leafgarland/typescript-vim'

" Status bar
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-ale'

" tpope utils
Plug 'tpope/vim-unimpaired'

call plug#end()

color base16-onedark

" Investigate
"Plug 'tpope/vim-surround'
"Plug 'tpope/vim-commentary'
"Plug 'mattn/emmet-vim'


" Plugin Settings
" --

" Ale
" --
let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
  \ 'typescript': ['prettier']
  \ }

nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

augroup ale_mappings
  autocmd!
  autocmd FileType typescript nnoremap <leader>h :ALEHover<cr>
  autocmd FileType typescript nnoremap <S-f12> :ALEFindReferences<cr>
  autocmd FileType typescript nnoremap <f12> :ALEGoToDefinition<cr>
augroup END

" CTRL P
let g:ctrlp_max_height = 30
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
let g:ctrlp_custom_ignore = '\v[\/](.*\.egg-info|venv|emacs|sublime|node_modules|__pycache__|target|dist)|(\.(swp|ico|git|svn))$'
let g:ctrlp_extensions = ['projects']

" Lightline
function! LightLineGitHunks()
  let l:hunks = GitGutterGetHunkSummary()

  let l:added = l:hunks[0]
  let l:modified = l:hunks[1]
  let l:removed = l:hunks[2]

  if l:added == 0 && l:modified == 0 && l:removed == 0
    return ''
  endif

  let l:displays = []

  if l:added > 0
    let l:displays = l:displays + ['+'.l:added]
  endif

  if l:modified > 0
    let l:displays = l:displays + ['~'.l:modified]
  endif

  if l:removed > 0
    let l:displays = l:displays + ['-'.l:removed]
  endif

  return join(l:displays, ' ')
endfunction

let g:lightline = {
  \ 'colorscheme': 'one',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'githunks', 'readonly', 'filename', 'modified'] ],
  \   'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ]]
  \ },
  \ 'component_expand': {
  \   'linter_checking': 'lightline#ale#checking',
  \   'linter_warnings': 'lightline#ale#warnings',
  \   'linter_errors': 'lightline#ale#errors',
  \   'linter_ok': 'lightline#ale#ok'
  \ },
  \ 'component_type': {
  \   'linter_checking': 'left',
  \   'linter_warnings': 'warning',
  \   'linter_errors': 'error',
  \   'linter_ok': 'left'
  \ },
  \ 'component_function': {
  \   'gitbranch': 'fugitive#head',
  \   'githunks': 'LightLineGitHunks'
  \ }
  \ }

" Customizations for Color Schemes
" --

function! s:customize_colorscheme()
  if g:colors_name == 'PaperColor'
    hi! StatusLine guifg='#444444'
  endif
endfunction

augroup customize_colorscheme_group
  autocmd!
  autocmd ColorScheme * call <SID>customize_colorscheme()
  "autocmd ColorScheme * call lightline#update()
augroup END

call <SID>customize_colorscheme()

" Mappings
" --
let mapleader = ' '
nnoremap <leader>x :source %<cr>
nnoremap <leader>s :w!<cr>
nnoremap <leader>b :ls<cr>
nnoremap <leader>n :NERDTreeToggle<cr>
nnoremap <leader>f :CtrlP<cr>
nnoremap <leader>b :CtrlPBuffer<cr>
nnoremap <leader>c :CtrlPProjects<cr>
nnoremap <leader>p :PlugInstall<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader><leader> :set nohlsearch<cr>
inoremap jj <esc>
nmap <leader>t :call <SID>SynStack()<CR>

function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

