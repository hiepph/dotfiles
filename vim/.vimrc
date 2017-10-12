""
" Plugins mananger
" (Plug) https://github.com/junegunn/vim-plug
"
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"
call plug#begin('~/.vim/plugged')

""" Helpers
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'https://github.com/ntpeters/vim-better-whitespace'
Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/airblade/vim-gitgutter'
Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/haya14busa/incsearch.vim'
Plug 'https://github.com/ctrlpvim/ctrlp.vim'
Plug 'https://github.com/wellle/targets.vim'
Plug 'https://github.com/Yggdroot/indentLine'

""" Structures
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'https://github.com/scrooloose/nerdtree'

" 1 nerd tree for all tabs
Plug 'https://github.com/jistr/vim-nerdtree-tabs'

""" Languages
Plug 'https://github.com/fatih/vim-go'
Plug 'https://github.com/rust-lang/rust.vim'
Plug 'https://github.com/IN3D/vim-raml'
Plug 'https://github.com/derekwyatt/vim-scala'
Plug 'https://github.com/elixir-editors/vim-elixir'
Plug 'octol/vim-cpp-enhanced-highlight'

""" Theme
Plug 'https://github.com/dracula/vim'
Plug 'https://github.com/joshdick/onedark.vim'
Plug 'https://github.com/kristijanhusak/vim-hybrid-material'
Plug 'dracula/vim'
Plug 'https://github.com/morhetz/gruvbox'

call plug#end()

""
" Basic Configuration
"

" show number
set number relativenumber
nnoremap <F2> :set nonumber! norelativenumber!<CR>

" highlight current line
set cursorline

" max line-length mark
" set colorcolumn=100

" show command in NORMAL mode
set showcmd

" 4 soft tabs
set smarttab
set expandtab
set tabstop=4 softtabstop=0 shiftwidth=4

" Indent helpers
filetype plugin indent on
set smarttab
set smartindent

" Stop creating .swp files
set nobackup
set noswapfile

" Set to auto read when a file is changed from the outside
set autoread

" Turn on wildmenu
set wildmenu

" Searching
set incsearch
set hlsearch
set smartcase

" For regular expressions turn magic on
set magic

""
" Custom file type config
"
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype eruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype lisp setlocal ts=2 sts=2 sw=2 expandtab
au Filetype lisp let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"', '`':'`'}
au Filetype rust let b:AutoPairs={'(':')', '[':']', '{':'}','"':'"', '`':'`'}


""
" Theme
"
try
source ~/.theme.vim
catch
endtry


""
" Plugins configuration
"

" NERD Tree
" toggle
map <C-\> :NERDTreeTabsToggle<CR>

" open NERD tree auto when vim starts up on opening a directory
let g:nerdtree_tabs_open_on_console_startup = 2
let g:nerdtree_tabs_autofind = 1

" close vim if only window left open is NERD
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeTabs") && b:NERDTree.isTabTree()) | q | endif

" Display hidden files
let NERDTreeShowHidden=1

" Lightline
" Needs font-awesome
set laststatus=2
let g:lightline = {
      \ 'mode_map': { 'c': 'NORMAL' },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
      \ },
      \ 'component_function': {
      \   'modified': 'LightlineModified',
      \   'readonly': 'LightlineReadonly',
      \   'fugitive': 'LightlineFugitive',
      \   'filename': 'LightlineFilename',
      \   'fileformat': 'LightlineFileformat',
      \   'filetype': 'LightlineFiletype',
      \   'fileencoding': 'LightlineFileencoding',
      \   'mode': 'LightlineMode',
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' }
      \ }

function! LightlineModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? '' : ''
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft == 'unite' ? unite#get_status_string() :
        \  &ft == 'vimshell' ? vimshell#get_status_string() :
        \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists("*fugitive#head")
    let branch = fugitive#head()
    return branch !=# '' ? ' '.branch : ''
  endif
  return ''
endfunction

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

" Vim Better Whitespace
autocmd BufEnter * EnableStripWhitespaceOnSave

" Go
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_generate_tags = 0
let g:go_template_autocreate = 0

" Incseach
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" :h g:incsearch#auto_nohlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

" Cpp enhancement
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1
let g:cpp_experimental_template_highlight = 1
let g:cpp_concepts_highlight = 1
let c_no_curly_error=1

" Indent line toggle
nnoremap <F3> :IndentLinesToggle<CR>
