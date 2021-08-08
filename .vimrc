set nocompatible              " be iMproved, required
filetype off                  " required

let g:pymode_python = 'python3'
"hide buffers with unwritten changes
set hidden

call plug#begin()

"Essential
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'

"Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

"Snippets
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'

"Helpful
Plug 'terryma/vim-multiple-cursors'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
"Plug 'docunext/closetag.vim'

Plug 'junegunn/vim-easy-align'
"Styling
Plug 'morhetz/gruvbox'

"Ruby
"Plug 'jgdavey/vim-blockle', { 'for': 'ruby'}
"Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
"Plug 'ecomba/vim-ruby-refactoring', { 'for': 'ruby' }
"Plug 'skwp/vim-spec-finder', { 'for': 'ruby' }
"Plug 'thoughtbot/vim-rspec', { 'for': 'ruby' }
"Plug 'tpope/vim-rails'

"Assorted Languages
"Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
"Plug 'kevinw/pyflakes-vim', { 'for': 'python' }
"Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
"Plug 'mattn/emmet-vim', { 'for': ['html', 'php'] }
"Plug 'tobyS/vmustache', { 'for': 'html' }

" All of your Plugs must be added before the following line
call plug#end() " required

let g:UltiSnipsSnippetDirectories = ['~/.config/nvim/UltiSnips']

"Basic
syntax enable 		"syntax highlighting on
filetype plugin indent on    " required

" Strip whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" Show line numbers
set number

"Config helpers
:command Vimrc tabe ~/.vimrc
:command Source source ~/.vimrc

"Change highlighting to underline
highlight clear SpellBad
highlight SpellBad cterm=underline

"formatting
set wrap "wrap long lines
set tw=80
set autoindent
set shiftwidth=4
set tabstop=4
set pastetoggle=<F8> "Turn off auto indent for a paste
"Turn Mouse on
set mouse=a

"formatting
set wrap "wrap long lines
set tw=80
set autoindent
set shiftwidth=4
set tabstop=4
set pastetoggle=<F8> "Turn off auto indent for a paste

"Langauge specific formatting
autocmd FileType python set tabstop=4 | set shiftwidth=4 | set expandtab | set smarttab | set softtabstop=4
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype php setlocal ts=4 sts=4 sw=4 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype gitcommit setlocal spell textwidth=72
autocmd Filetype markdown setlocal spell textwidth=0

"Key Maps

nnoremap <C-F> yiw <ESC>:Git commit --fixup=<C-r>"<CR>
noremap <space>gs :Gstatus<CR>
noremap <space>gc :Gcommit<CR>
noremap <space>gd :Gdiff<CR>
nmap <space>hs <Plug>GitGutterStageHunk
nmap <space>hr <Plug>GitGutterRevertHunk

"movement between buffers
map <space>p :tabprev <CR>
map <space>w :w<CR>
map <space>l :Git! log<CR>gg
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

vmap <space>y  "+y
nmap <space>p  "+p

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

"Display stuff
let g:gruvbox_italic=0
set background=dark
colorscheme gruvbox
"MacVim
set guifont=InconsolataForPowerline:h14

"Close that scratch buffer window that opens on autocompleting
autocmd CompleteDone * pclose

set backspace=indent,eol,start
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
