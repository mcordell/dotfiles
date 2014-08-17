set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'Valloric/YouCompleteMe'
Plugin 'docunext/closetag.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'wincent/Command-T'
Plugin 'Raimondi/delimitMate'
Plugin 'mattn/emmet-vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'tobyS/pdv'
Plugin 'malkomalko/projections.vim'
Plugin 'kevinw/pyflakes-vim'
Plugin 'ervandew/supertab'
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'marijnh/tern_for_vim'
Plugin 'tpope/vim-bundler'
Plugin 'kchmck/vim-coffee-script'
Plugin 'altercation/vim-colors-solarized'
Plugin 'heartsentwined/vim-emblem'
Plugin 'tpope/vim-fugitive'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'amiorin/vim-project'
Plugin 'tpope/vim-rails'
Plugin 'vim-ruby/vim-ruby'
Plugin 'ecomba/vim-ruby-refactoring'
Plugin 'kshenoy/vim-signature'
Plugin 'skwp/vim-spec-finder'
Plugin 'tpope/vim-surround'
Plugin 'tobyS/vmustache'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'tpope/vim-unimpaired'
Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


" :A on lib/foo.rb -> unit/lib/foo_spec.rb
autocmd User Rails/lib/* let b:rails_alternate = 'spec/lib/' . rails#buffer().name()[0:-4] . '_spec.rb'

" :A on unit/lib/foo_spec.rb -> lib/foo.rb
autocmd User Rails/spec/lib/* let b:rails_alternate = rails#buffer().name()[5:-9] . '.rb'

"Basic
syntax enable 		"syntax highlighting on

"Next three turns on file type
filetype on
filetype plugin on
filetype indent on
let @e = 'A;:w'

" Strip whitespace on save
autocmd BufWritePre * :%s/\s\+$//e
"show line numbers
set number

"Typo fixes
:command WQ wq
:command Wq wq
:command W w
:command Q q
:command Vimrc tabe ~/.vimrc
:command Source source ~/.vimrc

"Change highlighting to underline
highlight clear SpellBad
highlight SpellBad cterm=underline

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
"Python
autocmd FileType python set tabstop=4 | set shiftwidth=4 | set expandtab | set smarttab | set softtabstop=4
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype php setlocal ts=4 sts=4 sw=4 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
"autocmd Filetype coffee setlocal ts=2 sts=2 sw=2 expandtab


"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
" Remove trailing whitespaces and ^M chars
"autocmd FileType c,cpp,java,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))
"autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig"

"Html
autocmd FileType html,htmldjango,jinjahtml,eruby,mako let b:closetag_html_style=1
autocmd FileType html,xhtml,xml,htmldjango,jinjahtml,eruby,mako source ~/.vim/bundle/closetag/plugin/closetag.vim

"Key Maps

"plugin specific
"map <C-t> :CommandT <CR>
noremap <space>t :CommandT <CR>
noremap <space>T :NERDTreeToggle <CR>
noremap <C-t> :NERDTreeToggle <CR>
noremap <space>gs :Gstatus<CR>
noremap <space>gc :Gcommit<CR>
noremap <space>gd :Gdiff<CR>
noremap <space>s  :Ag<SPACE>
noremap <space>j  :!php codecept.phar run<CR>
noremap <space>i  :s/\.\([a-z\-]*\)/@include \1/g<CR>
noremap <space>m  :s/\.\([a-z\-]*\)/@mixin \1/g<CR>
noremap <space>v  :s/@/$/g<CR>


"movement between buffers
map <space>p :tabprev <CR>
map <space>n :tabnext <CR>
map <space>l :Git! log<CR>gg
nnoremap <C-F> yiw <ESC>:Git commit --fixup=<C-r>"<CR>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Color scheme
let g:solarized_termtrans = 1
colorscheme solarized

"Syntastic specific
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_php_checkers =  ['php', 'phpcs', 'phpmd']
let g:syntastic_aggregate_errors = 1
let g:syntastic_php_phpcs_args="-n -s"

"quick function for adding character at end of line
imap <silent><F2> <Esc>v`^me<Esc>gi<C-o>:call Ender()<CR>
function Ender()
  let endchar = nr2char(getchar())
  execute "normal \<End>a".endchar
  normal `e
endfunction

let NERDTreeBookmarksFile=expand("$HOME/.vim-NERDTreeBookmarks")
let NERDTreeShowBookmarks=1

let g:pdv_template_dir = $HOME ."/.vim/bundle/pdv/templates_snip"
nnoremap <space>d :call pdv#DocumentCurrentLine()<CR>

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

"Tagbar
"
noremap <space>c :TagbarToggle<CR>

nnoremap K :Ag -i <C-R><C-W><CR>

" bind \ (backward slash) to grep shortcut
command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!

"window opening
set splitright

inoremap <Up> <ESC><C-W><C-K>i
inoremap <Down> <ESC><C-W><C-J>i
inoremap <Left> <Esc>:tabprev<CR>
inoremap <Right> <Esc>:tabnext<CR>

nnoremap <Up> <ESC><C-W><C-K>i
nnoremap <Down> <ESC><C-W><C-J>i
nnoremap <Left> :tabprev<CR>
nnoremap <Right> :tabnext<CR>

call project#rc()
Project '~/monkdev/mcms-vagrant/mcms', 'mcms'
Callback 'mcms' , ['AddMcmsPaths']

Project '~/monkdev/carpenter', 'carpenter'
Project '~/rails-projects/winestat', 'vineweather'
Project '~/Dropbox/Ruby/noaa', 'noaa'
Project '~/monkdev/mchk', 'mchks'
Project '~/Dropbox/Websites/mikecordell', 'website'

function! AddMcmsPaths(...) abort
  setlocal path+=Library
  setlocal path+=model
  setlocal suffixesadd=.php
endfunction

let g:project_use_nerdtree = 0

call project#config#callback("noaa", project#utils#alternate(
  \  [{'regex': '^lib', 'string': 'spec/lib', 'suffix': '+_spec'},
  \   {'regex': '^spec/lib', 'string': 'lib', 'suffix': '-_spec'}]
  \  ))
"call project#config#callback("mcms", project#utils#alternate(
"  \  [{'regex': '^Module\/\([a-zA-z]*\)\/Page\/Capture\/\([a-zA-z]*\).content.php', 'string': 'Module/\1/Process/Capture/\2.php'},
"  \   {'regex': '^Module\/\([a-zA-z]*\)\/Process\/Capture\/\([a-zA-z]*\).php', 'string': 'Module/\1/Page/Capture/\2.content.php'}]
"  \  ))


" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:snips_author = "Michael Cordell <michael@monkdevelopment.com>"


set guifont=Inconsolata\ for\ Powerline:h15
let g:Powerline_symbols = 'fancy'
set encoding=utf-8
set t_Co=256
set fillchars+=stl:\ ,stlnc:\
set term=xterm-256color
set termencoding=utf-8
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
