"Set up Pathogen for plugin management
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" :A on lib/foo.rb -> unit/lib/foo_spec.rb
autocmd User Rails/lib/* let b:rails_alternate = 'spec/lib/' . rails#buffer().name()[0:-4] . '_spec.rb'

" :A on unit/lib/foo_spec.rb -> lib/foo.rb
autocmd User Rails/spec/lib/* let b:rails_alternate = rails#buffer().name()[5:-9] . '.rb'

"Basic
syntax enable 		"syntax highlighting on
set nocompatible 	"Don't try to be compatible with vi

"Next three turns on file type
filetype on
filetype plugin on
filetype indent on
let @e = 'A;:w'
let @f = 'A {O'
let @s = 'viw:s/\(\%V\u\l*\)/ €kb-\L\!€kb1/g'

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
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2 expandtab

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
let g:syntastic_php_checkers =  ['phpcs']
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
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  "let g:ctrlp_use_caching = 0
endif

"Tagbar
"
noremap <space>c :TagbarToggle<CR>

nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

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
Project '~/Dropbox/Ruby/noaa', 'noaa'
Project '~/monkdev/mcms-vagrant/mcms', 'mcms'
Project '~/monkdev/mchk', 'mchks'
Project '~/Dropbox/Websites/mikecordell', 'website'
Project '~/rails-projects/crm', 'crm'
let g:project_use_nerdtree = 0

call project#config#callback("noaa", project#utils#alternate(
  \  [{'regex': '^lib', 'string': 'spec/lib', 'suffix': '+_spec'},
  \   {'regex': '^spec/lib', 'string': 'lib', 'suffix': '-_spec'}]
  \  ))
