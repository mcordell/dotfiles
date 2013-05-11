"Basic
syntax on 		"syntax highlighting on
set nocompatible 	"Don't try to be compatible with vi
filetype on		"Next three turns on file type
filetype plugin on
filetype indent on 

"Typo fixes
:command WQ wq
:command Wq wq
:command W w
:command Q q

"Change highlighting to underline
highlight clear SpellBad
highlight SpellBad cterm=underline


"Set up Pathogen for plugin management
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

"formatting
set wrap "wrap long lines
set tw=80
set autoindent
set shiftwidth=4
set tabstop=4
set pastetoggle=<F10> "Turn off auto indent for a paste
"Langauge specific formatting
"Python
autocmd FileType python set tabstop=4 | set shiftwidth=4 | set expandtab | set smarttab | set softtabstop=4 
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2 expandtab


"set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
" Remove trailing whitespaces and ^M chars
"autocmd FileType c,cpp,java,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))
"autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig"

"Html
autocmd FileType html,htmldjango,jinjahtml,eruby,mako let b:closetag_html_style=1
autocmd FileType html,xhtml,xml,htmldjango,jinjahtml,eruby,mako source ~/.vim/bundle/closetag/plugin/closetag.vim

"Key Maps
map <F2> :tabprev <CR>
map <F3> :CommandT <CR>

