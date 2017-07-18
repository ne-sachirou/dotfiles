scriptencoding utf-8
set encoding=utf-8
augroup vimrc
  autocmd!
augroup END
set ambiwidth=double
set autoindent smartindent
set backupdir=~/.vim/tmp,. directory=~/.vim/tmp,. undodir=~/.vim/tmp,.
set clipboard=unnamed
"set cryptmethod=blowfish2
set display+=lastline
set expandtab shiftwidth=2 tabstop=2 softtabstop=2
set fileformats=unix,dos
set foldmethod=syntax foldlevel=1 foldnestmax=3
set hlsearch ignorecase smartcase
set laststatus=2
set listchars=tab:>-,trail:-,extends:<,precedes:>,nbsp:_
set number
set showtabline=1
set statusline=%-(%f%m%h%q%r%w%)%=%{&ff}\|%{&fenc}\ %y%l,%c\ %0P
set undofile
set wrap

autocmd vimrc BufWritePost *vimrc source $MYVIMRC
autocmd vimrc BufWritePost init.vim source $MYVIMRC
autocmd vimrc BufWritePost *gvimrc if has('gui_running') source $MYGVIMRC

" http://kannokanno.hatenablog.com/entry/2014/03/16/160109
nnoremap <Leader>f :call <SID>format_file()<Cr>
vnoremap <Leader>f =
function! s:format_file()
  let view = winsaveview()
  normal gg=G
  call winrestview(view)
endfunction

" http://c4se.hatenablog.com/entry/2013/03/10/032917
autocmd vimrc BufWritePre * call s:strip_trailing_whitespace()
function! s:strip_trailing_whitespace()
  let view = winsaveview()
  " if &ft =~ 'python\|haskell\|fsharp\|coffee\|haml'
  "   %s/\S\zs\s\+$//e
  " elseif &ft =~ 'markdown'
  if &ft =~ 'markdown'
    %s/^\s\+$//e
  else
    %s/\s\+$//e
  endif
  call winrestview(view)
endfunction

if executable('ag')
  set grepprg=ag\ --vimgrep\ -iS
  set grepformat=%f:%l:%c:%m
else
  set grepprg=grep\ -Hnd\ skip\ -r
  set grepformat=%f:%l:%m,%f:%l%m,%f\ \ %l%m
endif
" 自動的にquickfix-windowを開く http://qiita.com/yuku_t/items/0c1aff03949cb1b8fe6b
autocmd QuickFixCmdPost *grep* cwindow

autocmd vimrc BufNewFile,BufReadPost Guardfile,*.rabl,*.jbuilder setl ft=ruby
autocmd vimrc BufNewFile,BufReadPost *.ect,*.eex setl ft=html
autocmd vimrc BufNewFile,BufReadPost .envrc setl ft=sh
autocmd vimrc BufNewFile,BufReadPost Makefile setl noet
autocmd vimrc BufNewFile,BufReadPost *.xrl,*.yrl setl ft=erlang

if has('vim_starting')
  set runtimepath+=~/.vim
endif
call plug#begin('~/.vim/plugged')
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'kchmck/vim-coffee-script', {'for': 'coffee'}
Plug 'mgrabovsky/vim-cuesheet', {'for': 'cue'}
Plug 'elixir-lang/vim-elixir', {'for': 'elixir'}
Plug 'kongo2002/fsharp-vim', {'for': 'fsharp'}
Plug 'groenewege/vim-less', {'for': 'less'}
Plug 'tpope/vim-liquid', {'for': 'liquid'}
Plug 'vim-perl/vim-perl', {'for': 'perl'}
Plug 'digitaltoad/vim-pug', {'for': 'pug'}
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'slim-template/vim-slim', {'for': 'slim'}
Plug 'wavded/vim-stylus', {'for': 'stylus'}
Plug 'evidens/vim-twig', {'for': 'twig'}
Plug 'rhysd/vim-crystal', {'for': 'crystal'}
Plug 'smerrill/vcl-vim-plugin', {'for': 'vcl'}
Plug 'mattn/emmet-vim'
Plug 'tomasr/molokai'
Plug 'LeafCage/yankround.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-fugitive'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-operator-user'
Plug 'kana/vim-textobj-indent'
Plug 'thinca/vim-quickrun'
Plug 'dannyob/quickfixstatus'
Plug 'jceb/vim-hier'
Plug 'osyo-manga/shabadou.vim'
Plug 'osyo-manga/vim-watchdogs'
Plug 'LeafCage/qutefinger.vim'
Plug 'vim-scripts/Align', {'on': 'Align'}
Plug 'editorconfig/editorconfig-vim'
Plug 'vim-scripts/camelcasemotion'
call plug#end()

nmap p     <Plug>(yankround-p)
xmap p     <Plug>(yankround-p)
nmap P     <Plug>(yankround-P)
nmap gp    <Plug>(yankround-gp)
xmap gp    <Plug>(yankround-gp)
nmap gP    <Plug>(yankround-gP)
nmap <C-p> <Plug>(yankround-prev)
nmap <C-n> <Plug>(yankround-next)

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_color_change_percent = 3
let g:indent_guides_start_level = 2
let g:indent_guides_default_mapping = 0

if !exists('g:quickrun_config')
  let g:quickrun_config = {
        \   '_':  {
        \     'runner': 'vimproc',
        \     'runner/vimproc/updatetime': 60
        \  }
        \ }
endif
let g:quickrun_config.fsharp = {
      \   'command': 'fsharpc',
      \   'runmode': 'simple',
      \   'exec': [
      \     '%c /nologo --out:"%S:p:r:gs?/?\?.exe" %s:gs?/?\?',
      \     '"%S:p:r:gs?/?\?.exe" %a',
      \     ':call delete("%S:p:r.exe")'
      \    ],
      \   'tempfile': '%{tempname()}.fs',
      \ }
let g:quickrun_config.ps1 = {
      \   'command' : 'powershell.exe',
      \   'cmdopt'  : '-executionPolicy RemoteSigned',
      \   'exec'    : '%c %o -F %s:p',
      \   'tempfile': '%{tempname()}.ps1'
      \ }
let g:watchdogs_check_BufWritePost_enable = 1

if !exists('g:lightline')
  let g:lightline = {}
  let g:lightline.component = {}
  let g:lightline.component_function = {}
endif
let g:lightline = {
      \   'colorscheme': 'wombat',
      \   'active': {
      \     'left': [
      \       ['mode', 'paste', 'git'],
      \       ['readonly', 'filename', 'modified']
      \     ],
      \     'right': [
      \       ['lineinfo', 'char_counter'],
      \       ['percent'],
      \       ['fileformat', 'fileencoding', 'filetype']
      \     ]
      \   },
      \   'inactive': {
      \     'left': [
      \       ['filename']
      \     ],
      \     'right': [
      \       ['char_counter'],
      \       ['fileformat', 'fileencoding', 'filetype']
      \     ]
      \   },
      \   'component': {
      \     'char_counter': '%{exists("b:char_counter_count") ? b:char_counter_count : ""}'
      \   },
      \   'component_function': {
      \     'git': 'Lightline_git'
      \   }
      \ }
augroup char_counter
  autocmd!
  autocmd BufCreate,BufEnter * call s:char_counter_initialize()
  autocmd BufNew,BufEnter,BufWrite,InsertLeave * call s:char_counter_update()
augroup END
function! s:char_counter_initialize()
  if !exists('b:char_counter_count')
    let b:char_counter_count = 0
  endif
endfunction
function! s:char_counter_update()
  let b:char_counter_count = s:char_counter()
endfunction
function! s:char_counter()
  let result = 0
  for linenum in range(0, line('$'))
    let line = getline(linenum)
    let result += strlen(substitute(line, '.', 'x', 'g'))
  endfor
  return result
endfunction
call s:char_counter_initialize()
function! Lightline_git()
  try
    if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
      return fugitive#head()
    endif
  catch
  endtry
  return ''
endfunction
set stl+=\ %{fugitive#statusline()}

autocmd vimrc VimEnter * colorscheme molokai

" vim:et sw=2 sts=2 ts=2 tw=0:
