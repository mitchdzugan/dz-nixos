set nu rnu
set nocompatible
if has('filetype')
  filetype indent plugin on
endif
if has('syntax')
  syntax on
endif
set showtabline=2
set hlsearch
set number
set cursorline
set hidden
set visualbell
set t_vb=
if has('mouse')
  set mouse=a
endif

set expandtab
set tabstop=2
set shiftwidth=2

colorscheme rose-pine-main

syntax on
if has('termguicolors')
    set termguicolors
endif

set clipboard+=unnamedplus
set noshowcmd
hi MatchParen gui=underline

set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
  \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
  \,sm:block-blinkwait175-blinkoff150-blinkon175