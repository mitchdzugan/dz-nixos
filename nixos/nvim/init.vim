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

colorscheme dracula

syntax on
if has('termguicolors')
    set termguicolors
endif

set clipboard+=unnamedplus
set noshowcmd
hi MatchParen gui=underline
