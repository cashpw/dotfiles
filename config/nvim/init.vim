"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/cashweaver/.cashe/dein/repos/github.com/Shougo/dein.vim
let pluginconfigpath = "/home/cashweaver/.config/nvim/plugin-config"

" Required:
if dein#load_state('/home/cashweaver/.cashe/dein')
  call dein#begin('/home/cashweaver/.cashe/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/home/cashweaver/.cashe/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here like this:
  "call dein#add('Shougo/neosnippet.vim')
  "call dein#add('Shougo/neosnippet-snippets')

  " Colorschemes
  source $HOME/.config/nvim/

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------
