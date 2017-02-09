if &shell =~# 'fish$'
    set shell=sh
endif
" Here are some basic customizations, please refer to the top of the vimrc
" file for all possible options:
let g:spacevim_default_indent = 2
let g:spacevim_max_column     = 120
let g:spacevim_plugin_manager = 'dein'  " neobundle or dein or vim-plug

" If there is a particular plugin you don't like, you can define this
" variable to disable them entirely:
let g:spacevim_disabled_plugins=['fcitx.vim']
" If you want to add some custom plugins, use these options:
let g:spacevim_custom_plugins = [
 \ ['dag/vim-fish', {'on_ft' : 'fish'}],
 \ ]
