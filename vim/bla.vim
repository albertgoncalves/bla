" $ cp bla.vim ~/.vim/syntax/bla.vim
" $ grep '.bla' ~/.vimrc
" autocmd BufNewFile,BufRead *.bla setlocal filetype=bla

if exists("b:current_syntax")
    finish
endif

syn match Comment   "#.*$"
syn match Operator  "[(){}[\]:;=,+\-*/]"
syn match Number    "\<[0-9]\+\>"
syn match Function  "[A-Za-z][A-Za-z0-9_]*("me=e-1

syn keyword Conditional
    \ if
syn keyword Repeat
    \ loop
syn keyword Statement
    \ break
    \ continue
    \ return
syn keyword Type
    \ i32

let b:current_syntax = "bla"
