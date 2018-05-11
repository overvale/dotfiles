hi clear
syntax reset
let g:colors_name = "scales"

" Made by Oliver Taylor <olivertaylor.net>.
" This is a TERMINAL ONLY color-scheme.
" It assumes a specially designed terminal theme.
" 00 = Base 0 (Black)
" 09 = Base 1
" 10 = Base 2
" 08 = Base 3
" 11 = Base 4
" 12 = Base 5
" 07 = Base 6
" 13 = Base 7
" 14 = Base 8
" 15 = Base 9 (White)
" 01 = Red
" 02 = Green
" 04 = Blue
" 03 = Highlight 1
" 05 = Highlight 2
" 06 = Highlight 3

" }}}
" {{{ Reset Highlight Groups
" *********************************************************

" These are the default syntax highlighting groups.
" If you don't reset them here they will inherit default values
" Even after 'hilight clear'

hi!  Comment       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Constant      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Special       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Identifier    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Statement     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  PreProc       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Type          term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Underlined    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Ignore        term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Error         term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Todo          term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  NonText       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Directory     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  ErrorMsg      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  IncSearch     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Search        term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  MoreMsg       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  ModeMsg       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  LineNr        term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  CursorLineNr  term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Question      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  StatusLine    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  StatusLineNC  term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  VertSplit     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Title         term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Visual        term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  VisualNOS     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  WarningMsg    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  WildMenu      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Folded        term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  FoldColumn    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  DiffAdd       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  DiffChange    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  DiffDelete    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  DiffText      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  SignColumn    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Conceal       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  SpellBad      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  SpellCap      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  SpellRare     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  SpellLocal    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  Pmenu         term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  PmenuSel      term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  PmenuSbar     term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  PmenuThumb    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  TabLine       term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  TabLineSel    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  TabLineFill   term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  CursorColumn  term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  CursorLine    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  ColorColumn   term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  MatchParen    term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
hi!  rstEmphasis   term=NONE  cterm=NONE  ctermfg=NONE  ctermbg=NONE  gui=NONE  guifg=NONE  guibg=NONE
 
" }}}
" {{{ Interface Colors
" *********************************************************

hi! Normal         ctermfg=00     ctermbg=15
hi! Visual         ctermfg=00     ctermbg=14
hi! NonText        ctermfg=09
hi! StatusLine     ctermfg=15     ctermbg=06    cterm=bold
hi! StatusLineNC   ctermfg=15     ctermbg=13
hi! LineNr         ctermfg=13     ctermbg=15
hi! CursorLineNr   ctermfg=09
hi! VertSplit      ctermfg=13     ctermbg=13
hi! Folded         ctermfg=09     ctermbg=14
hi! FoldColumn     ctermfg=11     ctermbg=15
hi! TabLineSel     ctermfg=15     ctermbg=06

hi! Search         ctermfg=15     ctermbg=06
hi! IncSearch      ctermfg=15     ctermbg=02

hi! WildMenu       ctermfg=15     ctermbg=10
hi! Pmenu          ctermfg=15     ctermbg=06
hi! PmenuSel       ctermfg=15     ctermbg=01
hi! PmenuSbar      ctermfg=15     ctermbg=06
hi! PmenuThumb     ctermfg=15     ctermbg=09

hi! CursorColumn   ctermbg=14
hi! CursorLine     ctermbg=14

hi! link TabLine StatusLineNC
hi! link TabLineFill StatusLineNC
hi! link SignColumn LineNr

" ColorColumn
" Cursor
" CursorIM

" VisualNOS
" Conceal
" EndOfBuffer

" DiffAdd
" DiffChange
" DiffDelete
" DiffText

hi! ErrorMsg       cterm=bold
hi! ModeMsg        cterm=bold
hi! MoreMsg        cterm=bold
hi! WarningMsg     cterm=bold
hi! Directory      cterm=bold

hi! fzf1  ctermfg=01 ctermbg=08
hi! fzf2  ctermfg=02 ctermbg=08
hi! fzf3  ctermfg=07 ctermbg=08

" }}}
" {{{ Syntax highlighting
" *********************************************************

hi! Comment                                 cterm=bold,italic
hi! Underlined                              cterm=underline
hi! Title                                   cterm=bold
   
hi! SpellBad    ctermfg=01                  cterm=underline
hi! SpellCap                                cterm=underline
hi! SpellLocal                              cterm=underline
hi! SpellRare                               cterm=underline
   
hi! htmlBold                                cterm=bold
hi! htmlItalic                              cterm=italic
   
hi! Ignore                                  cterm=bold
hi! Error       ctermfg=15  ctermbg=01
hi! Todo                                    cterm=bold
                       
hi! Special                                 cterm=italic
hi! MatchParen                              cterm=bold
hi! SpecialKey                              cterm=bold
hi! Ignore                                  cterm=bold
hi! rstEmphasis                             cterm=italic
hi! todo                                    cterm=bold
hi! MatchParen                              cterm=bold
               
hi! helpHyperTextJump       ctermfg=05
hi! helpHyperTextEntry      ctermfg=05

hi link mk04ockquote Normal

hi! htmlH1 ctermfg=04 ctermbg=14 cterm=bold,italic
hi! htmlH2 ctermfg=04 cterm=bold
hi! htmlH3 ctermfg=00 cterm=bold

"hi! NormalMode   ctermfg=15  ctermbg=
hi! InsertMode   ctermfg=15  ctermbg=02
hi! VisualMode   ctermfg=00  ctermbg=14
hi! ReplaceMode  ctermfg=15  ctermbg=01
hi! CommandMode  ctermfg=15  ctermbg=04


" }}}
