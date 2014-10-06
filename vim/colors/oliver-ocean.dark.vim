hi clear
syntax reset
let g:colors_name = "oliver-ocean.dark"

" This is a refactoring / customization of of Base16-Ocean.dark
" The color-codes below refer to the ANSI numbers assigned here:
" https://github.com/chriskempson/base16-iterm2

" Text Colors
" Red     01
" Green   02
" Yellow  03
" Blue    04
" Magenta 05
" Cyan    06
" Orange  09
" Brown   14
" Grey-0  00
" Grey-1  10
" Grey-2  11
" Grey-3  08
" Grey-4  12
" Grey-5  13
" Grey-6  07
" Grey-7  15

" The Basics
hi Normal        ctermfg=013   ctermbg=000   cterm=NONE
hi Cursor        ctermfg=000   ctermbg=015   cterm=NONE
hi Bold          ctermfg=NONE  ctermbg=NONE  cterm=bold
hi Italic        ctermfg=NONE  ctermbg=NONE  cterm=italic
hi Underlined    ctermfg=001   ctermbg=NONE  cterm=underline

" The Structure
hi StatusLine    ctermfg=015   ctermbg=008   cterm=NONE
hi StatusLineNC  ctermfg=000   ctermbg=011   cterm=NONE
hi VertSplit     ctermfg=011   ctermbg=011   cterm=NONE
hi TabLine       ctermfg=015   ctermbg=010   cterm=NONE
hi TabLineFill   ctermfg=015   ctermbg=010   cterm=NONE
hi TabLineSel    ctermfg=003   ctermbg=010   cterm=NONE

hi LineNr        ctermfg=008   ctermbg=010   cterm=NONE
hi CursorLineNr  ctermfg=003   ctermbg=000   cterm=NONE
hi SignColumn    ctermfg=013   ctermbg=010   cterm=NONE
hi CursorLine    ctermfg=NONE  ctermbg=010   cterm=NONE
hi CursorColumn  ctermfg=NONE  ctermbg=010   cterm=NONE
hi ColorColumn   ctermfg=NONE  ctermbg=010   cterm=NONE

hi PMenu         ctermfg=000   ctermbg=012   cterm=NONE
hi PMenuSel      ctermfg=010   ctermbg=003   cterm=NONE
hi PmenuSbar     ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi PmenuThumb    ctermbg=NONE  ctermfg=NONE  cterm=NONE

" Misc
hi ErrorMsg      ctermfg=001   ctermbg=000   cterm=NONE
hi FoldColumn    ctermfg=NONE  ctermbg=010   cterm=NONE
hi Folded        ctermfg=011   ctermbg=NONE  cterm=italic
hi IncSearch     ctermfg=010   ctermbg=009   cterm=NONE
hi Macro         ctermfg=001   ctermbg=NONE  cterm=NONE
hi MatchParen    ctermfg=000   ctermbg=008   cterm=NONE
hi ModeMsg       ctermfg=003   ctermbg=NONE  cterm=NONE
hi MoreMsg       ctermfg=006   ctermbg=NONE  cterm=NONE
hi Question      ctermfg=009   ctermbg=NONE  cterm=NONE
hi Search        ctermfg=008   ctermbg=003   cterm=NONE
hi SpecialKey    ctermfg=008   ctermbg=NONE  cterm=NONE
hi Visual        ctermfg=NONE  ctermbg=011   cterm=NONE
hi VisualNOS     ctermfg=001   ctermbg=NONE  cterm=NONE
hi WarningMsg    ctermfg=001   ctermbg=NONE  cterm=NONE
hi WildMenu      ctermfg=001   ctermbg=NONE  cterm=NONE

" Standard syntax highlighting
hi Boolean      ctermfg=009 ctermbg=NONE cterm=NONE
hi Character    ctermfg=001 ctermbg=NONE cterm=NONE
hi Comment      ctermfg=008 ctermbg=NONE cterm=italic
hi Conceal      ctermfg=004 ctermbg=000  cterm=NONE
hi Conditional  ctermfg=005 ctermbg=NONE cterm=NONE
hi Constant     ctermfg=009 ctermbg=NONE cterm=NONE
hi Debug        ctermfg=001 ctermbg=NONE cterm=NONE
hi Define       ctermfg=005 ctermbg=NONE cterm=NONE
hi Delimiter    ctermfg=014 ctermbg=NONE cterm=NONE
hi Directory    ctermfg=004 ctermbg=NONE cterm=NONE
hi Exception    ctermfg=001 ctermbg=NONE cterm=NONE
hi Float        ctermfg=009 ctermbg=NONE cterm=NONE
hi Function     ctermfg=004 ctermbg=NONE cterm=NONE
hi Identifier   ctermfg=001 ctermbg=NONE cterm=NONE
hi Include      ctermfg=004 ctermbg=NONE cterm=NONE
hi Keyword      ctermfg=005 ctermbg=NONE cterm=NONE
hi Label        ctermfg=003 ctermbg=NONE cterm=NONE
hi NonText      ctermfg=008 ctermbg=NONE cterm=NONE
hi Number       ctermfg=005 ctermbg=NONE cterm=NONE
hi Operator     ctermfg=013 ctermbg=NONE cterm=NONE
hi PreProc      ctermfg=003 ctermbg=NONE cterm=NONE
hi Repeat       ctermfg=003 ctermbg=NONE cterm=NONE
hi Special      ctermfg=006 ctermbg=NONE cterm=NONE
hi SpecialChar  ctermfg=014 ctermbg=NONE cterm=NONE
hi SpecialKey   ctermfg=008 ctermbg=NONE cterm=NONE
hi Statement    ctermfg=001 ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=003 ctermbg=NONE cterm=NONE
hi String       ctermfg=002 ctermbg=NONE cterm=NONE
hi Structure    ctermfg=005 ctermbg=NONE cterm=NONE
hi Tag          ctermfg=003 ctermbg=NONE cterm=NONE
hi Title        ctermfg=015 ctermbg=NONE cterm=NONE
hi Todo         ctermfg=003 ctermbg=010  cterm=NONE
hi TooLong      ctermfg=001 ctermbg=NONE cterm=NONE
hi Type         ctermfg=006 ctermbg=NONE cterm=NONE
hi Typedef      ctermfg=003 ctermbg=NONE cterm=NONE

" Spelling highlighting
hi SpellBad     ctermfg=NONE ctermbg=000 cterm=undercurl
hi SpellLocal   ctermfg=NONE ctermbg=000 cterm=undercurl
hi SpellCap     ctermfg=NONE ctermbg=000 cterm=undercurl
hi SpellRare    ctermfg=NONE ctermbg=000 cterm=undercurl

" Additional diff highlighting
hi DiffAdd      ctermfg=002 ctermbg=000 cterm=NONE
hi DiffChange   ctermfg=004 ctermbg=000 cterm=NONE
hi DiffDelete   ctermfg=001 ctermbg=000 cterm=NONE
hi DiffText     ctermfg=004 ctermbg=000 cterm=NONE
hi DiffAdded    ctermfg=002 ctermbg=000 cterm=NONE
hi DiffFile     ctermfg=001 ctermbg=000 cterm=NONE
hi DiffNewFile  ctermfg=002 ctermbg=000 cterm=NONE
hi DiffLine     ctermfg=004 ctermbg=000 cterm=NONE
hi DiffRemoved  ctermfg=001 ctermbg=000 cterm=NONE

" Ruby highlighting
hi rubyAttribute               ctermfg=004 ctermbg=NONE cterm=NONE
hi rubyConstant                ctermfg=003 ctermbg=NONE cterm=NONE
hi rubyInterpolation           ctermfg=002 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=014 ctermbg=NONE cterm=NONE
hi rubyRegexp                  ctermfg=006 ctermbg=NONE cterm=NONE
hi rubySymbol                  ctermfg=002 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter         ctermfg=002 ctermbg=NONE cterm=NONE

" PHP highlighting
hi phpMemberSelector  ctermfg=013 ctermbg=NONE cterm=NONE
hi phpComparison      ctermfg=013 ctermbg=NONE cterm=NONE
hi phpParent          ctermfg=013 ctermbg=NONE cterm=NONE

" HTML highlighting
hi htmlBold    ctermfg=003 ctermbg=NONE cterm=bold
hi htmlItalic  ctermfg=006 ctermbg=NONE cterm=italic
hi htmlEndTag  ctermfg=013 ctermbg=NONE cterm=NONE
hi htmlTag     ctermfg=013 ctermbg=NONE cterm=NONE
hi link htmlTagN htmlTagName

" CSS highlighting
hi cssBraces      ctermfg=013 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=005 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=006 ctermbg=NONE cterm=NONE

" SASS highlighting
hi sassidChar     ctermfg=001 ctermbg=NONE cterm=NONE
hi sassClassChar  ctermfg=009 ctermbg=NONE cterm=NONE
hi sassInclude    ctermfg=005 ctermbg=NONE cterm=NONE
hi sassMixing     ctermfg=005 ctermbg=NONE cterm=NONE
hi sassMixinName  ctermfg=004 ctermbg=NONE cterm=NONE

" JavaScript highlighting
hi javaScript        ctermfg=013 ctermbg=NONE cterm=NONE
hi javaScriptBraces  ctermfg=013 ctermbg=NONE cterm=NONE
hi javaScriptNumber  ctermfg=009 ctermbg=NONE cterm=NONE

" Markdown highlighting
hi markdownCode              ctermfg=002 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=002 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=004 ctermbg=NONE cterm=NONE
hi htmlH1                    ctermfg=005 ctermbg=NONE cterm=NONE
hi htmlH2                    ctermfg=003 ctermbg=NONE cterm=NONE

" Git highlighting
hi gitCommitOverflow  ctermfg=001 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=002 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
hi GitGutterAdd           ctermfg=002 ctermbg=010 cterm=NONE
hi GitGutterChange        ctermfg=009 ctermbg=010 cterm=NONE
hi GitGutterDelete        ctermfg=001 ctermbg=010 cterm=NONE
hi GitGutterChangeDelete  ctermfg=005 ctermbg=010 cterm=NONE

" Signify highlighting
hi SignifySignAdd     ctermfg=002 ctermbg=010 cterm=NONE
hi SignifySignChange  ctermfg=004 ctermbg=010 cterm=NONE
hi SignifySignDelete  ctermfg=001 ctermbg=010 cterm=NONE

" NERDTree highlighting
hi NERDTreeDirSlash  ctermfg=004 ctermbg=NONE cterm=NONE
hi NERDTreeExecFile  ctermfg=013 ctermbg=NONE cterm=NONE

" Oliver's random shit
hi vimIsCommand      ctermfg=004
hi vimMapRhs         ctermfg=002
