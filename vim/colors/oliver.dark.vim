hi clear
syntax reset
let g:colors_name = "oliver.dark"

" This scheme is designed to be used with the
" 'oliver.dark' terminal color scheme.
" It uses the ANSI codes for colors
" and 256 codes for shades of gray.

" TEXT COLORS

" Black    00
" Red      01
" Green    02
" Yellow   03
" Blue     04
" Magenta  05
" Cyan     06
" Grey 1   07
" Grey 2   08
" Orange   09
"          10
" Brown    11
"          12
" Pink     13
"          14
" White    15

" The Basics
hi Normal        ctermfg=255   ctermbg=236   cterm=NONE
hi Cursor        ctermfg=236   ctermbg=015   cterm=NONE
hi Bold          ctermfg=NONE  ctermbg=NONE  cterm=bold
hi Italic        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi Underlined    ctermfg=001   ctermbg=NONE  cterm=NONE

" The Structure
hi StatusLine    ctermfg=004   ctermbg=235   cterm=NONE
hi TabLineSel    ctermfg=004   ctermbg=235   cterm=NONE
hi TabLine       ctermfg=240   ctermbg=235   cterm=NONE
hi StatusLineNC  ctermfg=240   ctermbg=235   cterm=NONE
hi VertSplit     ctermfg=235   ctermbg=235   cterm=NONE
hi TabLineFill   ctermfg=000   ctermbg=235   cterm=NONE

hi LineNr        ctermfg=240   ctermbg=237   cterm=NONE
hi CursorLineNr  ctermfg=255   ctermbg=237   cterm=NONE
hi SignColumn    ctermfg=251   ctermbg=237   cterm=NONE
hi FoldColumn    ctermfg=251   ctermbg=237   cterm=NONE
hi CursorLine    ctermfg=NONE  ctermbg=237   cterm=NONE
hi CursorColumn  ctermfg=NONE  ctermbg=237   cterm=NONE
hi ColorColumn   ctermfg=NONE  ctermbg=237   cterm=NONE

hi PMenu         ctermfg=236   ctermbg=250   cterm=NONE
hi PMenuSel      ctermfg=237   ctermbg=003   cterm=NONE
hi PmenuSbar     ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi PmenuThumb    ctermbg=NONE  ctermfg=NONE  cterm=NONE

" Misc
hi Error         ctermfg=223   ctermbg=232
hi ErrorMsg      ctermfg=001   ctermbg=236   cterm=NONE
hi FoldColumn    ctermfg=NONE  ctermbg=238   cterm=NONE
hi Folded        ctermfg=244   ctermbg=237   cterm=NONE
hi IncSearch     ctermfg=237   ctermbg=009   cterm=NONE
hi Macro         ctermfg=001   ctermbg=NONE  cterm=NONE
hi MatchParen    ctermfg=236   ctermbg=244   cterm=NONE
hi ModeMsg       ctermfg=003   ctermbg=NONE   cterm=NONE
hi MoreMsg       ctermfg=006   ctermbg=NONE  cterm=NONE
hi Question      ctermfg=009   ctermbg=NONE  cterm=NONE
hi Search        ctermfg=244   ctermbg=003   cterm=NONE
hi Visual        ctermfg=NONE  ctermbg=240   cterm=NONE
hi VisualNOS     ctermfg=001   ctermbg=NONE  cterm=NONE
hi WarningMsg    ctermfg=001   ctermbg=NONE  cterm=NONE
hi WildMenu      ctermfg=001   ctermbg=NONE  cterm=NONE

" Standard syntax highlighting
hi Boolean      ctermfg=009  ctermbg=NONE cterm=NONE
hi Character    ctermfg=001  ctermbg=NONE cterm=NONE
hi Comment      ctermfg=244  ctermbg=NONE cterm=NONE
hi Conceal      ctermfg=004  ctermbg=236  cterm=NONE
hi Conditional  ctermfg=005  ctermbg=NONE cterm=NONE
hi Constant     ctermfg=003  ctermbg=NONE cterm=NONE
hi Debug        ctermfg=001  ctermbg=NONE cterm=NONE
hi Define       ctermfg=005  ctermbg=NONE cterm=NONE
hi Delimiter    ctermfg=004  ctermbg=NONE cterm=NONE
hi Directory    ctermfg=004  ctermbg=NONE cterm=NONE
hi Exception    ctermfg=001  ctermbg=NONE cterm=NONE
hi Float        ctermfg=009  ctermbg=NONE cterm=NONE
hi Function     ctermfg=004  ctermbg=NONE cterm=NONE
hi Identifier   ctermfg=001  ctermbg=NONE cterm=NONE
hi Include      ctermfg=004  ctermbg=NONE cterm=NONE
hi Keyword      ctermfg=005  ctermbg=NONE cterm=NONE
hi Label        ctermfg=003  ctermbg=NONE cterm=NONE
hi NonText      ctermfg=240  ctermbg=NONE cterm=NONE
hi Number       ctermfg=005  ctermbg=NONE cterm=NONE
hi Operator     ctermfg=251  ctermbg=NONE cterm=NONE
hi PreProc      ctermfg=003  ctermbg=NONE cterm=NONE
hi Repeat       ctermfg=003  ctermbg=NONE cterm=NONE
hi Special      ctermfg=009  ctermbg=NONE cterm=NONE
hi SpecialChar  ctermfg=009  ctermbg=NONE cterm=NONE
hi SpecialKey   ctermfg=240  ctermbg=NONE cterm=NONE
hi Statement    ctermfg=004  ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=009  ctermbg=NONE cterm=NONE
hi String       ctermfg=002  ctermbg=NONE cterm=NONE
hi Structure    ctermfg=005  ctermbg=NONE cterm=NONE
hi Tag          ctermfg=003  ctermbg=NONE cterm=NONE
hi Title        ctermfg=015  ctermbg=NONE cterm=NONE
hi Todo         ctermfg=003  ctermbg=237  cterm=NONE
hi TooLong      ctermfg=001  ctermbg=NONE cterm=NONE
hi Type         ctermfg=006  ctermbg=NONE cterm=NONE
hi Typedef      ctermfg=003  ctermbg=NONE cterm=NONE

" Spelling highlighting
hi SpellBad     ctermfg=NONE ctermbg=234  cterm=NONE
hi SpellLocal   ctermfg=NONE ctermbg=234  cterm=NONE
hi SpellCap     ctermfg=NONE ctermbg=234  cterm=NONE
hi SpellRare    ctermfg=NONE ctermbg=234  cterm=NONE

" Additional diff highlighting
hi DiffAdd      ctermfg=002  ctermbg=236  cterm=NONE
hi DiffChange   ctermfg=004  ctermbg=236  cterm=NONE
hi DiffDelete   ctermfg=001  ctermbg=236  cterm=NONE
hi DiffText     ctermfg=004  ctermbg=236  cterm=NONE
hi DiffAdded    ctermfg=002  ctermbg=236  cterm=NONE
hi DiffFile     ctermfg=001  ctermbg=236  cterm=NONE
hi DiffNewFile  ctermfg=002  ctermbg=236  cterm=NONE
hi DiffLine     ctermfg=004  ctermbg=236  cterm=NONE
hi DiffRemoved  ctermfg=001  ctermbg=236  cterm=NONE

" Ruby highlighting
hi rubyAttribute               ctermfg=004 ctermbg=NONE cterm=NONE
hi rubyConstant                ctermfg=003 ctermbg=NONE cterm=NONE
hi rubyInterpolation           ctermfg=002 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=011 ctermbg=NONE cterm=NONE
hi rubyRegexp                  ctermfg=001 ctermbg=NONE cterm=NONE
hi rubyRegexpSpecial           ctermfg=004
hi rubyRegexpParens            ctermfg=009
hi rubyStringEscape            ctermfg=005
hi rubySymbol                  ctermfg=001 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter         ctermfg=002 ctermbg=NONE cterm=NONE
hi rubyDoBlock                 ctermfg=003
hi rubyConditionalExpression   ctermfg=251

" PHP highlighting
hi phpMemberSelector  ctermfg=251 ctermbg=NONE cterm=NONE
hi phpComparison      ctermfg=251 ctermbg=NONE cterm=NONE
hi phpParent          ctermfg=251 ctermbg=NONE cterm=NONE

" HTML highlighting
hi htmlBold    ctermfg=003  ctermbg=NONE cterm=bold
hi htmlItalic  ctermfg=009  ctermbg=NONE cterm=NONE
hi htmlEndTag  ctermfg=011  ctermbg=NONE cterm=NONE
hi htmlTag     ctermfg=011  ctermbg=NONE cterm=NONE
hi link htmlTagN htmlTagName
hi htmlLink    ctermfg=NONE ctermbg=NONE cterm=NONE
hi htmlH1      ctermfg=003 ctermbg=NONE cterm=NONE
hi htmlH2      ctermfg=006 ctermbg=NONE cterm=NONE
hi htmlH3      ctermfg=004 ctermbg=NONE cterm=NONE

" CSS highlighting
hi cssBraces      ctermfg=251 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=005 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=006 ctermbg=NONE cterm=NONE

" SASS highlighting
hi sassidChar     ctermfg=001 ctermbg=NONE cterm=NONE
hi sassClassChar  ctermfg=009 ctermbg=NONE cterm=NONE
hi sassInclude    ctermfg=005 ctermbg=NONE cterm=NONE
hi sassMixing     ctermfg=005 ctermbg=NONE cterm=NONE
hi sassMixinName  ctermfg=004 ctermbg=NONE cterm=NONE

" JavaScript highlighting
hi javaScript        ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptBraces  ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptNumber  ctermfg=009 ctermbg=NONE cterm=NONE

" Markdown highlighting
hi markdownCode              ctermfg=002 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=002 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=004 ctermbg=NONE cterm=NONE
hi mkdInlineURL              ctermfg=005 ctermbg=NONE cterm=NONE
hi mkdBlockquote             ctermfg=011
hi mkdLineContinue           ctermfg=011

" Git highlighting
hi gitCommitOverflow  ctermfg=001 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=002 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
hi GitGutterAdd           ctermfg=002 ctermbg=237 cterm=NONE
hi GitGutterChange        ctermfg=009 ctermbg=237 cterm=NONE
hi GitGutterDelete        ctermfg=001 ctermbg=237 cterm=NONE
hi GitGutterChangeDelete  ctermfg=005 ctermbg=237 cterm=NONE

" Signify highlighting
hi SignifySignAdd     ctermfg=002 ctermbg=237 cterm=NONE
hi SignifySignChange  ctermfg=004 ctermbg=237 cterm=NONE
hi SignifySignDelete  ctermfg=001 ctermbg=237 cterm=NONE

" NERDTree highlighting
hi NERDTreeDirSlash  ctermfg=004 ctermbg=NONE cterm=NONE
hi NERDTreeExecFile  ctermfg=251 ctermbg=NONE cterm=NONE

" Oliver's random shit
hi vimIsCommand      ctermfg=001
hi vimMapRhs         ctermfg=002
