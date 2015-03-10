hi clear
syntax reset
let g:colors_name = "oliver.light"

" TEXT COLORS
" Brown   094   Grey-0  236
" Orange  208   Grey-1  237
" Yellow  208   Grey-2  241
" Green   028   Grey-3  244
" Cyan    037   Grey-4  250
" Blue    032   Grey-5  251
" Magenta 098   Grey-6  256
" Pink    200   Grey-7  015
" Red     160   

" The Basics
hi Normal        ctermfg=236   ctermbg=255   cterm=NONE
hi Cursor        ctermfg=015   ctermbg=236   cterm=NONE
hi Bold          ctermfg=NONE  ctermbg=NONE  cterm=bold
hi Italic        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi Underlined    ctermfg=160   ctermbg=NONE  cterm=underline

" The Structure
hi StatusLine    ctermfg=015   ctermbg=256   cterm=NONE
hi StatusLineNC  ctermfg=250   ctermbg=256   cterm=NONE
hi VertSplit     ctermfg=245   ctermbg=256   cterm=NONE
hi TabLine       ctermfg=250   ctermbg=256   cterm=NONE
hi TabLineFill   ctermfg=250   ctermbg=256   cterm=NONE
hi TabLineSel    ctermfg=015   ctermbg=256   cterm=NONE

hi LineNr        ctermfg=251   ctermbg=254   cterm=NONE
hi CursorLineNr  ctermfg=251   ctermbg=254   cterm=NONE
hi SignColumn    ctermfg=251   ctermbg=254   cterm=NONE
hi CursorLine    ctermfg=NONE  ctermbg=254   cterm=NONE
hi CursorColumn  ctermfg=NONE  ctermbg=254   cterm=NONE
hi ColorColumn   ctermfg=NONE  ctermbg=254   cterm=NONE

hi PMenu         ctermfg=236   ctermbg=250   cterm=NONE
hi PMenuSel      ctermfg=237   ctermbg=208   cterm=NONE
hi PmenuSbar     ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi PmenuThumb    ctermbg=NONE  ctermfg=NONE  cterm=NONE

" Misc
hi ErrorMsg      ctermfg=160   ctermbg=236   cterm=NONE
hi FoldColumn    ctermfg=NONE  ctermbg=238   cterm=NONE
hi Folded        ctermfg=248   ctermbg=255   cterm=NONE
hi IncSearch     ctermfg=237   ctermbg=208   cterm=NONE
hi Macro         ctermfg=160   ctermbg=NONE  cterm=NONE
hi MatchParen    ctermfg=236   ctermbg=244   cterm=NONE
hi ModeMsg       ctermfg=208   ctermbg=NONE  cterm=NONE
hi MoreMsg       ctermfg=037   ctermbg=NONE  cterm=NONE
hi Question      ctermfg=208   ctermbg=NONE  cterm=NONE
hi Search        ctermfg=244   ctermbg=208   cterm=NONE
hi Visual        ctermfg=NONE  ctermbg=251   cterm=NONE
hi VisualNOS     ctermfg=160   ctermbg=NONE  cterm=NONE
hi WarningMsg    ctermfg=160   ctermbg=NONE  cterm=NONE
hi WildMenu      ctermfg=160   ctermbg=NONE  cterm=NONE

" Standard syntax highlighting
hi Boolean      ctermfg=208  ctermbg=NONE cterm=NONE
hi Character    ctermfg=160  ctermbg=NONE cterm=NONE
hi Comment      ctermfg=244  ctermbg=NONE cterm=NONE
hi Conceal      ctermfg=032  ctermbg=236  cterm=NONE
hi Conditional  ctermfg=098  ctermbg=NONE cterm=NONE
hi Constant     ctermfg=208  ctermbg=NONE cterm=NONE
hi Debug        ctermfg=160  ctermbg=NONE cterm=NONE
hi Define       ctermfg=098  ctermbg=NONE cterm=NONE
hi Delimiter    ctermfg=094  ctermbg=NONE cterm=NONE
hi Directory    ctermfg=032  ctermbg=NONE cterm=NONE
hi Exception    ctermfg=160  ctermbg=NONE cterm=NONE
hi Float        ctermfg=208  ctermbg=NONE cterm=NONE
hi Function     ctermfg=032  ctermbg=NONE cterm=NONE
hi Identifier   ctermfg=160  ctermbg=NONE cterm=NONE
hi Include      ctermfg=032  ctermbg=NONE cterm=NONE
hi Keyword      ctermfg=098  ctermbg=NONE cterm=NONE
hi Label        ctermfg=208  ctermbg=NONE cterm=NONE
hi NonText      ctermfg=240  ctermbg=NONE cterm=NONE
hi Number       ctermfg=098  ctermbg=NONE cterm=NONE
hi Operator     ctermfg=160  ctermbg=NONE cterm=NONE
hi PreProc      ctermfg=208  ctermbg=NONE cterm=NONE
hi Repeat       ctermfg=208  ctermbg=NONE cterm=NONE
hi Special      ctermfg=208  ctermbg=NONE cterm=NONE
hi SpecialChar  ctermfg=208  ctermbg=NONE cterm=NONE
hi SpecialKey   ctermfg=208  ctermbg=NONE cterm=NONE
hi Statement    ctermfg=032  ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=208  ctermbg=NONE cterm=NONE
hi String       ctermfg=028  ctermbg=NONE cterm=NONE
hi Structure    ctermfg=098  ctermbg=NONE cterm=NONE
hi Tag          ctermfg=208  ctermbg=NONE cterm=NONE
hi Title        ctermfg=160  ctermbg=NONE cterm=NONE
hi Todo         ctermfg=208  ctermbg=237  cterm=NONE
hi TooLong      ctermfg=160  ctermbg=NONE cterm=NONE
hi Type         ctermfg=037  ctermbg=NONE cterm=NONE
hi Typedef      ctermfg=208  ctermbg=NONE cterm=NONE

" Spelling highlighting
hi SpellBad     ctermfg=NONE ctermbg=NONE cterm=undercurl
hi SpellLocal   ctermfg=NONE ctermbg=NONE cterm=undercurl
hi SpellCap     ctermfg=NONE ctermbg=NONE cterm=undercurl
hi SpellRare    ctermfg=NONE ctermbg=NONE cterm=undercurl

" Additional diff highlighting
hi DiffAdd      ctermfg=028  ctermbg=236  cterm=NONE
hi DiffChange   ctermfg=032  ctermbg=236  cterm=NONE
hi DiffDelete   ctermfg=160  ctermbg=236  cterm=NONE
hi DiffText     ctermfg=032  ctermbg=236  cterm=NONE
hi DiffAdded    ctermfg=028  ctermbg=236  cterm=NONE
hi DiffFile     ctermfg=160  ctermbg=236  cterm=NONE
hi DiffNewFile  ctermfg=028  ctermbg=236  cterm=NONE
hi DiffLine     ctermfg=032  ctermbg=236  cterm=NONE
hi DiffRemoved  ctermfg=160  ctermbg=236  cterm=NONE

" Ruby highlighting
hi rubyAttribute               ctermfg=032 ctermbg=NONE cterm=NONE
hi rubyConstant                ctermfg=208 ctermbg=NONE cterm=NONE
hi rubyInterpolation           ctermfg=028 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=094 ctermbg=NONE cterm=NONE
hi rubyRegexp                  ctermfg=160 ctermbg=NONE cterm=NONE
hi rubyRegexpSpecial           ctermfg=032
hi rubyRegexpParens            ctermfg=208
hi rubyStringEscape            ctermfg=098
hi rubySymbol                  ctermfg=160 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter         ctermfg=028 ctermbg=NONE cterm=NONE
hi rubyDoBlock                 ctermfg=208
hi rubyConditionalExpression   ctermfg=251

" PHP highlighting
hi phpMemberSelector  ctermfg=251 ctermbg=NONE cterm=NONE
hi phpComparison      ctermfg=251 ctermbg=NONE cterm=NONE
hi phpParent          ctermfg=251 ctermbg=NONE cterm=NONE

" HTML highlighting
hi htmlBold    ctermfg=208  ctermbg=NONE cterm=bold
hi htmlItalic  ctermfg=208  ctermbg=NONE cterm=NONE
hi htmlEndTag  ctermfg=094  ctermbg=NONE cterm=NONE
hi htmlTag     ctermfg=094  ctermbg=NONE cterm=NONE
hi link htmlTagN htmlTagName
hi htmlLink    ctermfg=NONE ctermbg=NONE cterm=NONE
hi htmlH1      ctermfg=032 ctermbg=NONE cterm=NONE
hi htmlH2      ctermfg=032 ctermbg=NONE cterm=NONE

" CSS highlighting
hi cssBraces      ctermfg=251 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=098 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=037 ctermbg=NONE cterm=NONE

" SASS highlighting
hi sassidChar     ctermfg=160 ctermbg=NONE cterm=NONE
hi sassClassChar  ctermfg=208 ctermbg=NONE cterm=NONE
hi sassInclude    ctermfg=098 ctermbg=NONE cterm=NONE
hi sassMixing     ctermfg=098 ctermbg=NONE cterm=NONE
hi sassMixinName  ctermfg=032 ctermbg=NONE cterm=NONE

" JavaScript highlighting
hi javaScript        ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptBraces  ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptNumber  ctermfg=208 ctermbg=NONE cterm=NONE

" Markdown highlighting
hi markdownCode              ctermfg=028 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=028 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=032 ctermbg=NONE cterm=NONE
hi mkdInlineURL              ctermfg=032 ctermbg=NONE cterm=NONE

" Git highlighting
hi gitCommitOverflow  ctermfg=160 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=028 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
hi GitGutterAdd           ctermfg=028 ctermbg=255 cterm=NONE
hi GitGutterChange        ctermfg=208 ctermbg=255 cterm=NONE
hi GitGutterDelete        ctermfg=160 ctermbg=255 cterm=NONE
hi GitGutterChangeDelete  ctermfg=098 ctermbg=255 cterm=NONE

" Signify highlighting
hi SignifySignAdd     ctermfg=028 ctermbg=237 cterm=NONE
hi SignifySignChange  ctermfg=032 ctermbg=237 cterm=NONE
hi SignifySignDelete  ctermfg=160 ctermbg=237 cterm=NONE

" NERDTree highlighting
hi NERDTreeDirSlash  ctermfg=032 ctermbg=NONE cterm=NONE
hi NERDTreeExecFile  ctermfg=251 ctermbg=NONE cterm=NONE

" Oliver's random shit
hi vimIsCommand      ctermfg=160
hi vimMapRhs         ctermfg=028
