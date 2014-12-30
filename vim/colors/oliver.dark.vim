hi clear
syntax reset
let g:colors_name = "oliver.dark"

" Text Colors
"
" Brown   180
" Orange  216
" Yellow  223
" Green   107
" Cyan    116
" Blue    039
" Magenta 219
" Pink    218
" Red     174
"
" Grey-0  236
" Grey-1  237
" Grey-2  241
" Grey-3  244
" Grey-4  250
" Grey-5  251
" Grey-6  256
" Grey-7  015

" The Basics
hi Normal        ctermfg=256   ctermbg=236   cterm=NONE
hi Cursor        ctermfg=236   ctermbg=015   cterm=NONE
hi Bold          ctermfg=NONE  ctermbg=NONE  cterm=bold
hi Italic        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi Underlined    ctermfg=174   ctermbg=NONE  cterm=underline

" The Structure
hi StatusLine    ctermfg=251   ctermbg=237   cterm=NONE
hi StatusLineNC  ctermfg=236   ctermbg=237   cterm=NONE
hi VertSplit     ctermfg=237   ctermbg=237   cterm=NONE
hi TabLine       ctermfg=236   ctermbg=237   cterm=NONE
hi TabLineFill   ctermfg=236   ctermbg=237   cterm=NONE
hi TabLineSel    ctermfg=251   ctermbg=236   cterm=NONE

hi LineNr        ctermfg=244   ctermbg=237   cterm=NONE
hi CursorLineNr  ctermfg=223   ctermbg=236   cterm=NONE
hi SignColumn    ctermfg=251   ctermbg=237   cterm=NONE
hi CursorLine    ctermfg=NONE  ctermbg=237   cterm=NONE
hi CursorColumn  ctermfg=NONE  ctermbg=237   cterm=NONE
hi ColorColumn   ctermfg=NONE  ctermbg=237   cterm=NONE

hi PMenu         ctermfg=236   ctermbg=250   cterm=NONE
hi PMenuSel      ctermfg=237   ctermbg=223   cterm=NONE
hi PmenuSbar     ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi PmenuThumb    ctermbg=NONE  ctermfg=NONE  cterm=NONE

" Misc
hi ErrorMsg      ctermfg=174   ctermbg=236   cterm=NONE
hi FoldColumn    ctermfg=NONE  ctermbg=237   cterm=NONE
hi Folded        ctermfg=251   ctermbg=NONE  cterm=NONE
hi IncSearch     ctermfg=237   ctermbg=216   cterm=NONE
hi Macro         ctermfg=174   ctermbg=NONE  cterm=NONE
hi MatchParen    ctermfg=236   ctermbg=244   cterm=NONE
hi ModeMsg       ctermfg=223   ctermbg=NONE  cterm=NONE
hi MoreMsg       ctermfg=116   ctermbg=NONE  cterm=NONE
hi Question      ctermfg=216   ctermbg=NONE  cterm=NONE
hi Search        ctermfg=244   ctermbg=223   cterm=NONE
hi SpecialKey    ctermfg=244   ctermbg=NONE  cterm=NONE
hi Visual        ctermfg=NONE  ctermbg=251   cterm=NONE
hi VisualNOS     ctermfg=174   ctermbg=NONE  cterm=NONE
hi WarningMsg    ctermfg=174   ctermbg=NONE  cterm=NONE
hi WildMenu      ctermfg=174   ctermbg=NONE  cterm=NONE

" Standard syntax highlighting
hi Boolean      ctermfg=216 ctermbg=NONE cterm=NONE
hi Character    ctermfg=174 ctermbg=NONE cterm=NONE
hi Comment      ctermfg=244 ctermbg=NONE cterm=NONE
hi Conceal      ctermfg=039 ctermbg=236  cterm=NONE
hi Conditional  ctermfg=219 ctermbg=NONE cterm=NONE
hi Constant     ctermfg=216 ctermbg=NONE cterm=NONE
hi Debug        ctermfg=174 ctermbg=NONE cterm=NONE
hi Define       ctermfg=219 ctermbg=NONE cterm=NONE
hi Delimiter    ctermfg=180 ctermbg=NONE cterm=NONE
hi Directory    ctermfg=039 ctermbg=NONE cterm=NONE
hi Exception    ctermfg=174 ctermbg=NONE cterm=NONE
hi Float        ctermfg=216 ctermbg=NONE cterm=NONE
hi Function     ctermfg=039 ctermbg=NONE cterm=NONE
hi Identifier   ctermfg=174 ctermbg=NONE cterm=NONE
hi Include      ctermfg=039 ctermbg=NONE cterm=NONE
hi Keyword      ctermfg=219 ctermbg=NONE cterm=NONE
hi Label        ctermfg=223 ctermbg=NONE cterm=NONE
hi NonText      ctermfg=244 ctermbg=NONE cterm=NONE
hi Number       ctermfg=219 ctermbg=NONE cterm=NONE
hi Operator     ctermfg=251 ctermbg=NONE cterm=NONE
hi PreProc      ctermfg=223 ctermbg=NONE cterm=NONE
hi Repeat       ctermfg=223 ctermbg=NONE cterm=NONE
hi Special      ctermfg=116 ctermbg=NONE cterm=NONE
hi SpecialChar  ctermfg=180 ctermbg=NONE cterm=NONE
hi SpecialKey   ctermfg=244 ctermbg=NONE cterm=NONE
hi Statement    ctermfg=174 ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=223 ctermbg=NONE cterm=NONE
hi String       ctermfg=107 ctermbg=NONE cterm=NONE
hi Structure    ctermfg=219 ctermbg=NONE cterm=NONE
hi Tag          ctermfg=223 ctermbg=NONE cterm=NONE
hi Title        ctermfg=015 ctermbg=NONE cterm=NONE
hi Todo         ctermfg=223 ctermbg=237  cterm=NONE
hi TooLong      ctermfg=174 ctermbg=NONE cterm=NONE
hi Type         ctermfg=110 ctermbg=NONE cterm=NONE
hi Typedef      ctermfg=223 ctermbg=NONE cterm=NONE

" Spelling highlighting
hi SpellBad     ctermfg=NONE ctermbg=236 cterm=undercurl
hi SpellLocal   ctermfg=NONE ctermbg=236 cterm=undercurl
hi SpellCap     ctermfg=NONE ctermbg=236 cterm=undercurl
hi SpellRare    ctermfg=NONE ctermbg=236 cterm=undercurl

" Additional diff highlighting
hi DiffAdd      ctermfg=107 ctermbg=236 cterm=NONE
hi DiffChange   ctermfg=039 ctermbg=236 cterm=NONE
hi DiffDelete   ctermfg=174 ctermbg=236 cterm=NONE
hi DiffText     ctermfg=039 ctermbg=236 cterm=NONE
hi DiffAdded    ctermfg=107 ctermbg=236 cterm=NONE
hi DiffFile     ctermfg=174 ctermbg=236 cterm=NONE
hi DiffNewFile  ctermfg=107 ctermbg=236 cterm=NONE
hi DiffLine     ctermfg=039 ctermbg=236 cterm=NONE
hi DiffRemoved  ctermfg=174 ctermbg=236 cterm=NONE

" Ruby highlighting
hi rubyAttribute               ctermfg=039 ctermbg=NONE cterm=NONE
hi rubyConstant                ctermfg=223 ctermbg=NONE cterm=NONE
hi rubyInterpolation           ctermfg=107 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=180 ctermbg=NONE cterm=NONE
hi rubyRegexp                  ctermfg=116 ctermbg=NONE cterm=NONE
hi rubySymbol                  ctermfg=107 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter         ctermfg=107 ctermbg=NONE cterm=NONE

" PHP highlighting
hi phpMemberSelector  ctermfg=251 ctermbg=NONE cterm=NONE
hi phpComparison      ctermfg=251 ctermbg=NONE cterm=NONE
hi phpParent          ctermfg=251 ctermbg=NONE cterm=NONE

" HTML highlighting
hi htmlBold    ctermfg=223 ctermbg=NONE cterm=bold
hi htmlItalic  ctermfg=116 ctermbg=NONE cterm=NONE
hi htmlEndTag  ctermfg=251 ctermbg=NONE cterm=NONE
hi htmlTag     ctermfg=251 ctermbg=NONE cterm=NONE
hi link htmlTagN htmlTagName

" CSS highlighting
hi cssBraces      ctermfg=251 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=219 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=116 ctermbg=NONE cterm=NONE

" SASS highlighting
hi sassidChar     ctermfg=174 ctermbg=NONE cterm=NONE
hi sassClassChar  ctermfg=216 ctermbg=NONE cterm=NONE
hi sassInclude    ctermfg=219 ctermbg=NONE cterm=NONE
hi sassMixing     ctermfg=219 ctermbg=NONE cterm=NONE
hi sassMixinName  ctermfg=039 ctermbg=NONE cterm=NONE

" JavaScript highlighting
hi javaScript        ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptBraces  ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptNumber  ctermfg=216 ctermbg=NONE cterm=NONE

" Markdown highlighting
hi markdownCode              ctermfg=107 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=107 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=039 ctermbg=NONE cterm=NONE
hi htmlH1                    ctermfg=116 ctermbg=NONE cterm=NONE
hi htmlH2                    ctermfg=223 ctermbg=NONE cterm=NONE
hi mkdInlineURL              ctermfg=039 ctermbg=NONE cterm=NONE

" Git highlighting
hi gitCommitOverflow  ctermfg=174 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=107 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
hi GitGutterAdd           ctermfg=107 ctermbg=237 cterm=NONE
hi GitGutterChange        ctermfg=216 ctermbg=237 cterm=NONE
hi GitGutterDelete        ctermfg=174 ctermbg=237 cterm=NONE
hi GitGutterChangeDelete  ctermfg=219 ctermbg=237 cterm=NONE

" Signify highlighting
hi SignifySignAdd     ctermfg=107 ctermbg=237 cterm=NONE
hi SignifySignChange  ctermfg=039 ctermbg=237 cterm=NONE
hi SignifySignDelete  ctermfg=174 ctermbg=237 cterm=NONE

" NERDTree highlighting
hi NERDTreeDirSlash  ctermfg=039 ctermbg=NONE cterm=NONE
hi NERDTreeExecFile  ctermfg=251 ctermbg=NONE cterm=NONE

" Oliver's random shit
hi vimIsCommand      ctermfg=039
hi vimMapRhs         ctermfg=107
