hi clear
syntax reset
let g:colors_name = "oliver.dark"

" TEXT COLORS
" Brown   180   Grey-0  236
" Orange  216   Grey-1  237
" Yellow  223   Grey-2  241
" Green   108   Grey-3  244
" Cyan    116   Grey-4  250
" Blue    110   Grey-5  251
" Magenta 219   Grey-6  256
" Pink    218   White   015
" Red     174   Black   000

" This scheme is designed to be used with the 'oliver.dark'
" terminal color scheme

" The Basics
hi Normal        ctermfg=256   ctermbg=236   cterm=NONE
hi Cursor        ctermfg=236   ctermbg=015   cterm=NONE
hi Bold          ctermfg=NONE  ctermbg=NONE  cterm=bold
hi Italic        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi Underlined    ctermfg=174   ctermbg=NONE  cterm=underline

" The Structure
hi StatusLine    ctermfg=110   ctermbg=235   cterm=NONE
hi TabLineSel    ctermfg=110   ctermbg=235   cterm=NONE
hi TabLine       ctermfg=240   ctermbg=235   cterm=NONE
hi StatusLineNC  ctermfg=240   ctermbg=235   cterm=NONE
hi VertSplit     ctermfg=235   ctermbg=235   cterm=NONE
hi TabLineFill   ctermfg=000   ctermbg=235   cterm=NONE

hi LineNr        ctermfg=240   ctermbg=237   cterm=NONE
hi CursorLineNr  ctermfg=256   ctermbg=237   cterm=NONE
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
hi FoldColumn    ctermfg=NONE  ctermbg=238   cterm=NONE
hi Folded        ctermfg=244   ctermbg=237   cterm=NONE
hi IncSearch     ctermfg=237   ctermbg=216   cterm=NONE
hi Macro         ctermfg=174   ctermbg=NONE  cterm=NONE
hi MatchParen    ctermfg=236   ctermbg=244   cterm=NONE
hi ModeMsg       ctermfg=223   ctermbg=NONE   cterm=NONE
hi MoreMsg       ctermfg=116   ctermbg=NONE  cterm=NONE
hi Question      ctermfg=216   ctermbg=NONE  cterm=NONE
hi Search        ctermfg=244   ctermbg=223   cterm=NONE
hi Visual        ctermfg=NONE  ctermbg=240   cterm=NONE
hi VisualNOS     ctermfg=174   ctermbg=NONE  cterm=NONE
hi WarningMsg    ctermfg=174   ctermbg=NONE  cterm=NONE
hi WildMenu      ctermfg=174   ctermbg=NONE  cterm=NONE

" Standard syntax highlighting
hi Boolean      ctermfg=216  ctermbg=NONE cterm=NONE
hi Character    ctermfg=174  ctermbg=NONE cterm=NONE
hi Comment      ctermfg=244  ctermbg=NONE cterm=NONE
hi Conceal      ctermfg=110  ctermbg=236  cterm=NONE
hi Conditional  ctermfg=219  ctermbg=NONE cterm=NONE
hi Constant     ctermfg=223  ctermbg=NONE cterm=NONE
hi Debug        ctermfg=174  ctermbg=NONE cterm=NONE
hi Define       ctermfg=219  ctermbg=NONE cterm=NONE
hi Delimiter    ctermfg=180  ctermbg=NONE cterm=NONE
hi Directory    ctermfg=110  ctermbg=NONE cterm=NONE
hi Exception    ctermfg=174  ctermbg=NONE cterm=NONE
hi Float        ctermfg=216  ctermbg=NONE cterm=NONE
hi Function     ctermfg=110  ctermbg=NONE cterm=NONE
hi Identifier   ctermfg=174  ctermbg=NONE cterm=NONE
hi Include      ctermfg=110  ctermbg=NONE cterm=NONE
hi Keyword      ctermfg=219  ctermbg=NONE cterm=NONE
hi Label        ctermfg=223  ctermbg=NONE cterm=NONE
hi NonText      ctermfg=240  ctermbg=NONE cterm=NONE
hi Number       ctermfg=219  ctermbg=NONE cterm=NONE
hi Operator     ctermfg=251  ctermbg=NONE cterm=NONE
hi PreProc      ctermfg=223  ctermbg=NONE cterm=NONE
hi Repeat       ctermfg=223  ctermbg=NONE cterm=NONE
hi Special      ctermfg=216  ctermbg=NONE cterm=NONE
hi SpecialChar  ctermfg=216  ctermbg=NONE cterm=NONE
hi SpecialKey   ctermfg=216  ctermbg=NONE cterm=NONE
hi Statement    ctermfg=110  ctermbg=NONE cterm=NONE
hi StorageClass ctermfg=216  ctermbg=NONE cterm=NONE
hi String       ctermfg=108  ctermbg=NONE cterm=NONE
hi Structure    ctermfg=219  ctermbg=NONE cterm=NONE
hi Tag          ctermfg=223  ctermbg=NONE cterm=NONE
hi Title        ctermfg=015  ctermbg=NONE cterm=NONE
hi Todo         ctermfg=223  ctermbg=237  cterm=NONE
hi TooLong      ctermfg=174  ctermbg=NONE cterm=NONE
hi Type         ctermfg=116  ctermbg=NONE cterm=NONE
hi Typedef      ctermfg=223  ctermbg=NONE cterm=NONE

" Spelling highlighting
hi SpellBad     ctermfg=NONE ctermbg=NONE cterm=underline
hi SpellLocal   ctermfg=NONE ctermbg=NONE cterm=underline
hi SpellCap     ctermfg=NONE ctermbg=NONE cterm=underline
hi SpellRare    ctermfg=NONE ctermbg=NONE cterm=underline

" Additional diff highlighting
hi DiffAdd      ctermfg=108  ctermbg=236  cterm=NONE
hi DiffChange   ctermfg=110  ctermbg=236  cterm=NONE
hi DiffDelete   ctermfg=174  ctermbg=236  cterm=NONE
hi DiffText     ctermfg=110  ctermbg=236  cterm=NONE
hi DiffAdded    ctermfg=108  ctermbg=236  cterm=NONE
hi DiffFile     ctermfg=174  ctermbg=236  cterm=NONE
hi DiffNewFile  ctermfg=108  ctermbg=236  cterm=NONE
hi DiffLine     ctermfg=110  ctermbg=236  cterm=NONE
hi DiffRemoved  ctermfg=174  ctermbg=236  cterm=NONE

" Ruby highlighting
hi rubyAttribute               ctermfg=110 ctermbg=NONE cterm=NONE
hi rubyConstant                ctermfg=223 ctermbg=NONE cterm=NONE
hi rubyInterpolation           ctermfg=108 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=180 ctermbg=NONE cterm=NONE
hi rubyRegexp                  ctermfg=174 ctermbg=NONE cterm=NONE
hi rubyRegexpSpecial           ctermfg=110
hi rubyRegexpParens            ctermfg=216
hi rubyStringEscape            ctermfg=219
hi rubySymbol                  ctermfg=174 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter         ctermfg=108 ctermbg=NONE cterm=NONE
hi rubyDoBlock                 ctermfg=223
hi rubyConditionalExpression   ctermfg=251

" PHP highlighting
hi phpMemberSelector  ctermfg=251 ctermbg=NONE cterm=NONE
hi phpComparison      ctermfg=251 ctermbg=NONE cterm=NONE
hi phpParent          ctermfg=251 ctermbg=NONE cterm=NONE

" HTML highlighting
hi htmlBold    ctermfg=223  ctermbg=NONE cterm=bold
hi htmlItalic  ctermfg=216  ctermbg=NONE cterm=NONE
hi htmlEndTag  ctermfg=180  ctermbg=NONE cterm=NONE
hi htmlTag     ctermfg=180  ctermbg=NONE cterm=NONE
hi link htmlTagN htmlTagName
hi htmlLink    ctermfg=NONE ctermbg=NONE cterm=NONE
hi htmlH1      ctermfg=223 ctermbg=NONE cterm=NONE
hi htmlH2      ctermfg=116 ctermbg=NONE cterm=NONE
hi htmlH3      ctermfg=110 ctermbg=NONE cterm=NONE

" CSS highlighting
hi cssBraces      ctermfg=251 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=219 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=116 ctermbg=NONE cterm=NONE

" SASS highlighting
hi sassidChar     ctermfg=174 ctermbg=NONE cterm=NONE
hi sassClassChar  ctermfg=216 ctermbg=NONE cterm=NONE
hi sassInclude    ctermfg=219 ctermbg=NONE cterm=NONE
hi sassMixing     ctermfg=219 ctermbg=NONE cterm=NONE
hi sassMixinName  ctermfg=110 ctermbg=NONE cterm=NONE

" JavaScript highlighting
hi javaScript        ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptBraces  ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptNumber  ctermfg=216 ctermbg=NONE cterm=NONE

" Markdown highlighting
hi markdownCode              ctermfg=108 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=108 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=110 ctermbg=NONE cterm=NONE
hi mkdInlineURL              ctermfg=110 ctermbg=NONE cterm=NONE
hi mkdBlockquote             ctermfg=180
hi mkdLineContinue           ctermfg=180

" Git highlighting
hi gitCommitOverflow  ctermfg=174 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=108 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
hi GitGutterAdd           ctermfg=108 ctermbg=237 cterm=NONE
hi GitGutterChange        ctermfg=216 ctermbg=237 cterm=NONE
hi GitGutterDelete        ctermfg=174 ctermbg=237 cterm=NONE
hi GitGutterChangeDelete  ctermfg=219 ctermbg=237 cterm=NONE

" Signify highlighting
hi SignifySignAdd     ctermfg=108 ctermbg=237 cterm=NONE
hi SignifySignChange  ctermfg=110 ctermbg=237 cterm=NONE
hi SignifySignDelete  ctermfg=174 ctermbg=237 cterm=NONE

" NERDTree highlighting
hi NERDTreeDirSlash  ctermfg=110 ctermbg=NONE cterm=NONE
hi NERDTreeExecFile  ctermfg=251 ctermbg=NONE cterm=NONE

" Oliver's random shit
hi vimIsCommand      ctermfg=174
hi vimMapRhs         ctermfg=108
