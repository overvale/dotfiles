hi clear
syntax reset
let g:colors_name = "oliver-ocean.dark"

hi Normal        ctermfg=251   ctermbg=236   cterm=NONE

" Vim editor colors
hi Bold          ctermfg=NONE  ctermbg=NONE  cterm=bold
hi Debug         ctermfg=161   ctermbg=NONE  cterm=NONE
hi Directory     ctermfg=067   ctermbg=NONE  cterm=NONE
hi ErrorMsg      ctermfg=161   ctermbg=236   cterm=NONE
hi Exception     ctermfg=161   ctermbg=NONE  cterm=NONE
hi FoldColumn    ctermfg=NONE  ctermbg=237   cterm=NONE
hi Folded        ctermfg=239   ctermbg=237   cterm=NONE
hi IncSearch     ctermfg=237   ctermbg=173   cterm=NONE
hi Italic        ctermfg=NONE  ctermbg=NONE  cterm=NONE
hi Macro         ctermfg=161   ctermbg=NONE  cterm=NONE
hi MatchParen    ctermfg=236   ctermbg=242   cterm=NONE
hi ModeMsg       ctermfg=222   ctermbg=NONE  cterm=NONE
hi MoreMsg       ctermfg=108   ctermbg=NONE  cterm=NONE
hi Question      ctermfg=222   ctermbg=NONE  cterm=NONE
hi Search        ctermfg=242   ctermbg=222   cterm=NONE
hi SpecialKey    ctermfg=242   ctermbg=NONE  cterm=NONE
hi TooLong       ctermfg=161   ctermbg=NONE  cterm=NONE
hi Underlined    ctermfg=161   ctermbg=NONE  cterm=NONE
hi Visual        ctermfg=NONE  ctermbg=239   cterm=NONE
hi VisualNOS     ctermfg=161   ctermbg=NONE  cterm=NONE
hi WarningMsg    ctermfg=161   ctermbg=NONE  cterm=NONE
hi WildMenu      ctermfg=161   ctermbg=NONE  cterm=NONE
hi Title         ctermfg=255   ctermbg=NONE  cterm=NONE
hi Conceal       ctermfg=067   ctermbg=236   cterm=NONE
hi Cursor        ctermfg=236   ctermbg=255   cterm=NONE
hi NonText       ctermfg=242   ctermbg=NONE  cterm=NONE
hi LineNr        ctermfg=242   ctermbg=237   cterm=NONE
hi SignColumn    ctermfg=242   ctermbg=237   cterm=NONE
hi SpecialKey    ctermfg=242   ctermbg=NONE  cterm=NONE
hi StatusLine    ctermfg=236   ctermbg=251   cterm=NONE
hi StatusLineNC  ctermfg=235   ctermbg=239   cterm=NONE
hi VertSplit     ctermfg=239   ctermbg=239   cterm=NONE
hi ColorColumn   ctermfg=NONE  ctermbg=237   cterm=NONE
hi CursorColumn  ctermfg=NONE  ctermbg=237   cterm=NONE
hi CursorLine    ctermfg=NONE  ctermbg=235   cterm=NONE
hi CursorLineNr  ctermfg=108   ctermbg=235   cterm=NONE
hi PMenu         ctermfg=236   ctermbg=251   cterm=NONE
hi PMenuSel      ctermfg=237   ctermbg=222   cterm=NONE
hi TabLine       ctermfg=255   ctermbg=239   cterm=NONE
hi TabLineFill   ctermfg=255   ctermbg=239   cterm=NONE
hi TabLineSel    ctermfg=236   ctermbg=251   cterm=NONE

" Standard syntax highlighting
hi Boolean       ctermfg=173   ctermbg=NONE  cterm=NONE
hi Character     ctermfg=161   ctermbg=NONE  cterm=NONE
hi Comment       ctermfg=242   ctermbg=NONE  cterm=NONE
hi Conditional   ctermfg=133   ctermbg=NONE  cterm=NONE
hi Constant      ctermfg=173   ctermbg=NONE  cterm=NONE
hi Define        ctermfg=133   ctermbg=NONE  cterm=NONE
hi Delimiter     ctermfg=130   ctermbg=NONE  cterm=NONE
hi Float         ctermfg=173   ctermbg=NONE  cterm=NONE
hi Function      ctermfg=067   ctermbg=NONE  cterm=NONE
hi Identifier    ctermfg=161   ctermbg=NONE  cterm=NONE
hi Include       ctermfg=067   ctermbg=NONE  cterm=NONE
hi Keyword       ctermfg=133   ctermbg=NONE  cterm=NONE
hi Label         ctermfg=222   ctermbg=NONE  cterm=NONE
hi Number        ctermfg=173   ctermbg=NONE  cterm=NONE
hi Operator      ctermfg=251   ctermbg=NONE  cterm=NONE
hi PreProc       ctermfg=222   ctermbg=NONE  cterm=NONE
hi Repeat        ctermfg=222   ctermbg=NONE  cterm=NONE
hi Special       ctermfg=109   ctermbg=NONE  cterm=NONE
hi SpecialChar   ctermfg=130   ctermbg=NONE  cterm=NONE
hi Statement     ctermfg=161   ctermbg=NONE  cterm=NONE
hi StorageClass  ctermfg=222   ctermbg=NONE  cterm=NONE
hi String        ctermfg=108   ctermbg=NONE  cterm=NONE
hi Structure     ctermfg=133   ctermbg=NONE  cterm=NONE
hi Tag           ctermfg=222   ctermbg=NONE  cterm=NONE
hi Todo          ctermfg=222   ctermbg=237   cterm=NONE
hi Type          ctermfg=173   ctermbg=NONE  cterm=NONE
hi Typedef       ctermfg=222   ctermbg=NONE  cterm=NONE

" Spelling highlighting
hi SpellBad     ctermfg=NONE   ctermbg=NONE  cterm=underline
hi SpellLocal   ctermfg=NONE   ctermbg=NONE  cterm=underline
hi SpellCap     ctermfg=NONE   ctermbg=NONE  cterm=underline
hi SpellRare    ctermfg=NONE   ctermbg=NONE  cterm=underline

" Additional diff highlighting
hi DiffAdd      ctermfg=108 ctermbg=236 cterm=NONE
hi DiffChange   ctermfg=067  ctermbg=236 cterm=NONE
hi DiffDelete   ctermfg=161   ctermbg=236 cterm=NONE
hi DiffText     ctermfg=067  ctermbg=236 cterm=NONE
hi DiffAdded    ctermfg=108 ctermbg=236 cterm=NONE
hi DiffFile     ctermfg=161   ctermbg=236 cterm=NONE
hi DiffNewFile  ctermfg=108 ctermbg=236 cterm=NONE
hi DiffLine     ctermfg=067  ctermbg=236 cterm=NONE
hi DiffRemoved  ctermfg=161   ctermbg=236 cterm=NONE

" Ruby highlighting
hi rubyAttribute               ctermfg=067 ctermbg=NONE cterm=NONE
hi rubyConstant                ctermfg=222 ctermbg=NONE cterm=NONE
hi rubyInterpolation           ctermfg=108 ctermbg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=130 ctermbg=NONE cterm=NONE
hi rubyRegexp                  ctermfg=109 ctermbg=NONE cterm=NONE
hi rubySymbol                  ctermfg=108 ctermbg=NONE cterm=NONE
hi rubyStringDelimiter         ctermfg=108 ctermbg=NONE cterm=NONE

" PHP highlighting
hi phpMemberSelector  ctermfg=251 ctermbg=NONE cterm=NONE
hi phpComparison      ctermfg=251 ctermbg=NONE cterm=NONE
hi phpParent          ctermfg=251 ctermbg=NONE cterm=NONE

" HTML highlighting
hi htmlBold    ctermfg=222 ctermbg=NONE cterm=NONE
hi htmlItalic  ctermfg=109 ctermbg=NONE cterm=NONE
hi htmlEndTag  ctermfg=251 ctermbg=NONE cterm=NONE
hi htmlTag     ctermfg=251 ctermbg=NONE cterm=NONE

" CSS highlighting
hi cssBraces      ctermfg=251 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=133 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=109 ctermbg=NONE cterm=NONE

" SASS highlighting
hi sassidChar     ctermfg=161 ctermbg=NONE cterm=NONE
hi sassClassChar  ctermfg=173 ctermbg=NONE cterm=NONE
hi sassInclude    ctermfg=133 ctermbg=NONE cterm=NONE
hi sassMixing     ctermfg=133 ctermbg=NONE cterm=NONE
hi sassMixinName  ctermfg=067 ctermbg=NONE cterm=NONE

" JavaScript highlighting
hi javaScript        ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptBraces  ctermfg=251 ctermbg=NONE cterm=NONE
hi javaScriptNumber  ctermfg=173 ctermbg=NONE cterm=NONE

" Markdown highlighting
hi markdownCode              ctermfg=108 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=108 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=067 ctermbg=NONE cterm=NONE
hi htmlH1                    ctermfg=133 ctermbg=NONE cterm=NONE
hi htmlH2                    ctermfg=222 ctermbg=NONE cterm=NONE

" Git highlighting
hi gitCommitOverflow  ctermfg=161 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=108 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
hi GitGutterAdd           ctermfg=108  ctermbg=237 cterm=NONE
hi GitGutterChange        ctermfg=067   ctermbg=237 cterm=NONE
hi GitGutterDelete        ctermfg=161    ctermbg=237 cterm=NONE
hi GitGutterChangeDelete  ctermfg=133 ctermbg=237 cterm=NONE

" Signify highlighting
hi SignifySignAdd     ctermfg=108 ctermbg=237 cterm=NONE
hi SignifySignChange  ctermfg=067  ctermbg=237 cterm=NONE
hi SignifySignDelete  ctermfg=161   ctermbg=237 cterm=NONE

" NERDTree highlighting
hi NERDTreeDirSlash  ctermfg=067 ctermbg=NONE cterm=NONE
hi NERDTreeExecFile  ctermfg=251 ctermbg=NONE cterm=NONE
