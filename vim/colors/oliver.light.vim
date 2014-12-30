hi clear
syntax reset
let g:colors_name="oliver.light"

" Text Colors
"  Brown   136
"  Orange  208
"  Green   34
"  Cyan    37
"  Blue    32
"  Purple  92
"  Pink    205
"  Red     196

" Background Colors
"  Light-Brown   187
"  Light-Yellow  230
"  Light-Green   194
"  Light-Cyan    195
"  Light-Blue    189
"  Light-Purple  225
"  Light-Red     224

hi Normal             ctermbg=231   ctermfg=237   cterm=NONE
hi Cursor             ctermbg=33    ctermfg=07    cterm=NONE

hi Bold               ctermbg=NONE  ctermfg=NONE  cterm=bold
hi Boolean            ctermbg=NONE  ctermfg=032   cterm=NONE
hi Character          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi ColorColumn        ctermbg=252   ctermfg=NONE  cterm=NONE
hi Comment            ctermbg=NONE  ctermfg=248   cterm=NONE
hi Conceal            ctermbg=NONE  ctermfg=250   cterm=NONE
hi Conditional        ctermbg=NONE  ctermfg=092   cterm=NONE
hi Constant           ctermbg=NONE  ctermfg=32   cterm=NONE
hi CursorColumn       ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Debug              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Define             ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Delimiter          ctermbg=NONE  ctermfg=172   cterm=NONE
hi Directory          ctermbg=NONE  ctermfg=32    cterm=NONE
hi Error              ctermbg=196   ctermfg=15    cterm=NONE
hi ErrorMsg           ctermbg=196   ctermfg=15    cterm=NONE
hi Exception          ctermbg=NONE  ctermfg=092   cterm=NONE
hi Float              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi FoldColumn         ctermbg=NONE  ctermfg=250   cterm=NONE
hi Folded             ctermbg=NONE   ctermfg=250    cterm=NONE
hi Function           ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Identifier         ctermbg=NONE  ctermfg=196   cterm=NONE
hi Ignore             ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Include            ctermbg=NONE  ctermfg=196  cterm=NONE
hi IncSearch          ctermbg=214   ctermfg=015   cterm=NONE
hi Italic             ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Keyword            ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Label              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi lCursor            ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Macro              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi MatchParen         ctermbg=254   ctermfg=NONE  cterm=NONE
hi ModeMsg            ctermbg=32  ctermfg=231    cterm=NONE
hi MoreMsg            ctermbg=NONE  ctermfg=93    cterm=NONE
hi NonText            ctermbg=NONE  ctermfg=250   cterm=NONE
hi Number             ctermbg=NONE   ctermfg=092   cterm=NONE
hi Operator           ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi PreProc            ctermbg=NONE  ctermfg=092   cterm=NONE
hi Question           ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Repeat             ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Search             ctermbg=228   ctermfg=NONE  cterm=NONE
hi Special            ctermbg=NONE   ctermfg=32   cterm=NONE
hi SpecialChar        ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi SpecialKey         ctermbg=NONE  ctermfg=250   cterm=NONE
hi Statement          ctermbg=NONE   ctermfg=196   cterm=NONE
hi StorageClass       ctermbg=NONE  ctermfg=136   cterm=NONE
hi String             ctermbg=NONE  ctermfg=034   cterm=NONE
hi Structure          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Tag                ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Title              ctermbg=NONE   ctermfg=37    cterm=NONE
hi Todo               ctermbg=NONE   ctermfg=196   cterm=NONE
hi TooLong            ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Type               ctermbg=NONE  ctermfg=32    cterm=NONE
hi Typedef            ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi Underlined         ctermbg=NONE  ctermfg=NONE  cterm=underline
hi WarningMsg         ctermbg=208   ctermfg=231   cterm=NONE
hi WildMenu           ctermbg=230   ctermfg=196   cterm=NONE

hi VertSplit          ctermbg=252   ctermfg=252   cterm=NONE
hi StatusLine         ctermbg=245   ctermfg=255   cterm=NONE
hi StatusLineNC       ctermbg=252   ctermfg=245   cterm=NONE
hi TabLine            ctermbg=252   ctermfg=245   cterm=NONE
hi TabLineFill        ctermbg=252   ctermfg=245   cterm=NONE
hi TabLineSel         ctermbg=245   ctermfg=255   cterm=NONE
hi SignColumn         ctermbg=255   ctermfg=245   cterm=NONE

hi LineNr             ctermbg=255  ctermfg=251   cterm=NONE
hi CursorLine         ctermbg=255  ctermfg=NONE  cterm=NONE
hi CursorLineNr       ctermbg=255  ctermfg=245   cterm=NONE
hi Visual             ctermbg=194   ctermfg=NONE  cterm=NONE
hi VisualNOS          ctermbg=194   ctermfg=NONE  cterm=NONE

hi SpellBad           ctermbg=NONE  ctermfg=NONE  cterm=underline
hi SpellCap           ctermbg=NONE  ctermfg=NONE  cterm=underline
hi SpellLocal         ctermbg=NONE  ctermfg=NONE  cterm=underline
hi SpellRare          ctermbg=NONE  ctermfg=NONE  cterm=underline

hi Pmenu              ctermbg=252   ctermfg=NONE  cterm=NONE
hi PmenuSel           ctermbg=227   ctermfg=NONE  cterm=NONE
hi PmenuSbar          ctermbg=248   ctermfg=NONE  cterm=NONE
hi PmenuThumb         ctermbg=252   ctermfg=NONE  cterm=NONE

hi SignifySignAdd     ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi SignifySignChange  ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi SignifySignDelete  ctermbg=NONE  ctermfg=NONE  cterm=NONE

hi cssBraces          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi cssClassName       ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi cssColor           ctermbg=NONE  ctermfg=NONE  cterm=NONE

hi DiffAdd            ctermbg=120   ctermfg=NONE  cterm=NONE
hi DiffAdded          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi DiffChange         ctermbg=223   ctermfg=NONE  cterm=NONE
hi DiffDelete         ctermbg=175   ctermfg=NONE  cterm=NONE
hi DiffFile           ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi DiffLine           ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi DiffNewFile        ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi DiffRemoved        ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi DiffText           ctermbg=222   ctermfg=15    cterm=NONE

hi gitCommitOverflow      ctermbg=NONE ctermfg=NONE cterm=NONE
hi gitCommitSummary       ctermbg=NONE ctermfg=NONE cterm=NONE
hi GitGutterAdd           ctermbg=255 ctermfg=034  cterm=NONE
hi GitGutterChange        ctermbg=255 ctermfg=208  cterm=NONE
hi GitGutterChangeDelete  ctermbg=255 ctermfg=092  cterm=NONE
hi GitGutterDelete        ctermbg=255 ctermfg=196  cterm=NONE

hi link htmlTagN htmlTagName
hi htmlBold               ctermbg=NONE ctermfg=NONE cterm=bold
hi htmlTag                ctermbg=NONE ctermfg=020  cterm=NONE
hi htmlEndTag             ctermbg=NONE ctermfg=020  cterm=NONE
hi htmlH1                 ctermbg=NONE ctermfg=196 cterm=NONE
hi htmlH2                 ctermbg=NONE ctermfg=32   cterm=NONE
hi htmlItalic             ctermbg=NONE ctermfg=NONE cterm=NONE

hi javaScript             ctermbg=NONE ctermfg=NONE cterm=NONE
hi javaScriptBraces       ctermbg=NONE ctermfg=NONE cterm=NONE
hi javaScriptNumber       ctermbg=NONE ctermfg=NONE cterm=NONE

hi mkdDelimiter                ctermbg=NONE ctermfg=032  cterm=NONE
hi markdownCode                ctermbg=NONE ctermfg=NONE cterm=NONE
hi markdownCodeBlock           ctermbg=NONE ctermfg=NONE cterm=NONE
hi markdownHeadingDelimiter    ctermbg=NONE ctermfg=NONE cterm=NONE

hi phpComparison               ctermbg=NONE ctermfg=NONE cterm=NONE
hi phpMemberSelector           ctermbg=NONE ctermfg=NONE cterm=NONE
hi phpParent                   ctermbg=NONE ctermfg=NONE cterm=NONE

hi rubyAttribute               ctermbg=NONE ctermfg=NONE cterm=NONE
hi rubyConstant                ctermbg=NONE ctermfg=NONE cterm=NONE
hi rubyInterpolation           ctermbg=NONE ctermfg=NONE cterm=NONE
hi rubyInterpolationDelimiter  ctermbg=NONE ctermfg=NONE cterm=NONE
hi rubyRegexp                  ctermbg=NONE ctermfg=NONE cterm=NONE
hi rubyStringDelimiter         ctermbg=NONE ctermfg=NONE cterm=NONE
hi rubySymbol                  ctermbg=NONE ctermfg=NONE cterm=NONE
