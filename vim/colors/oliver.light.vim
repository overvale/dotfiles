hi clear
syntax reset
let g:colors_name = "oliver.light"

" This scheme is designed to be used with the
" 'oliver.light' terminal color scheme.
" It uses the ANSI codes for colors
" and 256 codes for shades of gray.

hi User1         ctermfg=015     ctermbg=001     cterm=NONE
hi User2         ctermfg=015     ctermbg=002     cterm=NONE
hi User3         ctermfg=015     ctermbg=003     cterm=NONE
hi User4         ctermfg=015     ctermbg=004     cterm=NONE
hi User5         ctermfg=015     ctermbg=005     cterm=NONE
hi User6         ctermfg=015     ctermbg=006     cterm=NONE
hi User7         ctermfg=015     ctermbg=007     cterm=NONE
hi User8         ctermfg=015     ctermbg=008     cterm=NONE
hi User9         ctermfg=015     ctermbg=009     cterm=NONE

" The Basics
" ---------------------------------------------------------
hi Normal        ctermfg=000     ctermbg=015     cterm=NONE
hi Cursor        ctermfg=015     ctermbg=236     cterm=NONE
hi Visual        ctermfg=000     ctermbg=003     cterm=NONE
    
" The Structure    
" ---------------------------------------------------------
hi StatusLine    ctermfg=015     ctermbg=008     cterm=NONE
hi TabLineSel    ctermfg=015     ctermbg=008     cterm=NONE

hi StatusLineNC  ctermfg=008     ctermbg=253     cterm=NONE
hi TabLine       ctermfg=247     ctermbg=253     cterm=NONE
hi TabLineFill   ctermfg=000     ctermbg=253     cterm=NONE
hi VertSplit     ctermfg=253     ctermbg=253     cterm=NONE
hi LineNr        ctermfg=250     ctermbg=255     cterm=NONE
hi CursorLineNr  ctermfg=004     ctermbg=255     cterm=NONE
hi SignColumn    ctermfg=251     ctermbg=255     cterm=NONE

hi CursorLine    ctermfg=NONE    ctermbg=011     cterm=NONE
hi CursorColumn  ctermfg=NONE    ctermbg=011     cterm=NONE
hi ColorColumn   ctermfg=NONE    ctermbg=255     cterm=NONE
    
hi PMenu         ctermfg=000     ctermbg=007     cterm=NONE
hi PMenuSel      ctermfg=015     ctermbg=003     cterm=NONE
hi PmenuSbar     ctermbg=NONE    ctermfg=NONE    cterm=NONE
hi PmenuThumb    ctermbg=NONE    ctermfg=NONE    cterm=NONE
    
" Misc    
" ---------------------------------------------------------
hi FoldColumn    ctermfg=250    ctermbg=255     cterm=NONE
hi Folded        ctermfg=007     ctermbg=NONE     cterm=NONE
hi IncSearch     ctermfg=000     ctermbg=003     cterm=NONE

hi MatchParen    ctermfg=NONE    ctermbg=009     cterm=NONE
hi ModeMsg       ctermfg=015     ctermbg=002    cterm=NONE
hi MoreMsg       ctermfg=006     ctermbg=NONE    cterm=NONE
hi Question      ctermfg=001     ctermbg=NONE    cterm=NONE
hi Search        ctermfg=244     ctermbg=003     cterm=NONE
hi Error         ctermfg=015     ctermbg=001     cterm=NONE

hi ErrorMsg      ctermfg=015     ctermbg=001     cterm=NONE
hi Macro         ctermfg=001     ctermbg=NONE    cterm=NONE
hi VisualNOS     ctermfg=001     ctermbg=NONE    cterm=NONE
hi WarningMsg    ctermfg=001     ctermbg=NONE    cterm=NONE
hi WildMenu      ctermfg=001     ctermbg=NONE    cterm=NONE



" Standard syntax highlighting
" ---------------------------------------------------------
hi Character    ctermfg=001  ctermbg=NONE  cterm=NONE
hi Debug        ctermfg=001  ctermbg=NONE  cterm=NONE
hi Exception    ctermfg=001  ctermbg=NONE  cterm=NONE
hi Identifier   ctermfg=001  ctermbg=NONE  cterm=NONE
hi TooLong      ctermfg=001  ctermbg=NONE  cterm=NONE
hi Float        ctermfg=001  ctermbg=NONE  cterm=NONE
hi Special      ctermfg=001  ctermbg=NONE  cterm=NONE
hi SpecialChar  ctermfg=001  ctermbg=NONE  cterm=NONE
hi SpecialKey   ctermfg=001  ctermbg=NONE  cterm=NONE
hi StorageClass ctermfg=001  ctermbg=NONE  cterm=NONE
hi Boolean      ctermfg=001  ctermbg=NONE  cterm=NONE

hi String       ctermfg=002  ctermbg=NONE  cterm=NONE

hi Constant     ctermfg=003  ctermbg=NONE  cterm=NONE
hi Label        ctermfg=003  ctermbg=NONE  cterm=NONE
hi PreProc      ctermfg=003  ctermbg=NONE  cterm=NONE
hi Repeat       ctermfg=003  ctermbg=NONE  cterm=NONE
hi Tag          ctermfg=003  ctermbg=NONE  cterm=NONE
hi Todo         ctermfg=003  ctermbg=NONE  cterm=NONE
hi Typedef      ctermfg=003  ctermbg=NONE  cterm=NONE

hi Conceal      ctermfg=004  ctermbg=NONE  cterm=NONE
hi Delimiter    ctermfg=004  ctermbg=NONE  cterm=NONE
hi Directory    ctermfg=004  ctermbg=NONE  cterm=NONE
hi Function     ctermfg=004  ctermbg=NONE  cterm=NONE
hi Include      ctermfg=004  ctermbg=NONE  cterm=NONE
hi Statement    ctermfg=004  ctermbg=NONE  cterm=NONE

hi Conditional  ctermfg=005  ctermbg=NONE  cterm=NONE
hi Define       ctermfg=005  ctermbg=NONE  cterm=NONE
hi Keyword      ctermfg=005  ctermbg=NONE  cterm=NONE
hi Number       ctermfg=005  ctermbg=NONE  cterm=NONE
hi Structure    ctermfg=005  ctermbg=NONE  cterm=NONE

hi Type         ctermfg=006  ctermbg=NONE  cterm=NONE


hi Title        ctermfg=000  ctermbg=NONE  cterm=NONE
hi Comment      ctermfg=008  ctermbg=NONE  cterm=NONE
hi NonText      ctermfg=007  ctermbg=NONE  cterm=NONE
hi Operator     ctermfg=008  ctermbg=NONE  cterm=NONE


" Spelling highlighting
" ---------------------------------------------------------
hi SpellBad     ctermfg=NONE ctermbg=009 
hi SpellLocal   ctermfg=NONE ctermbg=009  cterm=NONE
hi SpellCap     ctermfg=NONE ctermbg=010  cterm=NONE
hi SpellRare    ctermfg=NONE ctermbg=011  cterm=NONE


" diff highlighting
" ---------------------------------------------------------
hi DiffAdd      ctermfg=002  ctermbg=NONE  cterm=NONE
hi DiffChange   ctermfg=004  ctermbg=NONE  cterm=NONE
hi DiffDelete   ctermfg=001  ctermbg=NONE  cterm=NONE
hi DiffText     ctermfg=004  ctermbg=NONE  cterm=NONE
hi DiffAdded    ctermfg=002  ctermbg=NONE  cterm=NONE
hi DiffFile     ctermfg=001  ctermbg=NONE  cterm=NONE
hi DiffNewFile  ctermfg=002  ctermbg=NONE  cterm=NONE
hi DiffLine     ctermfg=004  ctermbg=NONE  cterm=NONE
hi DiffRemoved  ctermfg=001  ctermbg=NONE  cterm=NONE


" Ruby highlighting
" ---------------------------------------------------------
hi rubyAttribute               ctermfg=004   ctermbg=NONE   cterm=NONE
hi rubyConstant                ctermfg=003   ctermbg=NONE   cterm=NONE
hi rubyInterpolation           ctermfg=002   ctermbg=NONE   cterm=NONE
hi rubyInterpolationDelimiter  ctermfg=003   ctermbg=NONE   cterm=NONE
hi rubyRegexp                  ctermfg=001   ctermbg=NONE   cterm=NONE
hi rubyRegexpSpecial           ctermfg=004    
hi rubyRegexpParens            ctermfg=001    
hi rubyStringEscape            ctermfg=005    
hi rubySymbol                  ctermfg=001   ctermbg=NONE   cterm=NONE
hi rubyStringDelimiter         ctermfg=002   ctermbg=NONE   cterm=NONE
hi rubyDoBlock                 ctermfg=003    

" HTML highlighting
" ---------------------------------------------------------
hi htmlBold                     ctermfg=005  ctermbg=NONE cterm=NONE
hi htmlItalic                   ctermfg=001  ctermbg=NONE cterm=NONE
hi htmlArg                      ctermfg=005  ctermbg=NONE cterm=NONE
hi htmlEndTag                   ctermfg=004  ctermbg=NONE cterm=NONE
hi htmlTag                      ctermfg=004  ctermbg=NONE cterm=NONE
hi link htmlTagN htmlTagName
hi htmlTagName                  ctermfg=004  ctermbg=012  cterm=NONE
hi htmlLink                     ctermfg=NONE ctermbg=NONE cterm=NONE
hi htmlH1                       ctermfg=001  ctermbg=009  cterm=NONE
hi htmlH2                       ctermfg=004  ctermbg=012  cterm=NONE
hi htmlH3                       ctermfg=005  ctermbg=013  cterm=NONE
hi htmlH4                       ctermfg=002  ctermbg=010  cterm=NONE

" CSS highlighting
" ---------------------------------------------------------
hi cssBraces      ctermfg=000 ctermbg=NONE cterm=NONE
hi cssClassName   ctermfg=005 ctermbg=NONE cterm=NONE
hi cssColor       ctermfg=006 ctermbg=NONE cterm=NONE

" Markdown highlighting
" ---------------------------------------------------------
hi markdownCode              ctermfg=006 ctermbg=NONE cterm=NONE
hi markdownCodeBlock         ctermfg=006 ctermbg=NONE cterm=NONE
hi mkdIndentCode             ctermfg=006 ctermbg=NONE cterm=NONE
hi markdownHeadingDelimiter  ctermfg=004 ctermbg=NONE cterm=NONE
hi mkdInlineURL              ctermfg=004 ctermbg=NONE cterm=NONE
hi mkdBlockquote             ctermfg=002
hi mkdLineContinue           ctermfg=002

" Git highlighting
" ---------------------------------------------------------
hi gitCommitOverflow  ctermfg=001 ctermbg=NONE cterm=NONE
hi gitCommitSummary   ctermfg=002 ctermbg=NONE cterm=NONE
  
" GitGutter highlighting
" ---------------------------------------------------------
hi GitGutterAdd           ctermfg=002 ctermbg=255 cterm=NONE
hi GitGutterChange        ctermfg=003 ctermbg=255 cterm=NONE
hi GitGutterDelete        ctermfg=001 ctermbg=255 cterm=NONE
hi GitGutterChangeDelete  ctermfg=001 ctermbg=255 cterm=NONE

" Oliver's random shit
" ---------------------------------------------------------
hi vimIsCommand      ctermfg=001
hi vimMapRhs         ctermfg=002
