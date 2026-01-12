" Vim syntax file
" Language: langkit

if exists("b:current_syntax")
  finish
endif

syn keyword langkitKeyword class struct nextgroup=langkitEntity skipwhite
syn keyword langkitKeyword grammar nextgroup=langkitEntity skipwhite
syn keyword langkitKeyword family grammar nextgroup=langkitEntity skipwhite

syn keyword langkitKeyword fun nextgroup=langkitFunction skipwhite

syn match   langkitFunction	"\h\w*" display contained
syn match   langkitEntity	"\h\w*" display contained

syn keyword langkitKeyword as
syn keyword langkitKeyword bind
syn keyword langkitKeyword block
syn keyword langkitKeyword case
syn keyword langkitKeyword do
syn keyword langkitKeyword dynvar try
syn keyword langkitKeyword elif
syn keyword langkitKeyword else
syn keyword langkitKeyword end
syn keyword langkitKeyword field
syn keyword langkitKeyword generic
syn keyword langkitKeyword if
syn keyword langkitKeyword implements
syn keyword langkitKeyword import
syn keyword langkitKeyword in
syn keyword langkitKeyword is
syn keyword langkitKeyword is_a
syn keyword langkitKeyword match
syn keyword langkitKeyword then
syn keyword langkitKeyword trait
syn keyword langkitKeyword val
syn keyword langkitKeyword when

syn keyword langkitLiteral false true
syn keyword langkitQualifier enum memoized parse public qualifier
syn keyword langkitOperator and dont_skip entity new not null or

syn region  langkitString  start=+"+ skip=+\\"+ end=+"+
syn region  langkitChar  start=+'+ skip=+\\"+ end=+'+
syn match   langkitToken   "@\s*\h\%(\w\|\.\)*" display
syn match   langkitGrammarRule   "\%(\w\|_\)\+\s*\(<-\)\@=" display
syn match   langkitLiteral "\d\+" display
syn match   langkitComment "#.*$"
syn match   langkitOperator "<->"
syn match   langkitOperator "<-"
syn match   langkitOperator "?"
syn match   langkitOperator "list+"
syn match   langkitOperator "list\*"
syn match   langkitOperator "/"
syn match   langkitOperator "|"
syn match   langkitOperator "|>"
syn match   langkitLineString +|".*$+

hi def link langkitKeyword     Statement
hi def link langkitQualifier   Identifier
hi def link langkitString      String
hi def link langkitChar        String
hi def link langkitOperator    Special
hi def link langkitFunction	   Define
hi def link langkitEntity	   Function
hi def link langkitToken	   Define
hi def link langkitGrammarRule Define
hi def link langkitLiteral     Number
hi def link langkitLineString  String
hi def link langkitComment     Comment

set comments=b:#,b:\|\" " lkql block strings treated like comments for formatting
set formatoptions+=crno " Automatically wrap, and insert comment lead on newline
