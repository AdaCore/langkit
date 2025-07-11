" Vim syntax file
" Language: langkit

if exists("b:current_syntax")
  finish
endif

syn keyword langkitKeyword grammar lexer family language class struct nextgroup=langkitEntity skipwhite
syn keyword langkitKeyword fun nextgroup=langkitFunction skipwhite
syn match   langkitFunction	"\h\w*" display contained
syn match   langkitEntity	"\h\w*" display contained
syn keyword langkitKeyword is end match when field as if elif then else block case val in do bind is_a generic implements trait import dynvar try
syn keyword langkitLiteral false true lfalse ltrue
syn keyword langkitQualifier parse memoized public enum qualifier
syn keyword langkitOperator new entity null or and dont_skip not
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
" hi def link LalstateSloc            Type
" hi def link LalstateCurrentExpr     Identifier
" hi def link LalstateExpr            Identifier
" hi def link LalstateVarName         Function
" hi def link LalstateGenCodeVarName  Constant
" hi def link LalstateValue           String
" hi def link LalstateExprEvalValue   String

set comments=b:#,b:\|\" " lkql block strings treated like comments for formatting
set formatoptions+=crno " Automatically wrap, and insert comment lead on newline
