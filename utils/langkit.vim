" Vim syntax file
" Language: langkit

if exists("b:current_syntax")
  finish
endif

syn keyword langkitKeyword grammar language class nextgroup=langkitEntity skipwhite
syn keyword langkitKeyword fun nextgroup=langkitFunction skipwhite
syn match   langkitFunction	"\h\w*" display contained
syn match   langkitEntity	"\h\w*" display contained
syn keyword langkitKeyword is end match when field as if elif then else block case let in do bind is_a
syn keyword langkitLiteral false true lfalse ltrue
syn keyword langkitQualifier parse memoized public enum qualifier
syn keyword langkitOperator new entity null or and dont_skip
syn region  langkitString  start=+"+ skip=+""+ end=+"+
syn match   langkitToken   "@\s*\h\%(\w\|\.\)*" display
syn match   langkitGrammarRule   "\%(\w\|_\)\+\s*\(<-\)\@=" display
syn match   langkitLiteral "\d\+" display
syn match   langkitComment "#.*$"
syn match   langkitOperator "<-"
syn match   langkitOperator "?"
syn match   langkitOperator "list+"
syn match   langkitOperator "list\*"
syn match   langkitOperator "/"
syn match   langkitOperator "|"
syn match   langkitOperator "|>"

hi def link langkitKeyword     Statement
hi def link langkitQualifier   Identifier
hi def link langkitString      String
hi def link langkitOperator    Special
hi def link langkitFunction	   Define
hi def link langkitEntity	   Function
hi def link langkitToken	   Define
hi def link langkitGrammarRule Define
hi def link langkitLiteral     Number
hi def link langkitComment     Comment
" hi def link LalstateSloc            Type
" hi def link LalstateCurrentExpr     Identifier
" hi def link LalstateExpr            Identifier
" hi def link LalstateVarName         Function
" hi def link LalstateGenCodeVarName  Constant
" hi def link LalstateValue           String
" hi def link LalstateExprEvalValue   String
