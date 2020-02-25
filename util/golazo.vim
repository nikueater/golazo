if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn match golazoType "\<[A-Z][a-zA-Z0-9_']*\>"
syn match golazoForall "\<forall\>"
syn match golazoOperator "=>"
syn match golazoOperator "->"
syn match golazoOperator "::"
syn match golazoOperator "="
syn match golazoOperator "!="
syn match golazoOperator "<"
syn match golazoOperator "<="
syn match golazoOperator ">"
syn match golazoOperator ">="
syn match golazoOperator "&"
syn keyword golazoOperator is in
syn region golazoString start=+"+ skip=+\\"+ end=+"+ contains=@Spell
syn match golazoNumber "\<[1-9]\d*\>" display
syn match golazoHttpMethod "\<\%(get\|post\|put\|patch\|delete\)\>"
syn match golazoControl "\<spec\>"
syn match golazoControl "\<term\>"
syn match golazoControl "\<query\>"
syn match golazoControl "\<pre\>"
syn match golazoControl "\<post\>"
syn match golazoFunction "\<\%(description\|synonyms\|satisfy\|skip\|match\)\>"
syn match golazoLambda "\\"
syn match golazoAnnotation "\v#(.*)$"

" Note: this is a hack to prevent 'keywords' being highlighted
syn match golazoKeywordAsProperty "\%(\.\@1<!\.\)\_s*\%([_[:lower:]][_[:alnum:]]*\)" transparent contains=NONE
syn match golazoObjectKey "[]})\"':]\@1<!\<\%(\h\|[^\x00-\x7F]\)\%(\w\|[^\x00-\x7F]\)*[!?]\=:[[:space:],]\@=" he=e-1

hi def link golazoType Type
hi def link golazoOperator Operator
hi def link golazoAnnotation Comment
hi def link golazoForall Statement
hi def link golazoHttpMethod Statement
hi def link golazoControl Statement
hi def link golazoLambda Special
hi def link golazoString String
hi def link golazoNumber Number
hi def link golazoFunction Function

let b:current_syntax = "golazo"
