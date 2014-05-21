signature UL_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val SCRIPTS:  'a * 'a -> (svalue,'a) token
val CODEFILES:  'a * 'a -> (svalue,'a) token
val ULFILES:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val SML: (string) *  'a * 'a -> (svalue,'a) token
val LOC: (string) *  'a * 'a -> (svalue,'a) token
val UOFILE: (string) *  'a * 'a -> (svalue,'a) token
val ULFILE: (string) *  'a * 'a -> (svalue,'a) token
end
signature UL_LRVALS=
sig
structure Tokens : UL_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
