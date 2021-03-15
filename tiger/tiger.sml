structure TC =
struct

structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
			     structure Lex        = TigerLex
			     structure LrParser   = LrParser
			   )

fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerLexer o TextIO.openIn

val thisLexer = case CommandLine.arguments() of
		    []  => makeTigerLexer TextIO.stdIn
		 |  [x] => (makeFileLexer x)
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: tiger file"); OS.Process.exit OS.Process.failure)

fun print_error (s,pos:int,_) = ErrorMsg.error pos s

val (program,_) = TigerParser.parse (0,thisLexer,print_error,()) (* parsing *)
val _           = (PrintAST.print_ast program)

end