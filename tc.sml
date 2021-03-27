structure TC =
struct
    val output: int ref = ref 0;
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
            |  ["-P", x] => (ErrorMsg.fileName := #file (OS.Path.splitDirFile x); makeFileLexer x)
            |  ["-A", x] => (ErrorMsg.fileName := #file (OS.Path.splitDirFile x); output := 1; makeFileLexer x)
            |  _   =>   (TextIO.output
                            (TextIO.stdErr,
                                "usage: ./tc [OPTIONS] file\n\n\
                                \OPTIONS\n\
                                \------\n\
                                \-P: pretty print the input source file\n\
                                \-A: print the ast of the source file\n"
                            );
                        OS.Process.exit OS.Process.failure)

    fun print_error (s, pos1:int, pos2: int) = (ErrorMsg.error pos2 s);

    val (program,_) = TigerParser.parse (0, thisLexer, print_error,  ())
    val _           = if !output = 0 then (PP.compile program) else (PrintAST.print program)

end