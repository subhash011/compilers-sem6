structure TC =
struct
    val pp: bool ref = ref true;
    val toCol: bool ref = ref false;

    structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
    structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
    structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
                    structure Lex        = TigerLex
                    structure LrParser   = LrParser
                )

    fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
    val makeFileLexer      = makeTigerLexer o TextIO.openIn

    fun setFlags () = case CommandLine.arguments() of
                    [] => (toCol := true)
                    | (x::_) => let
                                    val out = (String.isSubstring "p" x, String.isSubstring "a" x, String.isSubstring "c" x)
                                in
                                    (toCol := (#3 out); pp := not (#2 out))
                                end

    val helpString = "usage: ./tc [OPTIONS] file\n\n\
                    \OPTIONS\n\
                    \------\n\
                    \-p: pretty print the input source file\n\
                    \-a: print the ast of the source file\n\
                    \-c: if passed along with -p, it pretty prints the source code with syntax highlighting.\n\
                    \Example usages: \n\
                    \1. ./tc -pc file => pretty print 'file' with syntax highlighting\n\
                    \2. ./tc -a file => print the ast of the 'file'\n\
                    \3. ./tc => live editor in terminal, output is pretty printed code with syntax highlighting\n"

    val thisLexer = case CommandLine.arguments() of
            []  => (makeTigerLexer TextIO.stdIn)
            | ["-h"] => (TextIO.output (TextIO.stdOut, helpString); OS.Process.exit OS.Process.success)
            |   [_] => (makeTigerLexer TextIO.stdIn)
            |   [_, x] => (ErrorMsg.fileName := #file (OS.Path.splitDirFile x); makeFileLexer x)
            |   _   =>   (TextIO.output(TextIO.stdErr, helpString);
                        OS.Process.exit OS.Process.failure)

    fun print_error (s, pos1:int, pos2: int) = (ErrorMsg.error pos2 s);
    val _ = setFlags();
    val (program,_) = TigerParser.parse (0, thisLexer, print_error,  ())
    val _           = if !pp then (PP.compile program (!toCol)) else (PrintAST.print program)
end