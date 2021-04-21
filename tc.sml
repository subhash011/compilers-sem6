structure TC =
struct
    
    val fileName = ref "";
    val fmt = ref true;
    val toCol = ref true;
    val ast = ref false;

    val flags = {fmt = ref true, toCol = ref true, ast = ref false};

    

    structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
    structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
    structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
                    structure Lex        = TigerLex
                    structure LrParser   = LrParser
                )

    fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
    val makeFileLexer      = makeTigerLexer o TextIO.openIn

    val helpString = "usage: ./tc [OPTIONS] file\n\n\
                    \OPTIONS\n\
                    \------\n\
                    \--pp: pretty print the input source file\n\
                    \--ast: print the ast of the source file\n\
                    \--fmt: pretty prints the source code without syntax highlighting.\n\
                    \--help: usages for the binary.\n\n\
                    \Example usages: \n\
                    \1. ./tc --pp file => pretty print 'file' with syntax highlighting\n\
                    \2. ./tc --fmt file => pretty print 'file' without syntax highlighting\n\
                    \3. ./tc --ast file => print the ast of the 'file'\n\
                    \4. ./tc file=> Same as \"./tc --pp file\"\n"

    fun setFlags [] = true
        | setFlags (x::xs) =   case x of
                                "--pp" => (fmt := true; toCol := true; ast := false; setFlags xs)
                                | "--ast" => (fmt := false; toCol := false; ast := true; setFlags xs)
                                | "--fmt" => (fmt := true; toCol := false; ast := false; setFlags xs)
                                | str => (if !fileName = "" then (fileName := str; setFlags xs) else false)

    fun failExit() =    (TextIO.output(TextIO.stdErr, helpString); 
                        OS.Process.exit OS.Process.failure)

    val thisLexer = case CommandLine.arguments() of
            []  => (makeTigerLexer TextIO.stdIn)
            | ["--help"] => (TextIO.output (TextIO.stdOut, helpString); OS.Process.exit OS.Process.success)
            |   [x] => (ErrorMsg.fileName := #file (OS.Path.splitDirFile x); makeFileLexer x)
            |   ls =>   let
                            val parseSuccess = setFlags ls
                            val file = !fileName
                        in
                            if parseSuccess
                            then (ErrorMsg.fileName := #file (OS.Path.splitDirFile file); makeFileLexer (file))
                            else failExit()
                        end

    fun print_error (s, pos1:int, pos2: int) = (ErrorMsg.error pos2 s);
    val (program,_) = TigerParser.parse (0, thisLexer, print_error,  ());
    val _           =   if !fmt 
                        then (PP.compile program (!toCol); ()) 
                        else if !ast
                        then (PrintAST.print program; ())
                        else ()
end