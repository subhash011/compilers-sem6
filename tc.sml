structure TC =
struct
    
    val fileName = ref "";
    val fmt = ref false;
    val toCol = ref false;
    val ast = ref false;
    val ir = ref true;
    val can = ref false;
    exception InvalidArgument

    

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
                    \--ast: print the ast of the source file\n\
                    \--ir : print the IR of the source file\n\
                    \--can : print the canonised IR of the source file\n\
                    \--fmt: pretty prints the source code without syntax highlighting.\n\
                    \--help: usages for the binary.\n\n\
                    \Example usages: \n\
                    \1. ./tc --pp file => pretty print 'file' with syntax highlighting\n\
                    \2. ./tc --fmt file => pretty print 'file' without syntax highlighting\n\
                    \3. ./tc --ast file => print the ast of the 'file'\n\
                    \4. ./tc --ir file => print the IR of the source in the 'file'\n\
                    \5. ./tc --can file => print the canonised IR of the source in the 'file'\n\
                    \6. ./tc file=> Same as \"./tc --pp file\"\n"

    fun setFlag s =
        case s of
            "--pp" => (fmt := true; toCol := true; ast := false; ir := false; can := false)
            | "--ast" => (fmt := false; toCol := false; ast := true; ir := false; can := false)
            | "--fmt" => (fmt := true; toCol := false; ast := false; ir := false; can := false)
            | "--ir" => (fmt := false; toCol := false; ast := false; ir := true; can := false)
            | "--can" => (fmt := false; toCol := false; ast := false; ir := false; can := true)
            | _ => raise InvalidArgument

    fun setFlags [] = true
        | setFlags (x::xs) =   case x of
                                "--pp" => (setFlag x; setFlags xs)
                                | "--ast" => (setFlag x; setFlags xs)
                                | "--fmt" => (setFlag x; setFlags xs)
                                | "--ir" => (setFlag x; setFlags xs)
                                | "--can" => (setFlag x; setFlags xs)
                                | str => (if !fileName = "" then (fileName := str; setFlags xs) else false)

    fun failExit() =    (TextIO.output(TextIO.stdErr, helpString); 
                        OS.Process.exit OS.Process.failure)

    val thisLexer = case CommandLine.arguments() of
            []  => (makeTigerLexer TextIO.stdIn)
            | ["--help"] => (TextIO.output (TextIO.stdOut, helpString); OS.Process.exit OS.Process.success)
            | ["--ast"] => (setFlag "--ast"; makeTigerLexer TextIO.stdIn)
            | ["--pp"] => (setFlag "--pp"; makeTigerLexer TextIO.stdIn)
            | ["--fmt"] => (setFlag "--fmt"; makeTigerLexer TextIO.stdIn)
            | ["--ir"] => (setFlag "--ir"; makeTigerLexer TextIO.stdIn)
            | ["--can"] => (setFlag "--can"; makeTigerLexer TextIO.stdIn)
            |   [x]      => (setFlag "--ir"; makeFileLexer x)
            |   ls =>   let
                            val parseSuccess = setFlags ls
                            val file = !fileName
                        in
                            if parseSuccess
                            then (ErrorMsg.fileName := #file (OS.Path.splitDirFile file); makeFileLexer (file))
                            else failExit()

                        end

    fun print_error (s, pos1:int, pos2: int) = (ErrorMsg.error pos2 s);
    
    fun printIR x = Printtree.printtree (TextIO.stdOut, x)
    
    fun printCanon [] = ()
        | printCanon (x::xs) = (Printtree.printtree (TextIO.stdOut, x); printCanon xs; ())

    fun printBasicBlocs [] = ()
        | printBasicBlocs (x::xs) = (printCanon x; printBasicBlocs xs)

    fun getIR program = Translate.translate program

    fun getCanon program =
        let
            val ir = getIR program
            val linearised = Canon.linearize ir
            val basicBlocks = Canon.basicBlocks linearised
            val traceSchedule = Canon.traceSchedule basicBlocks
        in
            (* basicBlocks *)
            (* linearised *)
            traceSchedule
        end

    fun getIRCM () = 
        let
            val (program,_) = TigerParser.parse (0, makeTigerLexer TextIO.stdIn, print_error, ());
        in
            printIR (getIR program)
        end
    
    fun getCanonCM () = 
        let
            val (program,_) = TigerParser.parse (0, makeTigerLexer TextIO.stdIn, print_error, ());
            (* val (basicBlocks, label) = getCanon program *)
        in
            (* (printBasicBlocs basicBlocks; print (Symbol.name label)) *)
            printCanon (getCanon program)
        end

    fun CM s =
        case s of
            "can" => getCanonCM ()
            | "ir" => getIRCM ()
            | "ast" =>
                let
                    val (program, _) = TigerParser.parse (0, makeTigerLexer TextIO.stdIn, print_error, ())
                in
                    PrintAST.print program
                end
            | _ => raise InvalidArgument

    val (program,_) = TigerParser.parse (0, thisLexer, print_error, ());

    val _   =   if !fmt 
            then (PP.compile program (!toCol); ()) 
            else if !ast
            then (PrintAST.print program; ())
            else if !ir
            then printIR (getIR program)
            else if !can
            then printCanon (getCanon program)
            else ()
end