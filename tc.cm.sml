(*
File for debugging through compilation manger.
*)

structure TC =
struct
    exception InvalidArgument

    

    structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
    structure TigerLex    = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
    structure TigerParser = Join( structure ParserData = TigerLrVals.ParserData
                    structure Lex        = TigerLex
                    structure LrParser   = LrParser
                )

    fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
    val makeFileLexer      = makeTigerLexer o TextIO.openIn

    fun print_error (s, pos1:int, pos2: int) = (ErrorMsg.error pos2 s);
    
    fun printIR x = Printtree.printtree (TextIO.stdOut, x)
    
    fun printCanon [] = ()
        | printCanon (x::xs) = (Printtree.printtree (TextIO.stdOut, x); printCanon xs; ())

    fun printBasicBlocs [] = ()
        | printBasicBlocs (x::xs) = (printCanon x; printBasicBlocs xs)

    fun getIR program = Translate.transProg program

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
end