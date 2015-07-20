structure ParseAbtTest =
struct
  datatype oper = LAM | AX | AP

  structure O : PARSE_OPERATOR =
  struct
    type world = unit
    type t = oper
    val eq = op=
    fun arity LAM = #[1]
      | arity AX = #[]
      | arity AP = #[0,0]
    fun toString LAM = "λ"
      | toString AX = "<>"
      | toString AP = "ap"

    open ParserCombinators CharParser
    infix 2 return
    infixr 1 ||

    fun parseOperator () =
      string "λ" return LAM
        || string "<>" return AX
        || string "ap" return AP
  end

  structure Syn = AbtUtil(Abt(structure Operator = O and Variable = Variable()))
  structure ParseSyn =
  struct
    structure P = ParseAbt(structure Syntax = Syn and Operator = O)
    open P

    local
      open ParserCombinators CharParser ParserKit
      infix 2 wth suchthat return guard when
      infixr 1 ||
      infixr 4 << >>
      infix 2 -- ##
      infixr 3 &&
      infix $$ \\
    in
      fun parseRaw w st = prettyLambda w st || P.extensibleParseAbt w (parseAbt w) st
      and prettyLambda w st =
        string "[" >> parseBoundVariable st << string "]" << spaces -- (fn (v, st') =>
          parseAbt w st' wth (fn E =>
            LAM $$ #[v \\ E]))
      and parenthetical w st () = string "(" >> parseAbt w st << string ")" << spaces
      and expItem w st = (parseRaw w st || $ (parenthetical w st)) wth Atm
      and parseAbt w st = parsefixityadj (expItem w st) Left (fn (M,N) => AP $$ #[M,N])
      val parseClosed = parseAbt () (initialState []) << eos
    end
  end

  fun printRes pr = print (Sum.sumR (fn b => Syn.toString b ^ "\n") pr)
  fun doit s = printRes (CharParser.parseString ParseSyn.parseClosed s)

  val _ =
    (doit "λ(x.λ(x. x <>))";
     doit "([x] x x) x x x";
     doit "x (x (x x))";
     doit "[x] [y] x y")
end
