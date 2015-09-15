functor ParseAbt
  (structure Syntax : ABT_UTIL
   structure Operator : PARSE_OPERATOR
   sharing type Syntax.Operator.t = Operator.t) : PARSE_ABT =
struct
  structure ParseOperator = Operator
  type world = Operator.world

  val force = ParserCombinators.$
  open ParserCombinators CharParser Syntax

  infix 5 $$ \\

  infixr 4 << >>
  infixr 3 &&
  infix 2 -- ##
  infix 2 wth suchthat return guard when
  infixr 1 || <|> ??

  structure LangDef :> LANGUAGE_DEF =
  struct
    type scanner = char CharParser.charParser
    val commentStart = NONE
    val commentEnd = NONE
    val commentLine = NONE
    val nestedComments = true

    val identLetter =
      CharParser.letter
        || CharParser.oneOf (String.explode "_'ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσΤτΥυΦφΧχΨψΩω")
        || digit
    val identStart = identLetter
    val opStart = fail "Operators not supported" : scanner
    val opLetter = opStart
    val reservedNames = []
    val reservedOpNames = []
    val caseSensitive = true
  end

  structure TP = TokenParser (LangDef)
  open TP

  structure SymbolTable =
  struct
    type table = Variable.t StringListDict.dict
    type t = {bound: table, free: table ref}
    fun bind {bound,free} (n, v) =
      {bound = StringListDict.insert bound n v,
       free = free}

    fun named {bound,free} n =
      StringListDict.lookup bound n
      handle _ => StringListDict.lookup (!free) n
      handle _ =>
        let
          val v = Variable.named n
        in
          free := StringListDict.insert (!free) n v;
          v
        end

    fun dictFromFvs fvs =
      let
        fun go [] R = R
          | go (x::xs) R = go xs (StringListDict.insert R (Variable.toString x) x)
      in
        go fvs StringListDict.empty
      end

    fun empty fvs : t =
      {bound = StringListDict.empty,
       free = ref (dictFromFvs fvs)}
  end

  type state = SymbolTable.t
  type parser_kit =
    {parseFreeVariable : state -> Variable.t CharParser.charParser,
     parseBoundVariable : state -> (Variable.t * state) CharParser.charParser
    }

  fun parens p = (symbol "(" >> spaces) >> p << (spaces >> symbol ")")

  type input = Variable.t list * ParseOperator.world

  structure ParserKit =
  struct
    fun parseFreeVariable sigma =
      identifier wth (fn n =>
        if n = "_" then
          Variable.named n
        else
          SymbolTable.named sigma n)

    fun parseBoundVariable sigma =
      identifier wth (fn n =>
        let
          val v = Variable.named n
        in
          (v, SymbolTable.bind sigma (n, v))
        end)
  end

  fun initialState fvs = SymbolTable.empty fvs

  fun extensibleParseAbt w ext =
    let
      open ParserKit
      fun abt sigma () =
        (force (abs sigma)
        || force (app sigma)
        || parseFreeVariable sigma wth ``
        ) ?? "abt"
      and app sigma () =
        ParseOperator.parseOperator w
          && opt (parens (force (args sigma)))
          wth (fn (O, ES) => O $$ getOpt (ES, #[])) ?? "app"
      and abs sigma () =
        (parseBoundVariable sigma << spaces << symbol "." << spaces -- (fn (v, tau) =>
          ext tau wth (fn E => v \\ E))) ?? "abs"
      and args sigma () =
        separate (ext sigma) (symbol ";") wth Vector.fromList ?? "args"
    in
      force o abt
    end

  fun parseAbt w st = extensibleParseAbt w (parseAbt w) st
end
