signature PARSE_ABT =
sig
  include ABT_UTIL

  structure ParseOperator : PARSE_OPERATOR
  sharing type Operator.t = ParseOperator.t

  type state
  val initialState : Variable.t list (* free variables *) -> state

  structure ParserKit :
  sig
    val parseFreeVariable : state -> Variable.t CharParser.charParser
    val parseBoundVariable : state -> (Variable.t * state) CharParser.charParser
  end

  val parseAbt : ParseOperator.world -> state -> t CharParser.charParser

  (* Pass your own recursive step *)
  val extensibleParseAbt : ParseOperator.world -> (state -> t CharParser.charParser) -> state -> t CharParser.charParser
end

