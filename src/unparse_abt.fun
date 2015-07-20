functor UnparseAbt
  (structure Abt : ABT
   structure Unparse : UNPARSE) : UNPARSE_ABT =
struct
  structure AbtUtil = AbtUtil(Abt) and Unparse = Unparse
  open AbtUtil Unparse
  structure V = Variable

  infix 5 $ $$ \ \\

  fun extensibleUnparseAbt inner e =
    case out e of
        ` v => atom (V.toString v)
      | v \ e =>
        let
          val vstr =
            if hasFree (e, v) then
              Variable.toString v
            else
              "_"
        in
          atom (vstr ^ "." ^ parens (done (inner e)))
        end
      | p $ es =>
          atom (Operator.toString p ^
                 (if Vector.length es = 0 then ""
                   else VectorPretty.toString (parens o done o inner) es))

  fun unparseAbt t =
    extensibleUnparseAbt unparseAbt t

  val toString = parens o done o unparseAbt
end
