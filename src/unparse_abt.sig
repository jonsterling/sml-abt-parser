signature UNPARSE_ABT =
sig
  include ABT_UTIL
  structure Unparse : UNPARSE

  val extensibleUnparseAbt : (t -> string Unparse.part) -> t -> string Unparse.part
  val unparseAbt : t -> string Unparse.part
end
