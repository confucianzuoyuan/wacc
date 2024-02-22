[@@@coverage off]

type t =
  | ConstChar of Cnums.Int8.t
  | ConstUChar of Cnums.UInt8.t
  | ConstInt of Int32.t
  | ConstLong of Int64.t
  | ConstUInt of Cnums.UInt32.t
  | ConstULong of Cnums.UInt64.t
  | ConstDouble of Cnums.Float.t
[@@deriving eq, ord]

(* 用来 debug 的打印函数 *)
let show = function
  | ConstChar c -> Cnums.Int8.to_string c
  | ConstUChar c -> Cnums.UInt8.to_string c
  | ConstInt i -> Int32.to_string i
  | ConstLong l -> Int64.to_string l ^ "L"
  | ConstUInt u -> Cnums.UInt32.to_string u ^ "U"
  | ConstULong ul -> Cnums.UInt64.to_string ul ^ "UL"
  | ConstDouble d -> Cnums.Float.to_string d

let pp fmt constant = Format.pp_print_string fmt (show constant)

[@@@coverage on]

let int_zero = ConstInt Int32.zero
let int_one = ConstInt Int32.one

let type_of_const = function
  | ConstChar _ -> Types.Char
  | ConstUChar _ -> Types.UChar
  | ConstInt _ -> Types.Int
  | ConstLong _ -> Types.Long
  | ConstUInt _ -> Types.UInt
  | ConstULong _ -> Types.ULong
  | ConstDouble _ -> Types.Double
